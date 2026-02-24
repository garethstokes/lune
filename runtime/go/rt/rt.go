package rt

import (
	"bufio"
	"context"
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"math"
	"net"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"
	"unicode/utf8"
)

type Value = any

type IO func() Value

type Atomic func(*Tx) Value

type Record map[string]Value

type Con struct {
	Name string
	Args []Value
}

type Func func(Value) Value

type Prim struct {
	Arity int
	Fn    func([]Value) Value
	Args  []Value
}

type Socket struct {
	ln net.Listener
}

type Connection struct {
	conn net.Conn
}

type TlsConn struct {
	conn *tls.Conn
}

type jsonKind int

const (
	jsonNullKind jsonKind = iota
	jsonBoolKind
	jsonIntKind
	jsonFloatKind
	jsonStringKind
	jsonArrayKind
	jsonObjectKind
)

type jsonValue struct {
	kind jsonKind

	b bool
	i int64
	f float64
	s string

	arr []*jsonValue
	obj []jsonField
}

type jsonField struct {
	key string
	val *jsonValue
}

var jsonNull = &jsonValue{kind: jsonNullKind}

type TVar struct {
	val Value
}

type Tx struct {
	writes map[*TVar]Value
}

type stmRetry struct{}

func (tx *Tx) Retry() {
	panic(stmRetry{})
}

func (tx *Tx) fork() *Tx {
	writes := make(map[*TVar]Value, len(tx.writes))
	for tv, v := range tx.writes {
		writes[tv] = v
	}
	return &Tx{writes: writes}
}

func (tx *Tx) Read(tv *TVar) Value {
	if v, ok := tx.writes[tv]; ok {
		return v
	}
	return tv.val
}

func (tx *Tx) Write(tv *TVar, v Value) {
	tx.writes[tv] = v
}

func (tx *Tx) NewTVar(v Value) *TVar {
	tv := &TVar{val: v}
	tx.writes[tv] = v
	return tv
}

func (tx *Tx) commit() {
	for tv, v := range tx.writes {
		tv.val = v
	}
}

var stmMu sync.Mutex
var stmCond = sync.NewCond(&stmMu)

var stdinMu sync.Mutex
var stdinReader = bufio.NewReader(os.Stdin)

func readStdinLine(ctx string) string {
	stdinMu.Lock()
	defer stdinMu.Unlock()

	line, err := stdinReader.ReadString('\n')
	if err != nil && err != io.EOF {
		panic(ctx + ": " + err.Error())
	}
	line = strings.TrimSuffix(line, "\n")
	line = strings.TrimSuffix(line, "\r")
	return line
}

func runAtomic(tx *Tx, ma Atomic) (val Value, retried bool) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(stmRetry); ok {
				val = nil
				retried = true
				return
			}
			panic(r)
		}
	}()
	val = ma(tx)
	return val, false
}

func atomically(ma Atomic) Value {
	for {
		stmMu.Lock()
		tx := &Tx{writes: make(map[*TVar]Value)}
		val, retried := runAtomic(tx, ma)
		if retried {
			stmCond.Wait()
			stmMu.Unlock()
			continue
		}
		tx.commit()
		stmCond.Broadcast()
		stmMu.Unlock()
		return val
	}
}

func orElse(tx *Tx, a, b Atomic) Value {
	leftTx := tx.fork()
	v, retried := runAtomic(leftTx, a)
	if !retried {
		tx.writes = leftTx.writes
		return v
	}

	rightTx := tx.fork()
	v2, retried2 := runAtomic(rightTx, b)
	if !retried2 {
		tx.writes = rightTx.writes
		return v2
	}

	tx.Retry()
	return nil // unreachable
}

type Task struct {
	done chan struct{}
	res  Value
}

type streamBuf struct {
	mu   sync.Mutex
	cond *sync.Cond
	buf  []byte
	eof  bool
	err  error
}

func newStreamBuf() *streamBuf {
	sb := &streamBuf{}
	sb.cond = sync.NewCond(&sb.mu)
	return sb
}

func (sb *streamBuf) appendBytes(data []byte) {
	if len(data) == 0 {
		return
	}
	sb.mu.Lock()
	sb.buf = append(sb.buf, data...)
	sb.cond.Broadcast()
	sb.mu.Unlock()
}

func (sb *streamBuf) closeWithError(err error) {
	sb.mu.Lock()
	if sb.eof {
		sb.mu.Unlock()
		return
	}
	sb.eof = true
	sb.err = err
	sb.cond.Broadcast()
	sb.mu.Unlock()
}

func (sb *streamBuf) closeEOF() {
	sb.closeWithError(nil)
}

func (sb *streamBuf) readN(n int) (chunk []byte, eof bool, err error) {
	sb.mu.Lock()
	defer sb.mu.Unlock()

	for len(sb.buf) == 0 && !sb.eof {
		sb.cond.Wait()
	}

	if len(sb.buf) > 0 {
		if n > len(sb.buf) {
			n = len(sb.buf)
		}
		out := make([]byte, n)
		copy(out, sb.buf[:n])
		sb.buf = sb.buf[n:]
		return out, false, nil
	}

	if sb.err != nil {
		return nil, true, sb.err
	}
	return nil, true, nil
}

type exitInfo struct {
	mu   sync.Mutex
	done bool
	code int
	err  error
	ch   chan struct{}
}

type Process struct {
	cmd *exec.Cmd

	stdinMu sync.Mutex
	stdin   io.WriteCloser

	stdout *streamBuf
	stderr *streamBuf

	exit exitInfo
}

func NewPrim(arity int, fn func([]Value) Value) Prim {
	return Prim{Arity: arity, Fn: fn, Args: nil}
}

func Apply(f Value, x Value) Value {
	switch fn := f.(type) {
	case Func:
		return fn(x)
	case Con:
		args := append(append([]Value{}, fn.Args...), x)
		return Con{Name: fn.Name, Args: args}
	case Prim:
		args := append(append([]Value{}, fn.Args...), x)
		if len(args) == fn.Arity {
			return fn.Fn(args)
		}
		return Prim{Arity: fn.Arity, Fn: fn.Fn, Args: args}
	default:
		panic(fmt.Sprintf("rt.Apply: not a function: %T", f))
	}
}

func MatchCon(v Value, name string, arity int) ([]Value, bool) {
	con, ok := v.(Con)
	if !ok {
		return nil, false
	}
	if con.Name != name {
		return nil, false
	}
	if len(con.Args) != arity {
		return nil, false
	}
	return con.Args, true
}

func Select(base Value, field string) Value {
	rec, ok := base.(Record)
	if !ok {
		panic(fmt.Sprintf("rt.Select: not a record: %T", base))
	}
	v, ok := rec[field]
	if !ok {
		panic("rt.Select: missing field: " + field)
	}
	return v
}

func MustIO(v Value) IO {
	io, ok := v.(IO)
	if !ok {
		panic(fmt.Sprintf("rt.MustIO: expected IO, got %T", v))
	}
	return io
}

func AsIO(v Value) IO {
	if io, ok := v.(IO); ok {
		return io
	}
	if args, ok := MatchCon(v, "Lune.Prelude.Task", 1); ok {
		return MustIO(args[0])
	}
	panic(fmt.Sprintf("rt.AsIO: expected IO or Task, got %T", v))
}

func expectAtomic(v Value, ctx string) Atomic {
	a, ok := v.(Atomic)
	if !ok {
		panic(fmt.Sprintf("%s: expected Atomic, got %T", ctx, v))
	}
	return a
}

func expectTVar(v Value, ctx string) *TVar {
	tv, ok := v.(*TVar)
	if !ok {
		panic(fmt.Sprintf("%s: expected Shared/TVar, got %T", ctx, v))
	}
	return tv
}

func RunIO(io IO) Value {
	return io()
}

func boolCon(b bool) Value {
	if b {
		return Con{Name: "Lune.Prelude.True", Args: nil}
	}
	return Con{Name: "Lune.Prelude.False", Args: nil}
}

func expectBool(v Value) bool {
	con, ok := v.(Con)
	if !ok {
		panic(fmt.Sprintf("expected Bool, got %T", v))
	}
	switch con.Name {
	case "Lune.Prelude.True":
		if len(con.Args) != 0 {
			panic("expected Bool True (arity 0)")
		}
		return true
	case "Lune.Prelude.False":
		if len(con.Args) != 0 {
			panic("expected Bool False (arity 0)")
		}
		return false
	default:
		panic("expected Bool, got constructor: " + con.Name)
	}
}

func expectFloat64(v Value, ctx string) float64 {
	f, ok := v.(float64)
	if !ok {
		panic(fmt.Sprintf("%s: expected Float, got %T", ctx, v))
	}
	return f
}

func expectInt64(v Value, ctx string) int64 {
	n, ok := v.(int64)
	if !ok {
		panic(fmt.Sprintf("%s: expected Int, got %T", ctx, v))
	}
	return n
}

func expectRune(v Value, ctx string) rune {
	r, ok := v.(rune)
	if !ok {
		panic(fmt.Sprintf("%s: expected Char, got %T", ctx, v))
	}
	return r
}

func expectString(v Value, ctx string) string {
	s, ok := v.(string)
	if !ok {
		panic(fmt.Sprintf("%s: expected String, got %T", ctx, v))
	}
	return s
}

func expectBytes(v Value, ctx string) []byte {
	bs, ok := v.([]byte)
	if !ok {
		panic(fmt.Sprintf("%s: expected Bytes, got %T", ctx, v))
	}
	return bs
}

func expectProcess(v Value, ctx string) *Process {
	p, ok := v.(*Process)
	if !ok {
		panic(fmt.Sprintf("%s: expected Process, got %T", ctx, v))
	}
	return p
}

func expectSocket(v Value, ctx string) *Socket {
	sock, ok := v.(*Socket)
	if !ok {
		panic(fmt.Sprintf("%s: expected Socket, got %T", ctx, v))
	}
	return sock
}

func expectConn(v Value, ctx string) *Connection {
	conn, ok := v.(*Connection)
	if !ok {
		panic(fmt.Sprintf("%s: expected Connection, got %T", ctx, v))
	}
	return conn
}

func expectTlsConn(v Value, ctx string) *TlsConn {
	conn, ok := v.(*TlsConn)
	if !ok {
		panic(fmt.Sprintf("%s: expected TlsConn, got %T", ctx, v))
	}
	return conn
}

func resultOk(v Value) Value {
	return Con{Name: "Lune.Prelude.Ok", Args: []Value{v}}
}

func resultErr(msg string) Value {
	return Con{Name: "Lune.Prelude.Err", Args: []Value{msg}}
}

func resultErrValue(v Value) Value {
	return Con{Name: "Lune.Prelude.Err", Args: []Value{v}}
}

func processCon(name string) string {
	return "Lune.Process." + name
}

func exitStatusValue(code int) Value {
	return Con{Name: processCon("Exited"), Args: []Value{int64(code)}}
}

func procErrNotFound(program string) Value {
	return Con{Name: processCon("NotFound"), Args: []Value{program}}
}

func procErrPermissionDenied(program string) Value {
	return Con{Name: processCon("PermissionDenied"), Args: []Value{program}}
}

func procErrSpawnFailed(msg string) Value {
	return Con{Name: processCon("SpawnFailed"), Args: []Value{msg}}
}

func procErrIoError(msg string) Value {
	return Con{Name: processCon("IoError"), Args: []Value{msg}}
}

func decodeUtf8Lenient(bs []byte) string {
	var b strings.Builder
	b.Grow(len(bs))
	for len(bs) > 0 {
		r, size := utf8.DecodeRune(bs)
		if r == utf8.RuneError && size == 1 {
			b.WriteRune(utf8.RuneError)
			bs = bs[1:]
			continue
		}
		b.WriteRune(r)
		bs = bs[size:]
	}
	return b.String()
}

func showFloat(f float64) string {
	if math.IsNaN(f) {
		return "NaN"
	}
	if math.IsInf(f, 1) {
		return "Infinity"
	}
	if math.IsInf(f, -1) {
		return "-Infinity"
	}
	if f == math.Trunc(f) {
		return strconv.FormatFloat(f, 'f', 1, 64)
	}
	return strconv.FormatFloat(f, 'g', -1, 64)
}

func parseDecimalInt64(s string) (int64, bool) {
	const maxI64 = int64(9223372036854775807)
	if s == "" {
		return 0, false
	}
	var n int64 = 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c < '0' || c > '9' {
			return 0, false
		}
		d := int64(c - '0')
		if n > (maxI64-d)/10 {
			return 0, false
		}
		n = n*10 + d
	}
	return n, true
}

func writeAll(w io.Writer, bs []byte) error {
	for len(bs) > 0 {
		n, err := w.Write(bs)
		if err != nil {
			return err
		}
		bs = bs[n:]
	}
	return nil
}

func haskellDivMod(a, b int64) (int64, int64) {
	if b == 0 {
		panic("division by zero")
	}
	q := a / b
	r := a % b
	if r != 0 && ((r < 0) != (b < 0)) {
		q -= 1
		r += b
	}
	return q, r
}

func Builtin(name string) Value {
	switch name {
	case "prim_addFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_addFloat")
			b := expectFloat64(args[1], "prim_addFloat")
			return a + b
		})
	case "prim_subFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_subFloat")
			b := expectFloat64(args[1], "prim_subFloat")
			return a - b
		})
	case "prim_mulFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_mulFloat")
			b := expectFloat64(args[1], "prim_mulFloat")
			return a * b
		})
	case "prim_divFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_divFloat")
			b := expectFloat64(args[1], "prim_divFloat")
			return a / b
		})
	case "prim_eqFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_eqFloat")
			b := expectFloat64(args[1], "prim_eqFloat")
			return boolCon(a == b)
		})
	case "prim_gtFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_gtFloat")
			b := expectFloat64(args[1], "prim_gtFloat")
			return boolCon(a > b)
		})
	case "prim_ltFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_ltFloat")
			b := expectFloat64(args[1], "prim_ltFloat")
			return boolCon(a < b)
		})
	case "prim_geFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_geFloat")
			b := expectFloat64(args[1], "prim_geFloat")
			return boolCon(a >= b)
		})
	case "prim_leFloat":
		return NewPrim(2, func(args []Value) Value {
			a := expectFloat64(args[0], "prim_leFloat")
			b := expectFloat64(args[1], "prim_leFloat")
			return boolCon(a <= b)
		})
	case "prim_fromIntFloat":
		return NewPrim(1, func(args []Value) Value {
			n := expectInt64(args[0], "prim_fromIntFloat")
			return float64(n)
		})
	case "prim_truncateFloat":
		return NewPrim(1, func(args []Value) Value {
			f := expectFloat64(args[0], "prim_truncateFloat")
			return int64(f)
		})
	case "prim_showFloat":
		return NewPrim(1, func(args []Value) Value {
			f := expectFloat64(args[0], "prim_showFloat")
			return showFloat(f)
		})
	case "prim_readFile":
		return NewPrim(1, func(args []Value) Value {
			path, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_readFile: expected String, got %T", args[0]))
			}
			return IO(func() Value {
				bs, err := os.ReadFile(path)
				if err != nil {
					return Con{Name: "Lune.Prelude.Err", Args: []Value{err.Error()}}
				}
				return Con{Name: "Lune.Prelude.Ok", Args: []Value{string(bs)}}
			})
		})
	case "prim_writeFile":
		return NewPrim(2, func(args []Value) Value {
			path, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_writeFile: expected String, got %T", args[0]))
			}
			contents, ok := args[1].(string)
			if !ok {
				panic(fmt.Sprintf("prim_writeFile: expected String, got %T", args[1]))
			}
			return IO(func() Value {
				err := os.WriteFile(path, []byte(contents), 0o644)
				if err != nil {
					return Con{Name: "Lune.Prelude.Err", Args: []Value{err.Error()}}
				}
				return Con{Name: "Lune.Prelude.Ok", Args: []Value{Con{Name: "Lune.Prelude.Unit", Args: nil}}}
			})
		})
	case "prim_sleepMs":
		return NewPrim(1, func(args []Value) Value {
			ms := expectInt64(args[0], "prim_sleepMs")
			return IO(func() Value {
				if ms > 0 {
					time.Sleep(time.Duration(ms) * time.Millisecond)
				}
				return Con{Name: "Lune.Prelude.Unit", Args: nil}
			})
		})
	case "prim_timeNowMicros":
		return IO(func() Value {
			return time.Now().UnixMicro()
		})
	case "prim_tcpListen":
		return NewPrim(1, func(args []Value) Value {
			port := expectInt64(args[0], "prim_tcpListen")
			return IO(func() Value {
				if port < 0 || port > 65535 {
					return resultErr("invalid port: " + strconv.FormatInt(port, 10))
				}

				lc := net.ListenConfig{
					Control: func(network, address string, c syscall.RawConn) error {
						var sockErr error
						if err := c.Control(func(fd uintptr) {
							sockErr = syscall.SetsockoptInt(int(fd), syscall.SOL_SOCKET, syscall.SO_REUSEADDR, 1)
						}); err != nil {
							return err
						}
						return sockErr
					},
				}

				ln, err := lc.Listen(context.Background(), "tcp", ":"+strconv.FormatInt(port, 10))
				if err != nil {
					return resultErr(err.Error())
				}
				return resultOk(&Socket{ln: ln})
			})
		})
	case "prim_tcpAccept":
		return NewPrim(1, func(args []Value) Value {
			sock := expectSocket(args[0], "prim_tcpAccept")
			return IO(func() Value {
				conn, err := sock.ln.Accept()
				if err != nil {
					return resultErr(err.Error())
				}
				return resultOk(&Connection{conn: conn})
			})
		})
	case "prim_tcpConnect":
		return NewPrim(2, func(args []Value) Value {
			host, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_tcpConnect: expected String, got %T", args[0]))
			}
			port := expectInt64(args[1], "prim_tcpConnect")
			return IO(func() Value {
				if port < 0 || port > 65535 {
					return resultErr("invalid port: " + strconv.FormatInt(port, 10))
				}
				conn, err := net.Dial("tcp", net.JoinHostPort(host, strconv.FormatInt(port, 10)))
				if err != nil {
					return resultErr(err.Error())
				}
				return resultOk(&Connection{conn: conn})
			})
		})
	case "prim_connRecv":
		return NewPrim(1, func(args []Value) Value {
			conn := expectConn(args[0], "prim_connRecv")
			return IO(func() Value {
				buf := make([]byte, 4096)
				n, err := conn.conn.Read(buf)
				if err != nil && err != io.EOF {
					return resultErr(err.Error())
				}
				return resultOk(decodeUtf8Lenient(buf[:n]))
			})
		})
	case "prim_connSend":
		return NewPrim(2, func(args []Value) Value {
			conn := expectConn(args[0], "prim_connSend")
			msg, ok := args[1].(string)
			if !ok {
				panic(fmt.Sprintf("prim_connSend: expected String, got %T", args[1]))
			}
			return IO(func() Value {
				if err := writeAll(conn.conn, []byte(msg)); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_connSendBytes":
		return NewPrim(2, func(args []Value) Value {
			conn := expectConn(args[0], "prim_connSendBytes")
			bs := expectBytes(args[1], "prim_connSendBytes")
			return IO(func() Value {
				if err := writeAll(conn.conn, bs); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_connRecvBytes":
		return NewPrim(2, func(args []Value) Value {
			conn := expectConn(args[0], "prim_connRecvBytes")
			maxLen := expectInt64(args[1], "prim_connRecvBytes")
			return IO(func() Value {
				if maxLen < 0 {
					return resultErr("negative length: " + strconv.FormatInt(maxLen, 10))
				}
				maxInt := int64(int(^uint(0) >> 1))
				if maxLen > maxInt {
					return resultErr("length too large: " + strconv.FormatInt(maxLen, 10))
				}
				buf := make([]byte, int(maxLen))
				n, err := conn.conn.Read(buf)
				if err != nil && err != io.EOF {
					return resultErr(err.Error())
				}
				return resultOk(buf[:n])
			})
		})
	case "prim_connClose":
		return NewPrim(1, func(args []Value) Value {
			conn := expectConn(args[0], "prim_connClose")
			return IO(func() Value {
				if err := conn.conn.Close(); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_socketClose":
		return NewPrim(1, func(args []Value) Value {
			sock := expectSocket(args[0], "prim_socketClose")
			return IO(func() Value {
				if err := sock.ln.Close(); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_tlsConnect":
		return NewPrim(2, func(args []Value) Value {
			host, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_tlsConnect: expected String, got %T", args[0]))
			}
			port := expectInt64(args[1], "prim_tlsConnect")
			return IO(func() Value {
				if port < 0 || port > 65535 {
					return resultErr("invalid port: " + strconv.FormatInt(port, 10))
				}
				conn, err := tls.Dial("tcp", net.JoinHostPort(host, strconv.FormatInt(port, 10)), &tls.Config{ServerName: host})
				if err != nil {
					return resultErr(err.Error())
				}
				return resultOk(&TlsConn{conn: conn})
			})
		})
	case "prim_tlsSendBytes":
		return NewPrim(2, func(args []Value) Value {
			conn := expectTlsConn(args[0], "prim_tlsSendBytes")
			bs := expectBytes(args[1], "prim_tlsSendBytes")
			return IO(func() Value {
				if err := writeAll(conn.conn, bs); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_tlsRecvBytes":
		return NewPrim(2, func(args []Value) Value {
			conn := expectTlsConn(args[0], "prim_tlsRecvBytes")
			maxLen := expectInt64(args[1], "prim_tlsRecvBytes")
			return IO(func() Value {
				if maxLen < 0 {
					return resultErr("negative length: " + strconv.FormatInt(maxLen, 10))
				}
				maxInt := int64(int(^uint(0) >> 1))
				if maxLen > maxInt {
					return resultErr("length too large: " + strconv.FormatInt(maxLen, 10))
				}
				buf := make([]byte, int(maxLen))
				n, err := conn.conn.Read(buf)
				if err != nil && err != io.EOF {
					return resultErr(err.Error())
				}
				return resultOk(buf[:n])
			})
		})
	case "prim_tlsClose":
		return NewPrim(1, func(args []Value) Value {
			conn := expectTlsConn(args[0], "prim_tlsClose")
			return IO(func() Value {
				if err := conn.conn.Close(); err != nil {
					return resultErr(err.Error())
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_jsonParse":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_jsonParse: expected String, got %T", args[0]))
			}
			j, err := parseJson(s)
			if err != nil {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{err.Error()}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{j}}
		})
	case "prim_jsonStringify":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonStringify")
			return stringifyJson(j)
		})
	case "prim_jsonNull":
		return jsonNull
	case "prim_jsonBool":
		return NewPrim(1, func(args []Value) Value {
			return &jsonValue{kind: jsonBoolKind, b: expectBool(args[0])}
		})
	case "prim_jsonInt":
		return NewPrim(1, func(args []Value) Value {
			return &jsonValue{kind: jsonIntKind, i: expectInt64(args[0], "prim_jsonInt")}
		})
	case "prim_jsonFloat":
		return NewPrim(1, func(args []Value) Value {
			return &jsonValue{kind: jsonFloatKind, f: expectFloat64(args[0], "prim_jsonFloat")}
		})
	case "prim_jsonString":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_jsonString: expected String, got %T", args[0]))
			}
			return &jsonValue{kind: jsonStringKind, s: s}
		})
	case "prim_jsonArray":
		return NewPrim(1, func(args []Value) Value {
			items := sliceFromList(args[0], "prim_jsonArray")
			arr := make([]*jsonValue, 0, len(items))
			for _, item := range items {
				arr = append(arr, expectJson(item, "prim_jsonArray"))
			}
			return &jsonValue{kind: jsonArrayKind, arr: arr}
		})
	case "prim_jsonObject":
		return NewPrim(1, func(args []Value) Value {
			items := sliceFromList(args[0], "prim_jsonObject")
			obj := make([]jsonField, 0, len(items))
			for _, item := range items {
				rec, ok := item.(Record)
				if !ok {
					panic(fmt.Sprintf("prim_jsonObject: expected { key : String, value : Json }, got %T", item))
				}
				keyAny, ok := rec["key"]
				if !ok {
					panic("prim_jsonObject: missing field 'key'")
				}
				key, ok := keyAny.(string)
				if !ok {
					panic(fmt.Sprintf("prim_jsonObject: expected field 'key' String, got %T", keyAny))
				}

				valAny, ok := rec["value"]
				if !ok {
					panic("prim_jsonObject: missing field 'value'")
				}
				val := expectJson(valAny, "prim_jsonObject")
				obj = append(obj, jsonField{key: key, val: val})
			}
			return &jsonValue{kind: jsonObjectKind, obj: obj}
		})
	case "prim_jsonType":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonType")
			switch j.kind {
			case jsonNullKind:
				return "null"
			case jsonBoolKind:
				return "bool"
			case jsonIntKind:
				return "int"
			case jsonFloatKind:
				return "float"
			case jsonStringKind:
				return "string"
			case jsonArrayKind:
				return "array"
			case jsonObjectKind:
				return "object"
			default:
				panic("prim_jsonType: unknown json kind")
			}
		})
	case "prim_jsonGetField":
		return NewPrim(2, func(args []Value) Value {
			field, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_jsonGetField: expected String, got %T", args[0]))
			}
			j := expectJson(args[1], "prim_jsonGetField")
			if j.kind != jsonObjectKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected object"}}
			}
			for i := len(j.obj) - 1; i >= 0; i-- {
				if j.obj[i].key == field {
					return Con{Name: "Lune.Prelude.Ok", Args: []Value{j.obj[i].val}}
				}
			}
			return Con{Name: "Lune.Prelude.Err", Args: []Value{"missing field: " + field}}
		})
	case "prim_jsonGetIndex":
		return NewPrim(2, func(args []Value) Value {
			idx := expectInt64(args[0], "prim_jsonGetIndex")
			j := expectJson(args[1], "prim_jsonGetIndex")
			if j.kind != jsonArrayKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected array"}}
			}
			if idx < 0 || idx >= int64(len(j.arr)) {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"index out of bounds: " + strconv.FormatInt(idx, 10)}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{j.arr[idx]}}
		})
	case "prim_jsonToArray":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonToArray")
			if j.kind != jsonArrayKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected array"}}
			}
			items := make([]Value, 0, len(j.arr))
			for _, item := range j.arr {
				items = append(items, item)
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{listFromSlice(items)}}
		})
	case "prim_jsonToBool":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonToBool")
			if j.kind != jsonBoolKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected bool"}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{boolCon(j.b)}}
		})
	case "prim_jsonToInt":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonToInt")
			if j.kind != jsonIntKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected int"}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{j.i}}
		})
	case "prim_jsonToFloat":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonToFloat")
			switch j.kind {
			case jsonFloatKind:
				return Con{Name: "Lune.Prelude.Ok", Args: []Value{j.f}}
			case jsonIntKind:
				return Con{Name: "Lune.Prelude.Ok", Args: []Value{float64(j.i)}}
			default:
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected number"}}
			}
		})
	case "prim_jsonToString":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonToString")
			if j.kind != jsonStringKind {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"expected string"}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{j.s}}
		})
	case "prim_jsonIsNull":
		return NewPrim(1, func(args []Value) Value {
			j := expectJson(args[0], "prim_jsonIsNull")
			return boolCon(j.kind == jsonNullKind)
		})
	case "prim_bytesEmpty":
		return []byte{}
	case "prim_bytesFromList":
		return NewPrim(1, func(args []Value) Value {
			var out []byte
			v := args[0]
			for {
				con, ok := v.(Con)
				if !ok {
					panic(fmt.Sprintf("prim_bytesFromList: expected List Int, got %T", v))
				}
				switch con.Name {
				case "Lune.Prelude.Nil":
					if len(con.Args) != 0 {
						panic("prim_bytesFromList: expected Nil arity 0")
					}
					return out
				case "Lune.Prelude.Cons":
					if len(con.Args) != 2 {
						panic("prim_bytesFromList: expected Cons arity 2")
					}
					n := expectInt64(con.Args[0], "prim_bytesFromList")
					out = append(out, byte(n))
					v = con.Args[1]
				default:
					panic("prim_bytesFromList: expected List Int constructor, got: " + con.Name)
				}
			}
		})
	case "prim_bytesToList":
		return NewPrim(1, func(args []Value) Value {
			bs := expectBytes(args[0], "prim_bytesToList")
			var out Value = Con{Name: "Lune.Prelude.Nil", Args: nil}
			for i := len(bs) - 1; i >= 0; i-- {
				out = Con{Name: "Lune.Prelude.Cons", Args: []Value{int64(bs[i]), out}}
			}
			return out
		})
	case "prim_bytesFromString":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_bytesFromString: expected String, got %T", args[0]))
			}
			return []byte(s)
		})
	case "prim_bytesToString":
		return NewPrim(1, func(args []Value) Value {
			bs := expectBytes(args[0], "prim_bytesToString")
			return decodeUtf8Lenient(bs)
		})
	case "prim_bytesLength":
		return NewPrim(1, func(args []Value) Value {
			bs := expectBytes(args[0], "prim_bytesLength")
			return int64(len(bs))
		})
	case "prim_bytesConcat":
		return NewPrim(2, func(args []Value) Value {
			a := expectBytes(args[0], "prim_bytesConcat")
			b := expectBytes(args[1], "prim_bytesConcat")
			out := make([]byte, 0, len(a)+len(b))
			out = append(out, a...)
			out = append(out, b...)
			return out
		})
	case "prim_bytesSlice":
		return NewPrim(3, func(args []Value) Value {
			start := expectInt64(args[0], "prim_bytesSlice")
			length := expectInt64(args[1], "prim_bytesSlice")
			bs := expectBytes(args[2], "prim_bytesSlice")

			startI := int(start)
			if startI < 0 {
				startI = 0
			}
			if startI > len(bs) {
				startI = len(bs)
			}

			lengthI := int(length)
			if lengthI < 0 {
				lengthI = 0
			}

			endI := startI + lengthI
			if endI > len(bs) {
				endI = len(bs)
			}

			out := make([]byte, endI-startI)
			copy(out, bs[startI:endI])
			return out
		})
	case "prim_bytesPackInt32BE":
		return NewPrim(1, func(args []Value) Value {
			n := expectInt64(args[0], "prim_bytesPackInt32BE")
			u := uint32(n)
			return []byte{
				byte(u >> 24),
				byte(u >> 16),
				byte(u >> 8),
				byte(u),
			}
		})
	case "prim_bytesUnpackInt32BE":
		return NewPrim(1, func(args []Value) Value {
			bs := expectBytes(args[0], "prim_bytesUnpackInt32BE")
			if len(bs) < 4 {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"bytes too short for Int32"}}
			}
			u := uint32(bs[0])<<24 | uint32(bs[1])<<16 | uint32(bs[2])<<8 | uint32(bs[3])
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{int64(u)}}
		})
	case "prim_bytesPackInt16BE":
		return NewPrim(1, func(args []Value) Value {
			n := expectInt64(args[0], "prim_bytesPackInt16BE")
			u := uint16(n)
			return []byte{
				byte(u >> 8),
				byte(u),
			}
		})
	case "prim_bytesUnpackInt16BE":
		return NewPrim(1, func(args []Value) Value {
			bs := expectBytes(args[0], "prim_bytesUnpackInt16BE")
			if len(bs) < 2 {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"bytes too short for Int16"}}
			}
			u := uint16(bs[0])<<8 | uint16(bs[1])
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{int64(u)}}
		})
	case "prim_addInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_addInt")
			b := expectInt64(args[1], "prim_addInt")
			return a + b
		})
	case "prim_subInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_subInt")
			b := expectInt64(args[1], "prim_subInt")
			return a - b
		})
	case "prim_mulInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_mulInt")
			b := expectInt64(args[1], "prim_mulInt")
			return a * b
		})
	case "prim_divInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_divInt")
			b := expectInt64(args[1], "prim_divInt")
			q, _ := haskellDivMod(a, b)
			return q
		})
	case "prim_modInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_modInt")
			b := expectInt64(args[1], "prim_modInt")
			_, r := haskellDivMod(a, b)
			return r
		})
	case "prim_eqInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_eqInt")
			b := expectInt64(args[1], "prim_eqInt")
			return boolCon(a == b)
		})
	case "prim_leInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_leInt")
			b := expectInt64(args[1], "prim_leInt")
			return boolCon(a <= b)
		})
	case "prim_geInt":
		return NewPrim(2, func(args []Value) Value {
			a := expectInt64(args[0], "prim_geInt")
			b := expectInt64(args[1], "prim_geInt")
			return boolCon(a >= b)
		})
	case "prim_and":
		return NewPrim(2, func(args []Value) Value {
			a := expectBool(args[0])
			if !a {
				return boolCon(false)
			}
			b := expectBool(args[1])
			return boolCon(a && b)
		})
	case "prim_or":
		return NewPrim(2, func(args []Value) Value {
			a := expectBool(args[0])
			b := expectBool(args[1])
			return boolCon(a || b)
		})
	case "prim_not":
		return NewPrim(1, func(args []Value) Value {
			a := expectBool(args[0])
			return boolCon(!a)
		})
	case "prim_eqString":
		return NewPrim(2, func(args []Value) Value {
			a, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_eqString: expected String, got %T", args[0]))
			}
			b, ok := args[1].(string)
			if !ok {
				panic(fmt.Sprintf("prim_eqString: expected String, got %T", args[1]))
			}
			return boolCon(a == b)
		})
	case "prim_showInt":
		return NewPrim(1, func(args []Value) Value {
			n := expectInt64(args[0], "prim_showInt")
			return strconv.FormatInt(n, 10)
		})
	case "prim_parseInt":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_parseInt: expected String, got %T", args[0]))
			}
			n, ok2 := parseDecimalInt64(s)
			if !ok2 {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"invalid int"}}
			}
			return Con{Name: "Lune.Prelude.Ok", Args: []Value{n}}
		})
	case "prim_stringToChars":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_stringToChars: expected String, got %T", args[0]))
			}
			runes := []rune(s)
			var out Value = Con{Name: "Lune.Prelude.Nil", Args: nil}
			for i := len(runes) - 1; i >= 0; i-- {
				out = Con{Name: "Lune.Prelude.Cons", Args: []Value{runes[i], out}}
			}
			return out
		})
	case "prim_charsToString":
		return NewPrim(1, func(args []Value) Value {
			v := args[0]
			var b strings.Builder
			for {
				con, ok := v.(Con)
				if !ok {
					panic(fmt.Sprintf("prim_charsToString: expected List Char, got %T", v))
				}
				switch con.Name {
				case "Lune.Prelude.Nil":
					if len(con.Args) != 0 {
						panic("prim_charsToString: expected Nil arity 0")
					}
					return b.String()
				case "Lune.Prelude.Cons":
					if len(con.Args) != 2 {
						panic("prim_charsToString: expected Cons arity 2")
					}
					r := expectRune(con.Args[0], "prim_charsToString")
					b.WriteRune(r)
					v = con.Args[1]
				default:
					panic("prim_charsToString: expected List Char constructor, got: " + con.Name)
				}
			}
		})
	case "prim_charToInt":
		return NewPrim(1, func(args []Value) Value {
			c := expectRune(args[0], "prim_charToInt")
			return int64(c)
		})
	case "prim_intToChar":
		return NewPrim(1, func(args []Value) Value {
			n := expectInt64(args[0], "prim_intToChar")
			return rune(n)
		})
	case "prim_appendString":
		return NewPrim(2, func(args []Value) Value {
			a, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_appendString: expected String, got %T", args[0]))
			}
			b, ok := args[1].(string)
			if !ok {
				panic(fmt.Sprintf("prim_appendString: expected String, got %T", args[1]))
			}
			return a + b
		})
	case "prim_putStrLn":
		return NewPrim(1, func(args []Value) Value {
			s, ok := args[0].(string)
			if !ok {
				panic(fmt.Sprintf("prim_putStrLn: expected String, got %T", args[0]))
			}
			return IO(func() Value {
				fmt.Println(s)
				return Con{Name: "Lune.Prelude.Unit", Args: nil}
			})
		})
	case "prim_readLine":
		return IO(func() Value {
			return readStdinLine("prim_readLine")
		})
	case "prim_readInt":
		return IO(func() Value {
			s := readStdinLine("prim_readInt")
			n, ok := parseDecimalInt64(s)
			if !ok {
				panic("prim_readInt: invalid int")
			}
			return n
		})
	case "$primSTMPure":
		return NewPrim(1, func(args []Value) Value {
			x := args[0]
			return Atomic(func(*Tx) Value { return x })
		})
	case "$primSTMBind":
		return NewPrim(2, func(args []Value) Value {
			ma := expectAtomic(args[0], "$primSTMBind")
			k := args[1]
			return Atomic(func(tx *Tx) Value {
				a := ma(tx)
				mbAny := Apply(k, a)
				mb, ok := mbAny.(Atomic)
				if !ok {
					return mbAny
				}
				return mb(tx)
			})
		})
	case "prim_newTVar":
		return NewPrim(1, func(args []Value) Value {
			initial := args[0]
			return Atomic(func(tx *Tx) Value {
				return tx.NewTVar(initial)
			})
		})
	case "prim_readTVar":
		return NewPrim(1, func(args []Value) Value {
			tv := expectTVar(args[0], "prim_readTVar")
			return Atomic(func(tx *Tx) Value {
				return tx.Read(tv)
			})
		})
	case "prim_writeTVar":
		return NewPrim(2, func(args []Value) Value {
			tv := expectTVar(args[0], "prim_writeTVar")
			v := args[1]
			return Atomic(func(tx *Tx) Value {
				tx.Write(tv, v)
				return Con{Name: "Lune.Prelude.Unit", Args: nil}
			})
		})
	case "prim_retry":
		return Atomic(func(tx *Tx) Value {
			tx.Retry()
			return nil
		})
	case "prim_orElse":
		return NewPrim(2, func(args []Value) Value {
			a := expectAtomic(args[0], "prim_orElse")
			b := expectAtomic(args[1], "prim_orElse")
			return Atomic(func(tx *Tx) Value {
				return orElse(tx, a, b)
			})
		})
	case "prim_atomically":
		return NewPrim(1, func(args []Value) Value {
			ma := expectAtomic(args[0], "prim_atomically")
			return IO(func() Value {
				return atomically(ma)
			})
		})
	case "$primIOPure", "prim_ioPure":
		return NewPrim(1, func(args []Value) Value {
			x := args[0]
			return IO(func() Value { return x })
		})
	case "$primIOBind", "prim_ioBind":
		return NewPrim(2, func(args []Value) Value {
			ma := MustIO(args[0])
			k := args[1]
			return IO(func() Value {
				a := ma()
				mb := MustIO(Apply(k, a))
				return mb()
			})
		})
	case "$primIOThen", "prim_ioThen":
		return NewPrim(2, func(args []Value) Value {
			ma := MustIO(args[0])
			mb := MustIO(args[1])
			return IO(func() Value {
				_ = ma()
				return mb()
			})
		})
	case "prim_spawn":
		return NewPrim(1, func(args []Value) Value {
			ma := MustIO(args[0])
			return IO(func() Value {
				t := &Task{done: make(chan struct{})}
				go func() {
					t.res = ma()
					close(t.done)
				}()
				return t
			})
		})
	case "prim_await":
		return NewPrim(1, func(args []Value) Value {
			t, ok := args[0].(*Task)
			if !ok {
				panic(fmt.Sprintf("prim_await: expected Task, got %T", args[0]))
			}
			return IO(func() Value {
				<-t.done
				return t.res
			})
		})
	case "prim_awaitAny":
		return NewPrim(2, func(args []Value) Value {
			t1, ok := args[0].(*Task)
			if !ok {
				panic(fmt.Sprintf("prim_awaitAny: expected Task, got %T", args[0]))
			}
			t2, ok := args[1].(*Task)
			if !ok {
				panic(fmt.Sprintf("prim_awaitAny: expected Task, got %T", args[1]))
			}
			return IO(func() Value {
				select {
				case <-t1.done:
					return t1.res
				case <-t2.done:
					return t2.res
				}
			})
		})
	case "prim_process_run":
		return NewPrim(1, func(args []Value) Value {
			spec, msg, ok := cmdSpecFromValue(args[0])
			if !ok {
				return IO(func() Value { return resultErrValue(procErrSpawnFailed(msg)) })
			}

			return IO(func() Value {
				cmd := exec.Command(spec.program, spec.args...)
				if spec.cwd != nil {
					cmd.Dir = *spec.cwd
				}
				if env := mkProcessEnv(spec); env != nil {
					cmd.Env = env
				}
				cmd.Stdin = os.Stdin
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr

				err := cmd.Run()
				if err == nil {
					return resultOk(exitStatusValue(0))
				}
				if exitErr, ok := err.(*exec.ExitError); ok {
					return resultOk(exitStatusValue(exitErr.ExitCode()))
				}
				return resultErrValue(spawnErrorToProcessError(spec.program, err))
			})
		})
	case "prim_process_spawn":
		return NewPrim(1, func(args []Value) Value {
			spec, msg, ok := cmdSpecFromValue(args[0])
			if !ok {
				return IO(func() Value { return resultErrValue(procErrSpawnFailed(msg)) })
			}

			return IO(func() Value {
				cmd := exec.Command(spec.program, spec.args...)
				if spec.cwd != nil {
					cmd.Dir = *spec.cwd
				}
				if env := mkProcessEnv(spec); env != nil {
					cmd.Env = env
				}

				stdin, err := cmd.StdinPipe()
				if err != nil {
					return resultErrValue(procErrSpawnFailed(err.Error()))
				}
				stdout, err := cmd.StdoutPipe()
				if err != nil {
					_ = stdin.Close()
					return resultErrValue(procErrSpawnFailed(err.Error()))
				}
				stderr, err := cmd.StderrPipe()
				if err != nil {
					_ = stdin.Close()
					_ = stdout.Close()
					return resultErrValue(procErrSpawnFailed(err.Error()))
				}

				if err := cmd.Start(); err != nil {
					_ = stdin.Close()
					_ = stdout.Close()
					_ = stderr.Close()
					return resultErrValue(spawnErrorToProcessError(spec.program, err))
				}

				p := &Process{
					cmd:    cmd,
					stdin:  stdin,
					stdout: newStreamBuf(),
					stderr: newStreamBuf(),
					exit:   exitInfo{ch: make(chan struct{})},
				}

				go drainReader(stdout, p.stdout)
				go drainReader(stderr, p.stderr)
				go func() {
					waitErr := cmd.Wait()
					code := 0
					var ioErr error
					if waitErr != nil {
						if exitErr, ok := waitErr.(*exec.ExitError); ok {
							code = exitErr.ExitCode()
							ioErr = nil
						} else {
							code = 1
							ioErr = waitErr
						}
					}

					p.exit.mu.Lock()
					p.exit.done = true
					p.exit.code = code
					p.exit.err = ioErr
					close(p.exit.ch)
					p.exit.mu.Unlock()
				}()

				return resultOk(p)
			})
		})
	case "prim_process_wait":
		return NewPrim(1, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_wait")
			return IO(func() Value {
				p.exit.mu.Lock()
				done := p.exit.done
				ch := p.exit.ch
				p.exit.mu.Unlock()

				if !done && ch != nil {
					<-ch
				}

				p.exit.mu.Lock()
				code := p.exit.code
				waitErr := p.exit.err
				p.exit.mu.Unlock()

				if waitErr != nil {
					return resultErrValue(procErrIoError(waitErr.Error()))
				}
				return resultOk(exitStatusValue(code))
			})
		})
	case "prim_process_kill":
		return NewPrim(2, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_kill")
			sigName, ok := signalFromValue(args[1])
			if !ok {
				panic(fmt.Sprintf("prim_process_kill: expected Signal, got %T", args[1]))
			}
			return IO(func() Value {
				if p.cmd == nil || p.cmd.Process == nil {
					return resultErrValue(procErrIoError("process not started"))
				}

				var err error
				switch sigName {
				case "SigInt":
					err = p.cmd.Process.Signal(os.Interrupt)
				case "SigTerm":
					err = p.cmd.Process.Kill()
				case "SigKill":
					err = p.cmd.Process.Kill()
				default:
					err = p.cmd.Process.Kill()
				}

				if err != nil && !errors.Is(err, os.ErrProcessDone) {
					return resultErrValue(procErrIoError(err.Error()))
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_process_stdin_write":
		return NewPrim(2, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_stdin_write")
			bs := expectBytes(args[1], "prim_process_stdin_write")
			return IO(func() Value {
				p.stdinMu.Lock()
				w := p.stdin
				if w == nil {
					p.stdinMu.Unlock()
					return resultErrValue(procErrIoError("stdin is closed"))
				}
				err := writeAll(w, bs)
				p.stdinMu.Unlock()

				if err != nil {
					return resultErrValue(procErrIoError(err.Error()))
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_process_stdin_close":
		return NewPrim(1, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_stdin_close")
			return IO(func() Value {
				p.stdinMu.Lock()
				w := p.stdin
				p.stdin = nil
				p.stdinMu.Unlock()

				if w == nil {
					return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
				}
				if err := w.Close(); err != nil {
					return resultErrValue(procErrIoError(err.Error()))
				}
				return resultOk(Con{Name: "Lune.Prelude.Unit", Args: nil})
			})
		})
	case "prim_process_stdout_read":
		return NewPrim(2, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_stdout_read")
			n64 := expectInt64(args[1], "prim_process_stdout_read")
			return IO(func() Value {
				if n64 <= 0 {
					return resultOk(Con{Name: "Lune.Prelude.Just", Args: []Value{[]byte{}}})
				}
				n := int(n64)
				if n < 0 {
					n = 0
				}
				chunk, eof, err := p.stdout.readN(n)
				if err != nil {
					return resultErrValue(procErrIoError(err.Error()))
				}
				if eof {
					return resultOk(Con{Name: "Lune.Prelude.Nothing", Args: nil})
				}
				return resultOk(Con{Name: "Lune.Prelude.Just", Args: []Value{chunk}})
			})
		})
	case "prim_process_stderr_read":
		return NewPrim(2, func(args []Value) Value {
			p := expectProcess(args[0], "prim_process_stderr_read")
			n64 := expectInt64(args[1], "prim_process_stderr_read")
			return IO(func() Value {
				if n64 <= 0 {
					return resultOk(Con{Name: "Lune.Prelude.Just", Args: []Value{[]byte{}}})
				}
				n := int(n64)
				if n < 0 {
					n = 0
				}
				chunk, eof, err := p.stderr.readN(n)
				if err != nil {
					return resultErrValue(procErrIoError(err.Error()))
				}
				if eof {
					return resultOk(Con{Name: "Lune.Prelude.Nothing", Args: nil})
				}
				return resultOk(Con{Name: "Lune.Prelude.Just", Args: []Value{chunk}})
			})
		})
	case "prim_yield":
		return IO(func() Value {
			runtime.Gosched()
			return Con{Name: "Lune.Prelude.Unit", Args: nil}
		})
	default:
		panic("rt.Builtin: unknown primitive: " + name)
	}
}

func expectJson(v Value, ctx string) *jsonValue {
	j, ok := v.(*jsonValue)
	if !ok {
		panic(fmt.Sprintf("%s: expected Json, got %T", ctx, v))
	}
	return j
}

func sliceFromList(v Value, ctx string) []Value {
	var out []Value
	for {
		con, ok := v.(Con)
		if !ok {
			panic(fmt.Sprintf("%s: expected List, got %T", ctx, v))
		}
		switch con.Name {
		case "Lune.Prelude.Nil":
			if len(con.Args) != 0 {
				panic(ctx + ": expected Nil arity 0")
			}
			return out
		case "Lune.Prelude.Cons":
			if len(con.Args) != 2 {
				panic(ctx + ": expected Cons arity 2")
			}
			out = append(out, con.Args[0])
			v = con.Args[1]
		default:
			panic(ctx + ": expected List constructor, got: " + con.Name)
		}
	}
}

func listFromSlice(items []Value) Value {
	var out Value = Con{Name: "Lune.Prelude.Nil", Args: nil}
	for i := len(items) - 1; i >= 0; i-- {
		out = Con{Name: "Lune.Prelude.Cons", Args: []Value{items[i], out}}
	}
	return out
}

// =============================================================================
// Process helpers
// =============================================================================

type cmdEnvPair struct {
	key string
	val string
}

type cmdSpec struct {
	program  string
	args     []string
	cwd      *string
	env      []cmdEnvPair
	clearEnv bool
}

func cmdSpecFromValue(v Value) (cmdSpec, string, bool) {
	rec, ok := v.(Record)
	if !ok {
		return cmdSpec{}, "Cmd: expected record", false
	}

	progAny, ok := rec["program"]
	if !ok {
		return cmdSpec{}, "Cmd.program: missing", false
	}
	prog, ok := progAny.(string)
	if !ok {
		return cmdSpec{}, "Cmd.program: expected String", false
	}

	argsAny, ok := rec["args"]
	if !ok {
		return cmdSpec{}, "Cmd.args: missing", false
	}
	argVals := sliceFromList(argsAny, "Cmd.args")
	args := make([]string, 0, len(argVals))
	for _, aAny := range argVals {
		a, ok := aAny.(string)
		if !ok {
			return cmdSpec{}, "Cmd.args: expected String", false
		}
		args = append(args, a)
	}

	var cwd *string
	cwdAny, ok := rec["cwd"]
	if ok {
		if _, ok := MatchCon(cwdAny, "Lune.Prelude.Nothing", 0); ok {
			cwd = nil
		} else if justArgs, ok := MatchCon(cwdAny, "Lune.Prelude.Just", 1); ok {
			s, ok := justArgs[0].(string)
			if !ok {
				return cmdSpec{}, "Cmd.cwd: expected Maybe String", false
			}
			cwd = &s
		} else {
			return cmdSpec{}, "Cmd.cwd: expected Maybe String", false
		}
	}

	var env []cmdEnvPair
	envAny, ok := rec["env"]
	if ok {
		envVals := sliceFromList(envAny, "Cmd.env")
		env = make([]cmdEnvPair, 0, len(envVals))
		for _, item := range envVals {
			pairArgs, ok := MatchCon(item, "Lune.Prelude.Pair", 2)
			if !ok {
				return cmdSpec{}, "Cmd.env: expected List (String, String)", false
			}
			k, ok := pairArgs[0].(string)
			if !ok {
				return cmdSpec{}, "Cmd.env: expected List (String, String)", false
			}
			val, ok := pairArgs[1].(string)
			if !ok {
				return cmdSpec{}, "Cmd.env: expected List (String, String)", false
			}
			env = append(env, cmdEnvPair{key: k, val: val})
		}
	}

	clearEnv := false
	clearAny, ok := rec["clearEnv"]
	if ok {
		con, ok := clearAny.(Con)
		if !ok {
			return cmdSpec{}, "Cmd.clearEnv: expected Bool", false
		}
		switch con.Name {
		case "Lune.Prelude.True":
			clearEnv = true
		case "Lune.Prelude.False":
			clearEnv = false
		default:
			return cmdSpec{}, "Cmd.clearEnv: expected Bool", false
		}
	}

	return cmdSpec{
		program:  prog,
		args:     args,
		cwd:      cwd,
		env:      env,
		clearEnv: clearEnv,
	}, "", true
}

func mkProcessEnv(spec cmdSpec) []string {
	if spec.clearEnv {
		out := make([]string, 0, len(spec.env))
		for _, kv := range spec.env {
			out = append(out, kv.key+"="+kv.val)
		}
		return out
	}

	if len(spec.env) == 0 {
		return nil
	}

	base := os.Environ()
	merged := make(map[string]string, len(base)+len(spec.env))
	for _, kv := range base {
		if k, v, ok := strings.Cut(kv, "="); ok {
			merged[k] = v
		}
	}
	for _, kv := range spec.env {
		merged[kv.key] = kv.val
	}

	out := make([]string, 0, len(merged))
	for k, v := range merged {
		out = append(out, k+"="+v)
	}
	return out
}

func spawnErrorToProcessError(program string, err error) Value {
	if errors.Is(err, exec.ErrNotFound) || errors.Is(err, os.ErrNotExist) {
		return procErrNotFound(program)
	}
	if errors.Is(err, os.ErrPermission) {
		return procErrPermissionDenied(program)
	}
	return procErrSpawnFailed(err.Error())
}

func drainReader(rc io.ReadCloser, buf *streamBuf) {
	defer rc.Close()
	tmp := make([]byte, 4096)
	for {
		n, err := rc.Read(tmp)
		if n > 0 {
			buf.appendBytes(tmp[:n])
		}
		if err != nil {
			if err == io.EOF {
				buf.closeEOF()
			} else {
				buf.closeWithError(err)
			}
			return
		}
	}
}

func signalFromValue(v Value) (string, bool) {
	con, ok := v.(Con)
	if !ok {
		return "", false
	}
	if len(con.Args) != 0 {
		return "", false
	}
	switch con.Name {
	case "Lune.Process.SigTerm":
		return "SigTerm", true
	case "Lune.Process.SigKill":
		return "SigKill", true
	case "Lune.Process.SigInt":
		return "SigInt", true
	default:
		return "", false
	}
}

func parseJson(input string) (*jsonValue, error) {
	dec := json.NewDecoder(strings.NewReader(input))
	dec.UseNumber()
	val, err := parseJsonValue(dec)
	if err != nil {
		return nil, err
	}

	if tok, err := dec.Token(); err != nil {
		if err == io.EOF {
			return val, nil
		}
		return nil, err
	} else {
		return nil, fmt.Errorf("unexpected trailing token: %v", tok)
	}
}

func parseJsonValue(dec *json.Decoder) (*jsonValue, error) {
	tok, err := dec.Token()
	if err != nil {
		return nil, err
	}

	switch t := tok.(type) {
	case nil:
		return jsonNull, nil
	case bool:
		return &jsonValue{kind: jsonBoolKind, b: t}, nil
	case string:
		return &jsonValue{kind: jsonStringKind, s: t}, nil
	case json.Number:
		return parseJsonNumber(t.String())
	case json.Delim:
		switch t {
		case '{':
			var fields []jsonField
			for dec.More() {
				keyTok, err := dec.Token()
				if err != nil {
					return nil, err
				}
				key, ok := keyTok.(string)
				if !ok {
					return nil, fmt.Errorf("expected string object key, got %T", keyTok)
				}

				v, err := parseJsonValue(dec)
				if err != nil {
					return nil, err
				}
				fields = append(fields, jsonField{key: key, val: v})
			}
			endTok, err := dec.Token()
			if err != nil {
				return nil, err
			}
			end, ok := endTok.(json.Delim)
			if !ok || end != '}' {
				return nil, fmt.Errorf("expected '}', got %v", endTok)
			}
			return &jsonValue{kind: jsonObjectKind, obj: fields}, nil
		case '[':
			var elems []*jsonValue
			for dec.More() {
				v, err := parseJsonValue(dec)
				if err != nil {
					return nil, err
				}
				elems = append(elems, v)
			}
			endTok, err := dec.Token()
			if err != nil {
				return nil, err
			}
			end, ok := endTok.(json.Delim)
			if !ok || end != ']' {
				return nil, fmt.Errorf("expected ']', got %v", endTok)
			}
			return &jsonValue{kind: jsonArrayKind, arr: elems}, nil
		default:
			return nil, fmt.Errorf("unexpected delimiter: %q", t)
		}
	default:
		return nil, fmt.Errorf("unexpected token: %T", tok)
	}
}

func parseJsonNumber(s string) (*jsonValue, error) {
	if strings.ContainsAny(s, ".eE") {
		f, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return nil, err
		}
		return &jsonValue{kind: jsonFloatKind, f: f}, nil
	}

	i, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return nil, err
	}
	return &jsonValue{kind: jsonIntKind, i: i}, nil
}

func stringifyJson(j *jsonValue) string {
	var b strings.Builder
	writeJsonValue(&b, j)
	return b.String()
}

func writeJsonValue(b *strings.Builder, j *jsonValue) {
	switch j.kind {
	case jsonNullKind:
		b.WriteString("null")
	case jsonBoolKind:
		if j.b {
			b.WriteString("true")
		} else {
			b.WriteString("false")
		}
	case jsonIntKind:
		b.WriteString(strconv.FormatInt(j.i, 10))
	case jsonFloatKind:
		b.WriteString(showFloat(j.f))
	case jsonStringKind:
		writeJsonString(b, j.s)
	case jsonArrayKind:
		b.WriteByte('[')
		for i, item := range j.arr {
			if i > 0 {
				b.WriteByte(',')
			}
			writeJsonValue(b, item)
		}
		b.WriteByte(']')
	case jsonObjectKind:
		b.WriteByte('{')
		for i, kv := range j.obj {
			if i > 0 {
				b.WriteByte(',')
			}
			writeJsonString(b, kv.key)
			b.WriteByte(':')
			writeJsonValue(b, kv.val)
		}
		b.WriteByte('}')
	default:
		panic("stringifyJson: unknown json kind")
	}
}

func writeJsonString(b *strings.Builder, s string) {
	b.WriteByte('"')
	for _, r := range s {
		switch r {
		case '"':
			b.WriteString("\\\"")
		case '\\':
			b.WriteString("\\\\")
		case '\n':
			b.WriteString("\\n")
		case '\r':
			b.WriteString("\\r")
		case '\t':
			b.WriteString("\\t")
		case '\b':
			b.WriteString("\\b")
		case '\f':
			b.WriteString("\\f")
		default:
			if r < 0x20 {
				b.WriteString("\\u")
				b.WriteString(hex4(uint16(r)))
			} else {
				b.WriteRune(r)
			}
		}
	}
	b.WriteByte('"')
}

func hex4(n uint16) string {
	const hexdigits = "0123456789abcdef"
	return string([]byte{
		hexdigits[(n>>12)&0xf],
		hexdigits[(n>>8)&0xf],
		hexdigits[(n>>4)&0xf],
		hexdigits[n&0xf],
	})
}
