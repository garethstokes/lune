package rt

import (
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
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

func expectBytes(v Value, ctx string) []byte {
	bs, ok := v.([]byte)
	if !ok {
		panic(fmt.Sprintf("%s: expected Bytes, got %T", ctx, v))
	}
	return bs
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
			const maxI64 = int64(9223372036854775807)
			if s == "" {
				return Con{Name: "Lune.Prelude.Err", Args: []Value{"invalid int"}}
			}
			var n int64 = 0
			for i := 0; i < len(s); i++ {
				c := s[i]
				if c < '0' || c > '9' {
					return Con{Name: "Lune.Prelude.Err", Args: []Value{"invalid int"}}
				}
				d := int64(c - '0')
				if n > (maxI64-d)/10 {
					return Con{Name: "Lune.Prelude.Err", Args: []Value{"invalid int"}}
				}
				n = n*10 + d
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
	case "$primIOBind":
		return NewPrim(2, func(args []Value) Value {
			ma := MustIO(args[0])
			k := args[1]
			return IO(func() Value {
				a := ma()
				mb := MustIO(Apply(k, a))
				return mb()
			})
		})
	case "$primIOThen":
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
	case "prim_yield":
		return IO(func() Value {
			runtime.Gosched()
			return Con{Name: "Lune.Prelude.Unit", Args: nil}
		})
	default:
		panic("rt.Builtin: unknown primitive: " + name)
	}
}
