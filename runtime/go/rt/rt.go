package rt

import (
	"fmt"
	"runtime"
	"strconv"
	"strings"
)

type Value = any

type IO func() Value

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
