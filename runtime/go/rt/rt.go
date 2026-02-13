package rt

import (
	"fmt"
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

func Builtin(name string) Value {
	switch name {
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
	default:
		panic("rt.Builtin: unknown primitive: " + name)
	}
}
