package rt

import "fmt"

type Value = any

type IO func() Value

type Unit struct{}

func RunIO(io IO) Value {
	return io()
}

func PrimPutStrLn(s string) IO {
	return func() Value {
		fmt.Println(s)
		return Unit{}
	}
}

