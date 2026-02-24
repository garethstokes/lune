//go:build !windows

package rt

import (
	"os"
	"syscall"
)

func sendSigTerm(p *os.Process) error {
	return p.Signal(syscall.SIGTERM)
}

func sendSigKill(p *os.Process) error {
	return p.Signal(syscall.SIGKILL)
}

