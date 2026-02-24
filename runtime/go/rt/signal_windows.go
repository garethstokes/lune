//go:build windows

package rt

import "os"

func sendSigTerm(p *os.Process) error {
	return p.Kill()
}

func sendSigKill(p *os.Process) error {
	return p.Kill()
}

