# Convenience Makefile

C_SIM = ../riscv-rocket/emulator/emulator
R_SIM = ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv

.phony: gen ctest rtest itest iretest cretest rretest

gen:
	sbt 'generator/run'

itest:
	sbt 'testrun/run'

ctest:
	sbt 'testrun/run -c ../riscv-rocket/emulator/emulator'

rtest:
	sbt 'testrun/run -r ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv'

iretest:
	sbt 'testrun/run -a output/test.S'

cretest:
	sbt 'testrun/run -c ../riscv-rocket/emulator/emulator -a output/test.S'

rretest:
	sbt 'testrun/run -r ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv -a output/test.S'
