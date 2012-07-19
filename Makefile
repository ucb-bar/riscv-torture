# Convenience Makefile

C_SIM := ../riscv-rocket/emulator/emulator
R_SIM := ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv
DIR :=  output/failedtests
ERRORS := 5
MINUTES := 1
EMAIL := your@email.address
CONFIG := config
INSTCNT := 1
INSTDIR := ..
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-C$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))

.phony: gen ctest ctestd rtest rtestd itest iretest cretest cretestd rretest \
rretestd cnight rnight crnight cnighte rnighte crnighte cschaden rschaden    \
crschaden

gen:
	sbt 'generator/run'

itest:
	sbt 'testrun/run'

ctest:
	sbt 'testrun/run -c $(C_SIM)'

ctestd:
	sbt 'testrun/run -d true -c $(C_SIM)'

rtest:
	sbt 'testrun/run -r $(R_SIM)'

rtestd:
	sbt 'testrun/run -d true -r $(R_SIM)'

iretest:
	sbt 'testrun/run -a output/test.S'

cretest:
	sbt 'testrun/run -c $(C_SIM) -a output/test.S'

cretestd:
	sbt 'testrun/run -d true -c $(C_SIM) -a output/test.S'

rretest:
	sbt 'testrun/run -r $(R_SIM) -a output/test.S'

rretestd:
	sbt 'testrun/run -d true -r $(R_SIM) -a output/test.S'

cnight:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES)'

cnighte:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

rnight:
	sbt 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

rnighte:
	sbt 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

crnight:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

crnighte:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

cschaden:
	sbt 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT)'

rschaden:
	sbt 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT)'

crschaden:
	sbt 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT)' 
