# Convenience Makefile
SBT := ./sbt
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
	$(SBT) 'generator/run'

itest:
	$(SBT) 'testrun/run'

ctest:
	$(SBT) 'testrun/run -c $(C_SIM)'

ctestd:
	$(SBT) 'testrun/run -d true -c $(C_SIM)'

rtest:
	$(SBT) 'testrun/run -r $(R_SIM)'

rtestd:
	$(SBT) 'testrun/run -d true -r $(R_SIM)'

iretest:
	$(SBT) 'testrun/run -a output/test.S'

cretest:
	$(SBT) 'testrun/run -c $(C_SIM) -a output/test.S'

cretestd:
	$(SBT) 'testrun/run -d true -c $(C_SIM) -a output/test.S'

rretest:
	$(SBT) 'testrun/run -r $(R_SIM) -a output/test.S'

rretestd:
	$(SBT) 'testrun/run -d true -r $(R_SIM) -a output/test.S'

cnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES)'

cnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

rnight:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

rnighte:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

crnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

crnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

cschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

rschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

crschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)' 
