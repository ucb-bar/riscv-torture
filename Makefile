# Convenience Makefile

SBT := ./sbt
C_SIM := ../riscv-rocket/emulator/emulator
R_SIM := ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv
SEEK=true
TEST = output/test.S
TESTDIR = output
DIR :=  output/failedtests
ERRORS := 5
MINUTES := 1
EMAIL := your@email.address
CONFIG := config
INSTCNT := 1
INSTDIR := ..
INSTTYPE := local
COMMIT := HEAD
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-C$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))

.phony: gen ctest ctestd rtest rtestd itest iretest cretest cretestd rretest \
rretestd cnight rnight crnight cnighte rnighte crnighte cschaden rschaden    \
crschaden csuite rsuite

gen:
	$(SBT) 'generator/run'

csuite:
	for i in `ls $(TESTDIR) | grep .S` ; do echo $$i ; \
	result=`make cretest TEST=$(TESTDIR)/$$i SEEK=false | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(TESTDIR)/tes*[!.S]

rsuite:
	for i in `ls $(TESTDIR) | grep .S` ; do echo $$i ; \
	result=`make rretest TEST=$(TESTDIR)/$$i SEEK=false | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(TESTDIR)/tes*[!.S]

itest:
	$(SBT) 'testrun/run'

ctest:
	$(SBT) 'testrun/run -c $(C_SIM) -s $(SEEK)'

ctestd:
	$(SBT) 'testrun/run -d true -c $(C_SIM) -s $(SEEK)'

rtest:
	$(SBT) 'testrun/run -r $(R_SIM) -s $(SEEK)'

rtestd:
	$(SBT) 'testrun/run -d true -r $(R_SIM) -s $(SEEK)'

iretest:
	$(SBT) 'testrun/run -a $(TEST)'

cretest:
	$(SBT) 'testrun/run -c $(C_SIM) -a $(TEST) -s $(SEEK)'

cretestd:
	$(SBT) 'testrun/run -d true -c $(C_SIM) -a $(TEST) -s $(SEEK)'

rretest:
	$(SBT) 'testrun/run -r $(R_SIM) -a $(TEST) -s $(SEEK)'

rretestd:
	$(SBT) 'testrun/run -d true -r $(R_SIM) -a $(TEST) -s $(SEEK)'

cnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES)'

cnightg:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

cnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

rnight:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

rnightg:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

rnighte:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

crnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

crnightg:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

crnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

cschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

rschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

crschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)' 
