# Convenience Makefile

SBT := ./sbt
C_SIM := ../riscv-rocket/emulator/emulator
R_SIM := ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv
TEST := output/test.S
OPTIONS := $(empty)
SUITE := output
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
gitopt := $(space)-g$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))
GITCMT := $(subst $(space),$(gitopt),$(COMMIT))

.phony: gen ctest rtest itest iretest cretest rretest \
cnight rnight crnight cnighte rnighte crnighte cschaden rschaden    \
crschaden csuite rsuite cnightg rnightg crnightg cschadeng rschadeng crschadeng \

gen:
	$(SBT) 'generator/run $(OPTIONS)'

csuite:
	for i in `ls $(SUITE) | grep .S` ; do echo $$i ; \
	result=`make cretest TEST=$(SUITE)/$$i OPTIONS="-s false" | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(SUITE)/tes*[!.S]

rsuite:
	for i in `ls $(SUITE) | grep .S` ; do echo $$i ; \
	result=`make rretest TEST=$(SUITE)/$$i OPTIONS="-s false" | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(SUITE)/tes*[!.S]

itest:
	$(SBT) 'testrun/run'

ctest:
	$(SBT) 'testrun/run -c $(C_SIM) $(OPTIONS)'

rtest:
	$(SBT) 'testrun/run -r $(R_SIM) $(OPTIONS)'

iretest:
	$(SBT) 'testrun/run -a $(TEST) $(OPTIONS)'

cretest:
	$(SBT) 'testrun/run -c $(C_SIM) -a $(TEST) $(OPTIONS)'

rretest:
	$(SBT) 'testrun/run -r $(R_SIM) -a $(TEST) $(OPTIONS)'

cnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES)'

cnightg:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

cnightge:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT) -e $(EMAIL)'

cnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

rnight:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

rnightg:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

rnightge:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT) -e $(EMAIL)'

rnighte:
	$(SBT) 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

crnight:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

crnightg:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT)'

crnightge:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -g $(COMMIT) -e $(EMAIL)'

crnighte:
	$(SBT) 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -e $(EMAIL)'

cschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

cschadeng:
	$(SBT) 'schadenfreude/run -C $(CFG) -g $(GITCMT) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

rschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

rschadeng:
	$(SBT) 'schadenfreude/run -C $(CFG) -g $(GITCMT) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)'

crschaden:
	$(SBT) 'schadenfreude/run -C $(CFG) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)' 

crschadeng:
	$(SBT) 'schadenfreude/run -C $(CFG) -g $(GITCMT) -p $(DIR) -d $(INSTDIR) -i $(INSTTYPE) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES) -n $(INSTCNT) -e $(EMAIL)' 
