# Convenience Makefile

SBT := ./sbt
C_SIM := ../riscv-rocket/emulator/emulator
R_SIM := ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv
TEST := output/test.S
OPTIONS := $(empty)
SUITE := output
CONFIG := config
INSTCNT := 1
INSTDIR := ..
INSTTYPE := local
COMMIT := none
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-C$(space)
gitopt := $(space)-g$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))
GITCMT := $(subst $(space),$(gitopt),$(COMMIT))

.phony: gen ctest rtest itest igentest cgentest rgentest \
cnight rnight crnight cschaden rschaden \
crschaden csuite rsuite cschadeng rschadeng crschadeng \

gen:
	$(SBT) 'generator/run $(OPTIONS)'

csuite:
	for i in `ls $(SUITE) | grep .S` ; do echo $$i ; \
	result=`make ctest TEST=$(SUITE)/$$i OPTIONS="-s false" | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(SUITE)/tes*[!.S]

rsuite:
	for i in `ls $(SUITE) | grep .S` ; do echo $$i ; \
	result=`make rtest TEST=$(SUITE)/$$i OPTIONS="-s false" | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(SUITE)/tes*[!.S]

crsuite:
	for i in `ls $(SUITE) | grep .S` ; do echo $$i ; \
	result=`make crtest TEST=$(SUITE)/$$i OPTIONS="-s false" | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(SUITE)/tes*[!.S]

igentest:
	$(SBT) 'testrun/run'

cgentest:
	$(SBT) 'testrun/run -c $(C_SIM) $(OPTIONS)'

rgentest:
	$(SBT) 'testrun/run -r $(R_SIM) $(OPTIONS)'

crgentest:
	$(SBT) 'testrun/run -c $(C_SIM) -r $(R_SIM) $(OPTIONS)'

itest:
	$(SBT) 'testrun/run -a $(TEST) $(OPTIONS)'

ctest:
	$(SBT) 'testrun/run -c $(C_SIM) -a $(TEST) $(OPTIONS)'

rtest:
	$(SBT) 'testrun/run -r $(R_SIM) -a $(TEST) $(OPTIONS)'

crtest:
	$(SBT) 'testrun/run -c $(C_SIM) -r $(R_SIM) -a $(TEST) $(OPTIONS)'

cnight:
	$(SBT) 'overnight/run -c $(C_SIM) -g $(COMMIT) $(OPTIONS)'

rnight:
	$(SBT) 'overnight/run -r $(R_SIM) -g $(COMMIT) $(OPTIONS)'

crnight:
	$(SBT) 'overnight/run -c $(C_SIM) -r $(R_SIM) -g $(COMMIT) $(OPTIONS)'

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
