# Convenience Makefile

SBT := ./sbt
RTL_CONFIG := DefaultConfig
C_SIM := ../emulator/emulator-Top-$(RTL_CONFIG)-debug
R_SIM := ../vsim/simv-Top-$(RTL_CONFIG)-debug
TEST := output/test.S
OPTIONS := $(empty)
EC2OPTIONS := $(empty)
SUITE := output
CONFIG := config
COMMIT := none
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-f$(space)
gitopt := $(space)-g$(space)
CFG := $(subst $(space),$(cfgopt),$(CONFIG))
GITCMT := $(subst $(space),$(gitopt),$(COMMIT))

.phony: gen ctest rtest itest igentest cgentest rgentest \
cnight rnight crnight cschaden rschaden \
crschaden csuite rsuite \

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
	$(SBT) 'schadenfreude/run -f $(CFG) -c $(C_SIM) -g $(GITCMT) $(EC2OPTIONS) $(OPTIONS)'

rschaden:
	$(SBT) 'schadenfreude/run -f $(CFG) -r $(R_SIM) -g $(GITCMT) $(EC2OPTIONS) $(OPTIONS)'

crschaden:
	$(SBT) 'schadenfreude/run -f $(CFG) -c $(C_SIM) -r $(R_SIM) -g $(GITCMT) $(EC2OPTIONS) $(OPTIONS)'
