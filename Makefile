# Convenience Makefile

SBT ?= java -Xmx1G -Xss8M -XX:MaxPermSize=128M -jar sbt-launch.jar
CONFIG ?= DefaultConfig
ISA ?= RM64IMAFD
C_SIM := ../emulator/emulator-rocketchip-$(CONFIG)
R_SIM := ../vsim/simv-rocketchip-$(CONFIG)
TEST := output/test.S
OPTIONS := $(empty)
SUITE := output
TORTURE_CONFIG := config/default.config
COMMIT := none
empty :=
space := $(empty) $(empty)
cfgopt := $(space)-f$(space)
gitopt := $(space)-g$(space)
CFG := $(subst $(space),$(cfgopt),$(TORTURE_CONFIG))
GITCMT := $(subst $(space),$(gitopt),$(COMMIT))

.phony: gen ctest rtest itest igentest cgentest rgentest \
cnight rnight crnight csuite rsuite \

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
	$(SBT) 'testrun/run -I $(ISA)'

cgentest:
	$(SBT) 'testrun/run -c $(C_SIM) -I $(ISA) $(OPTIONS)'

rgentest:
	$(SBT) 'testrun/run -r $(R_SIM) -I $(ISA) $(OPTIONS)'

crgentest:
	$(SBT) 'testrun/run -c $(C_SIM) -r $(R_SIM) -I $(ISA) $(OPTIONS)'

itest:
	$(SBT) 'testrun/run -a $(TEST) -I $(ISA) $(OPTIONS)'

ctest:
	$(SBT) 'testrun/run -c $(C_SIM) -a $(TEST) -I $(ISA) $(OPTIONS)'

rtest:
	$(SBT) 'testrun/run -r $(R_SIM) -a $(TEST) -I $(ISA) $(OPTIONS)'

crtest:
	$(SBT) 'testrun/run -c $(C_SIM) -r $(R_SIM) -a $(TEST) -I $(ISA) $(OPTIONS)'

cnight:
	$(SBT) 'overnight/run -c $(C_SIM) -g $(COMMIT) $(OPTIONS)'

rnight:
	$(SBT) 'overnight/run -r $(R_SIM) -g $(COMMIT) $(OPTIONS)'

crnight:
	$(SBT) 'overnight/run -c $(C_SIM) -r $(R_SIM) -g $(COMMIT) $(OPTIONS)'

