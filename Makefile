# Convenience Makefile

C_SIM = ../riscv-rocket/emulator/emulator
R_SIM = ../riscv-rocket/vlsi-generic/build/vcs-sim-rtl/simv
DIR =  ./output
ERRORS = 5
MINUTES = 1
SEEK=true
TEST = output/test.S
TESTDIR = output

.phony: gen ctest ctestd rtest rtestd itest iretest cretest cretestd rretest \
rretestd cnight rnight crnight csuite rsuite

gen:
	sbt 'generator/run'

csuite:
	for i in `ls $(TESTDIR) | grep .S` ; do echo $$i ; \
	result=`make cretest TEST=$(TESTDIR)/$$i SEEK=false | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(TESTDIR)/tes*[!.S]

rsuite:
	for i in `ls $(TESTDIR) | grep .S` ; do echo $$i ; \
	result=`make rretest TEST=$(TESTDIR)/$$i SEEK=false | grep 'Simulation failed\|signatures match'` ; \
	echo $$result ; done
	rm $(TESTDIR)/test*[!.S]

itest:
	sbt 'testrun/run'

ctest:
	sbt 'testrun/run -c $(C_SIM) -s $(SEEK)'

ctestd:
	sbt 'testrun/run -d true -c $(C_SIM) -s $(SEEK)'

rtest:
	sbt 'testrun/run -r $(R_SIM) -s $(SEEK)'

rtestd:
	sbt 'testrun/run -d true -r $(R_SIM) -s $(SEEK)'

iretest:
	sbt 'testrun/run -a $(TEST)'

cretest:
	sbt 'testrun/run -c $(C_SIM) -a $(TEST) -s $(SEEK)'

cretestd:
	sbt 'testrun/run -d true -c $(C_SIM) -a $(TEST) -s $(SEEK)'

rretest:
	sbt 'testrun/run -r $(R_SIM) -a $(TEST) -s $(SEEK)'

rretestd:
	sbt 'testrun/run -d true -r $(R_SIM) -a $(TEST) -s $(SEEK)'

cnight:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -t $(ERRORS) -m $(MINUTES)'

rnight:
	sbt 'overnight/run -p $(DIR) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'

crnight:
	sbt 'overnight/run -p $(DIR) -c $(C_SIM) -r $(R_SIM) -t $(ERRORS) -m $(MINUTES)'
