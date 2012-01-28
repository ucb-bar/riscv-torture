#=======================================================================
# UCB VLSI FLOW: Makefile for riscv-tests
#-----------------------------------------------------------------------
# Yunsup Lee (yunsup@cs.berkeley.edu)
#

default: all

#--------------------------------------------------------------------
# Sources
#--------------------------------------------------------------------

asm_tests = \
	test.S \

extra_files = \
	test_riscv.h \

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_GCC = riscv-gcc
RISCV_GCC_OPTS = -O2 -nostdlib -nostartfiles -T test.ld
RISCV_OBJDUMP = riscv-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.data
RISCV_SIM = fesvr -testrun

#------------------------------------------------------------
# Build assembly tests

asm_tests_bin  = $(patsubst %.S, %, $(asm_tests))
asm_tests_dump = $(addsuffix .dump, $(asm_tests_bin))
asm_tests_out  = $(addsuffix .out,  $(asm_tests_bin))

$(asm_tests_dump): %.dump: %
	$(RISCV_OBJDUMP) $< > $@

$(asm_tests_bin): %: %.S
	$(RISCV_GCC) $(RISCV_GCC_OPTS) $< -o $@

$(asm_tests_out): %.out: %
	$(RISCV_SIM) $< > $@

riscv-: $(asm_tests_dump)
run: $(asm_tests_out)
	echo; perl -ne 'print "  [$$1] $$ARGV \t$$2\n" if /\*{3}(.{8})\*{3}(.*)/' \
	       $(asm_tests_out); echo;

junk += $(asm_tests_bin) $(asm_tests_dump) $(asm_tests_out)

#------------------------------------------------------------
# Default

all: riscv-

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(junk)
