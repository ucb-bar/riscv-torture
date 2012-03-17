#ifndef _TEST_RISCV_VM_H
#define _TEST_RISCV_VM_H

//*****************************************************************************
// riscv-v2_macros.S
//-----------------------------------------------------------------------------
//
// Helper macros for forming test cases.
//

//-----------------------------------------------------------------------
// Begin Macro
//-----------------------------------------------------------------------

#define TEST_RISCV

#define TEST_CODEBEGIN                                                  \
        .text;                                                          \
        .align  4;                                                      \
        .global _start;                                                 \
        .ent    _start;                                                 \
_start:                                                                 \
        la sp, stack_top;                                               \
        li a1, 1337;                                                    \
        rdnpc a0;                                                       \
        addi a0, a0, 8;                                                 \
        j vm_boot

//-----------------------------------------------------------------------
// End Macro
//-----------------------------------------------------------------------

#define TEST_CODEEND                                                    \
        .end _start                                                     \

//-----------------------------------------------------------------------
// Pass/Fail Macro
//-----------------------------------------------------------------------

#define TEST_PASS li a0, 1; syscall
#define TEST_FAIL li a0, 2; syscall

//-----------------------------------------------------------------------
// Data Section Macro
//-----------------------------------------------------------------------

#define TEST_DATABEGIN .align 4; .global begin_signature; begin_signature:

#define TEST_DATAEND .global end_signature; end_signature:

//-----------------------------------------------------------------------
// Supervisor mode definitions and macros
//-----------------------------------------------------------------------

#define ASM_CR(r)   _ASM_CR(r)
#define _ASM_CR(r)  cr##r

#define mtpcr(reg,val) ({ long __tmp = (long)(val); \
          asm volatile ("mtpcr %0,cr%1"::"r"(__tmp),"i"(reg)); })

#define mfpcr(reg) ({ long __tmp; \
          asm volatile ("mfpcr %0,cr%1" : "=r"(__tmp) : "i"(reg)); \
          __tmp; })

#define PCR_SR       0
#define PCR_EPC      1
#define PCR_BADVADDR 2
#define PCR_EVEC     3
#define PCR_COUNT    4
#define PCR_COMPARE  5
#define PCR_CAUSE    6
#define PCR_PTBR     7
#define PCR_SEND_IPI 8
#define PCR_CLR_IPI  9
#define PCR_COREID   10
#define PCR_K0       12
#define PCR_K1       13
#define PCR_TOHOST   16
#define PCR_FROMHOST 17
#define PCR_CONSOLE  18

#define CAUSE_MISALIGNED_FETCH 0
#define CAUSE_FAULT_FETCH 1
#define CAUSE_ILLEGAL_INSTRUCTION 2
#define CAUSE_PRIVILEGED_INSTRUCTION 3
#define CAUSE_FP_DISABLED 4
#define CAUSE_SYSCALL 6
#define CAUSE_BREAKPOINT 7
#define CAUSE_MISALIGNED_LOAD 8
#define CAUSE_MISALIGNED_STORE 9
#define CAUSE_FAULT_LOAD 10
#define CAUSE_FAULT_STORE 11
#define CAUSE_VECTOR_DISABLED 12
#define CAUSE_IRQ0 16
#define CAUSE_IRQ1 17
#define CAUSE_IRQ2 18
#define CAUSE_IRQ3 19
#define CAUSE_IRQ4 20
#define CAUSE_IRQ5 21
#define CAUSE_IRQ6 22
#define CAUSE_IRQ7 23
#define NUM_CAUSES 24

#define SR_ET    0x00000001
#define SR_EF    0x00000002
#define SR_EV    0x00000004
#define SR_EC    0x00000008
#define SR_PS    0x00000010
#define SR_S     0x00000020
#define SR_UX    0x00000040
#define SR_SX    0x00000080
#define SR_IM    0x0000FF00
#define SR_VM    0x00010000
#define SR_ZERO  ~(SR_ET|SR_EF|SR_EV|SR_EC|SR_PS|SR_S|SR_UX|SR_SX|SR_IM|SR_VM)
#define SR_IM_SHIFT 8
#define IPI_IRQ 5
#define TIMER_IRQ 7

#define MAX_TEST_PAGES 127 // this must be the period of the LFSR below
#define LFSR_NEXT(x) (((((x)^((x)>>1)) & 1) << 6) | ((x) >> 1))

#define PGSHIFT 13
#define PGSIZE (1 << PGSHIFT)

#ifndef __ASSEMBLER__


typedef unsigned long pte_t;
#define LEVELS (sizeof(pte_t) == sizeof(uint64_t) ? 3 : 2)
#define PTIDXBITS (PGSHIFT - (sizeof(pte_t) == 8 ? 3 : 2))
#define VPN_BITS (PTIDXBITS * LEVELS)
#define VA_BITS (VPN_BITS + PGSHIFT)
#define PTES_PER_PT (PGSIZE/sizeof(pte_t))

typedef struct
{
  long gpr[32];
  long sr;
  long epc;
  long badvaddr;
  long cause;
  long insn;
} trapframe_t;
#endif

#endif
