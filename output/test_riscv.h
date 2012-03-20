#*****************************************************************************
# riscv-v2_macros.S
#-----------------------------------------------------------------------------
#
# Helper macros for forming test cases.
#

#-----------------------------------------------------------------------
# Begin Macro
#-----------------------------------------------------------------------

#define TEST_RISCV

#define TEST_CODEBEGIN                                                  \
        .text;                                                          \
        .align  4;                                                      \
        .global _start;                                                 \
        .ent    _start;                                                 \
_start:                                                                 \

#define TEST_FP_ENABLE \
  mfpcr t0, cr0; \
  or    t0, t0, 2; \
  mtpcr t0, cr0; \
  mtfsr x0

#define TEST_VEC_ENABLE \
  mfpcr t0, cr0; \
  ori   t0, t0, 4; \
  mtpcr t0, cr0; \
  li    t0, 0xff; \
  mtpcr t0, cr11; \

#-----------------------------------------------------------------------
# End Macro
#-----------------------------------------------------------------------

#define TEST_CODEEND                                                    \
        .end _start                                                     \

#-----------------------------------------------------------------------
# Pass/Fail Macro
#-----------------------------------------------------------------------

#define TEST_FAIL                                                       \
        fence;                                                          \
        li  x1, 2;                                                      \
        mtpcr x1, cr16;                                                 \
1:      beq x0, x0, 1b;                                                 \
        nop;                                                            \

#define TEST_PASS                                                       \
        fence;                                                          \
        li  x1, 1;                                                      \
        mtpcr x1, cr16;                                                 \
1:      beq x0, x0, 1b;                                                 \
        nop;                                                            \

#-----------------------------------------------------------------------
# Data Section Macro
#-----------------------------------------------------------------------

#define TEST_DATABEGIN .align 4; .global begin_signature; begin_signature:

#define TEST_DATAEND .global end_signature; end_signature:
