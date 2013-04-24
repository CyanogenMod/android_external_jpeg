/*=========================================================================
* jdidct-armv7.s
*
*  Copyright (c) 2010-2013 The Linux Foundation. All rights reserved.
*
*  Redistribution and use in source and binary forms, with or without
*  modification, are permitted provided that the following conditions are
*  met:
*      * Redistributions of source code must retain the above copyright
*        notice, this list of conditions and the following disclaimer.
*      * Redistributions in binary form must reproduce the above
*        copyright notice, this list of conditions and the following
*        disclaimer in the documentation and/or other materials provided
*        with the distribution.
*      * Neither the name of The Linux Foundation nor the names of its
*        contributors may be used to endorse or promote products derived
*        from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
*  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT
*  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
*  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
*  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
*  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
*  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
*  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
*  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
*  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*==========================================================================

*==========================================================================
*                         FUNCTION LIST
*--------------------------------------------------------------------------
* - idct_1x1_venum
* - idct_2x2_venum
* - idct_4x4_venum
*
*==========================================================================
*/


@==========================================================================
@ END of MACRO DEFINITION
@==========================================================================


    .section idct_func, "x"            @ ARE
    .text                              @ idct_func, CODE, READONLY
    .align 2
    .code 32                           @ CODE32

@==========================================================================
@ Main Routine
@==========================================================================

    .global idct_1x1_venum
    .global idct_2x2_venum
    .global idct_4x4_venum

@==========================================================================
@ FUNCTION     : idct_1x1_venum
@--------------------------------------------------------------------------
@ DISCRIPTION  : ARM optimization of one 1x1 block iDCT
@--------------------------------------------------------------------------
@ C PROTOTYPE  : void idct_1x1_venum(int16 * input,
@                                    int16 * output,
@                                    int32 stride)
@--------------------------------------------------------------------------
@ REG INPUT    : R0 pointer to input (int16)
@                R1 pointer to output (int16)
@                R2 block stride
@--------------------------------------------------------------------------
@ STACK ARG    : None
@--------------------------------------------------------------------------
@ MEM INPUT    : None
@--------------------------------------------------------------------------
@ REG OUTPUT   : None
@--------------------------------------------------------------------------
@ MEM OUTPUT   : None
@--------------------------------------------------------------------------
@ REG AFFECTED : R0 - R2
@--------------------------------------------------------------------------
@ STACK USAGE  : none
@--------------------------------------------------------------------------
@ CYCLES       : 17 cycles
@--------------------------------------------------------------------------
@ NOTES        :
@ This idct_1x1_venum code was developed with ARM instruction set.
@
@ ARM REGISTER ALLOCATION
@ =========================================================================
@ r0  : pointer to input data
@ r1  : pointer to output area
@ r2  : stride in the output buffer
@==========================================================================
.type idct_1x1_venum, %function
idct_1x1_venum:

    ldrsh   r3, [r0]                   @ Load signed half word (int16)
    ldr     r2, =1028                  @ 1028 = 4 + 128 << 3
                                       @ 4 for rounding, 128 for offset
    add     r2, r3, r2
    asrs    r2, r2, #3                 @ Divide by 8, and set status bit
    movmi   r2, #0                     @ Clamp to be greater than 0
    cmp     r2, #255
    movgt   r2, #255                   @ Clamp to be less than 255
    str     r2, [r1]                   @ Save output
    bx      lr                         @ Return to caller

                                       @ end of idct_1x1_venum


@==========================================================================
@ FUNCTION     : idct_2x2_venum
@--------------------------------------------------------------------------
@ DISCRIPTION  : VeNum optimization of one 2x2 block iDCT
@--------------------------------------------------------------------------
@ C PROTOTYPE  : void idct_2x2_venum(int16 * input,
@                                    int16 * output,
@                                    int32 stride)
@--------------------------------------------------------------------------
@ REG INPUT    : R0 pointer to input (int16)
@                R1 pointer to output (int16)
@                R2 block stride
@--------------------------------------------------------------------------
@ STACK ARG    : None
@--------------------------------------------------------------------------
@ MEM INPUT    : None
@--------------------------------------------------------------------------
@ REG OUTPUT   : None
@--------------------------------------------------------------------------
@ MEM OUTPUT   : None
@--------------------------------------------------------------------------
@ REG AFFECTED : R0 - R2
@--------------------------------------------------------------------------
@ STACK USAGE  : none
@--------------------------------------------------------------------------
@ CYCLES       : 27 cycles
@--------------------------------------------------------------------------
@ NOTES        : Output buffer must be an 8x8 16-bit buffer
@
@ ARM REGISTER ALLOCATION
@ ==========================================
@ r0  : pointer to input data
@ r1  : pointer to output area
@ r2  : stride in the output buffer
@ -------------------------------------------
@
@ VENUM REGISTER ALLOCATION
@ =================================================
@ q0     : output x0 - x4
@ q1     : not used
@ q2     : not used
@ q3     : not used
@ q4     : not used
@ q5     : not used
@ q6     : not used
@ q7     : not used
@ q8     : input y0 - y4
@ q9     : intermediate value
@ q10    : intermediate value
@ q11    : offset value
@ q12    : clamp value
@ q13    : not used
@ q14    : not used
@ q15    : not used
@==========================================================================
.type idct_2x2_venum, %function
idct_2x2_venum:

    vld4.32    {d16, d17, d18, d19}, [r0]
                                       @  d16: y0 | y1 | y2 | y3  (LSB | MSB)

    vtrn.32    d16, d17                @  d16: y0 | y1 | X | X
                                       @  d17: y2 | y3 | X | X

    vqadd.s16  d18, d16, d17           @ d18: y0+y2 | y1+y3 | X | X   q: saturated
    vqsub.s16  d19, d16, d17           @ d19: y0-y2 | y1-y3 | X | X   q: saturated

    vtrn.16    d18, d19                @ d18: y0+y2 | y0-y2 | X | X
                                       @ d19: y1+y3 | y1-y3 | X | X

    vqadd.s16  d20, d18, d19           @ d20: (y0+y2)+(y1+y3) | (y0-y2)+(y1-y3)
                                       @       x0 | x2 | X | X
    vqsub.s16  d21, d18, d19           @ d21: (y0+y2)-(y1+y3) | (y0-y2)-(y1-y3)
                                       @       x1 | x3 | X | X

    vtrn.16    d20, d21                @ d20:  x0 | x1 | X | X
                                       @ d21:  x2 | x3 | X | X

    vrshr.s16  q10, q10, #3               @ Divide by 8

    vmov.i16   q11, #128               @ q11 = 128|128|128|128|128|128|128|128
    vqadd.s16  q0, q10, q11            @ Add offset to make output in [0,255]

    vmov.i16   q12, #0                   @ q12 = [0000000000000000]
    vmov.i16   q13, #255               @ q13 = [FFFFFFFFFFFFFFFF] (hex)

    vmax.s16   q0, q0, q12             @ Clamp > 0
    vmin.s16   q0, q0, q13             @ Clamp < 255

    vstr       d0, [r1]                @ Store  x0 | x1 | X | X
                                       @ Potential out of boundary issue
    add        r1, r1, r2              @ Add the offset to the output pointer
    vstr       d1, [r1]                @ Store  x2 | x3 | X | X
                                       @ Potential out of boundary issue
    bx         lr                      @ Return to caller

                                       @ end of idct_2x2_venum


@==========================================================================
@ FUNCTION     : idct_4x4_venum
@--------------------------------------------------------------------------
@ DISCRIPTION  : VeNum optimization of one 4x4 block iDCT
@--------------------------------------------------------------------------
@ C PROTOTYPE  : void idct_4x4_venum(int16 * input,
@                                    int16 * output,
@                                    int32 stride)
@--------------------------------------------------------------------------
@ REG INPUT    : R0 pointer to input (int16)
@                R1 pointer to output (int16)
@                R2 block stride
@--------------------------------------------------------------------------
@ STACK ARG    : None
@--------------------------------------------------------------------------
@ MEM INPUT    : None
@--------------------------------------------------------------------------
@ REG OUTPUT   : None
@--------------------------------------------------------------------------
@ MEM OUTPUT   : None
@--------------------------------------------------------------------------
@ REG AFFECTED : R0 - R3, R12
@--------------------------------------------------------------------------
@ STACK USAGE  : none
@--------------------------------------------------------------------------
@ CYCLES       : 56 cycles
@--------------------------------------------------------------------------
@ NOTES        :
@
@ ARM REGISTER ALLOCATION
@ ==========================================
@ r0  : pointer to input data
@ r1  : pointer to output area
@ r2  : stride in the output buffer
@ r3  : pointer to the coefficient set
@ r12 : pointer to the coefficient set
@ -------------------------------------------
@
@ VENUM REGISTER ALLOCATION
@ =================================================
@ q0     : coefficients[0]
@ q1     : coefficients[1]
@ q2     : coefficients[2]
@ q3     : coefficients[3]
@ q4     : not used
@ q5     : not used
@ q6     : not used
@ q7     : not used
@ q8     : input y0 - y7
@ q9     : input y8 - y15
@ q10    : intermediate value
@ q11    : intermediate value
@ q12    : intermediate value
@ q13    : intermediate value
@ q14    : intermediate value
@ q15    : intermediate value
@==========================================================================
.type idct_4x4_venum, %function
idct_4x4_venum:

        @ Load the locations of the first 2 sets of coefficients
        ldr  r3,   =coefficient+0*16   @ coefficient[0]
        ldr  r12,  =coefficient+1*16   @ coefficient[1]

        @ Load the first 2 sets of coefficients
        vld1.16  {d0, d1},[r3]         @ q0 = C4 | C2 | C4 | C6 | C4 | C2 | C4 | C6
        vld1.16  {d2, d3},[r12]        @ q1 = C4 | C6 | C4 | C2 | C4 | C6 | C4 | C2

        @ Load the locations of the second 2 sets of coefficients
        ldr  r3,   =coefficient+2*16   @ coefficient[2]
        ldr  r12,  =coefficient+3*16   @ coefficient[3]

        @ Load the second 2 sets of coefficients
        vld1.16  {d4, d5},[r3]         @ q2 = C4 | C4 | C4 | C4 | C2 | C2 | C2 | C2
        vld1.16  {d6, d7},[r12]        @ q3 = C4 | C4 | C4 | C4 | C6 | C6 | C6 | C6

        @ Load the input values
        vld1.16  {d16}, [r0], r2       @ d16:   y0  | y1  | y2  | y3  (LSB | MSB)
        vld1.16  {d17}, [r0], r2       @ d17:   y4  | y5  | y6  | y7  (LSB | MSB)
        vld1.16  {d18}, [r0], r2       @ d18:   y8  | y9  | y10 | y11 (LSB | MSB)
        vld1.16  {d19}, [r0], r2       @ d19:   y12 | y13 | y14 | y15 (LSB | MSB)

        @ Apply iDCT Horizonally

        @ q8: y0 |y1 |y2 |y3 |y4 |y5 |y6 |y7
        @ q9: y8 |y9 |y10|y11|y12|y13|y14|y15

        @======================================================================
        @ vqrdmulh doubles the result and save the high 16 bits of the result,
        @ this is equivalent to right shift by 15 bits.
        @ since coefficients are in Q15 format, it contradicts with the right
        @ shift 15 here, so the final result is in Q0 format
        @
        @ vqrdmulh will also round the result
        @======================================================================

        vqrdmulh.s16  q10, q8, q0      @ q10: C4*y0  | C2*y1  | C4*y2  | C6*y3  | C4*y4  | C2*y5  | C4*y6  | C6*y7
        vqrdmulh.s16  q11, q8, q1      @ q11: C4*y0  | C6*y1  | C4*y2  | C2*y3  | C4*y4  | C6*y5  | C4*y6  | C2*y7

        vqrdmulh.s16  q12, q9, q0      @ q12: C4*y8  | C2*y9  | C4*y10 | C6*y11 | C4*y12 | C2*y13 | C4*y14 | C6*y15
        vqrdmulh.s16  q13, q9, q1      @ q13: C4*y8  | C6*y9  | C4*y10 | C2*y11 | C4*y12 | C6*y13 | C4*y14 | C2*y15

        vtrn.32       q10, q12         @ q10: C4*y0  | C2*y1  | C4*y8  | C2*y9  | C4*y4  | C2*y5  | C4*y12 | C2*y13
                                       @ q12: C4*y2  | C6*y3  | C4*y10 | C6*y11 | C4*y6  | C6*y7  | C4*y14 | C6*y15

        vtrn.32       q11, q13         @ q11: C4*y0  | C6*y1  | C4*y8  | C6*y9  | C4*y4  | C6*y5  | C4*y12 | C6*y13
                                       @ q13: C4*y2  | C2*y3  | C4*y10 | C2*y11 | C4*y6  | C2*y7  | C4*y14 | C2*y15

        vqadd.s16     q14, q10, q12    @ q14: C4*y0 + C4*y2 | C2*y1 + C6*y3 | C4*y8 + C4*y10 | C2*y9 + C6*y11 | C4*y4 + C4*y6 | C2*y5 + C6*y7 | C4*y12 + C4*y14 | C2*y13 + C6*y15
                                       @       S0 | S2 | S8 | S10 | S4 | S6 | S12 | S14

        vqsub.s16     q15, q11, q13    @ q15: C4*y0 - C4*y2 | C6*y1 - C2*y3 | C4*y8 - C4*y10 | C6*y9 - C2*y11 | C4*y4 - C4*y6 | C6*y5 - C2*y7 | C4*y12 - C4*y14 | C6*y13 - C2*y15
                                       @       S1 | S3 | S9 | S11 | S5 | S7 | S13 | S15

        vtrn.16       q14, q15         @ q14: S0 | S1 | S8  | S9  | S4 | S5 | S12 | S13
                                       @ q15: S2 | S3 | S10 | S11 | S6 | S7 | S14 | S15

        vqadd.s16     q8, q14, q15     @ q8:  Z0 | Z1 | Z8  | Z9  | Z4 | Z5 | Z12 | Z13
        vqsub.s16     q9, q14, q15     @ q9:  Z3 | Z2 | Z11 | Z10 | Z7 | Z6 | Z15 | Z14
        vrev32.16     q9, q9           @ q9:  Z2 | Z3 | Z10 | Z11 | Z6 | Z7 | Z14 | Z15


        @ Apply iDCT Vertically

        vtrn.32       q8, q9           @ q8:  Z0 | Z1 | Z2  | Z3  | Z4  | Z5  | Z6  | Z7
                                       @ q9:  Z8 | Z9 | Z10 | Z11 | Z12 | Z13 | Z14 | Z15


        vqrdmulh.s16  q10, q8, q2      @ q10: C4*Z0 | C4*Z1 | C4*Z2 | C4*Z3 | C2*Z4 | C2*Z5 | C2*Z6 | C2*Z7
        vqrdmulh.s16  q11, q8, q3      @ q11: C4*Z0 | C4*Z1 | C4*Z2 | C4*Z3 | C6*Z4 | C6*Z5 | C6*Z6 | C6*Z7

        vqrdmulh.s16  q12, q9, q2      @ q12: C4*Z8 | C4*Z9 | C4*Z10 | C4*Z11 | C2*Z12 | C2*Z13 | C2*Z14 | C2*Z15
        vqrdmulh.s16  q13, q9, q3      @ q13: C4*Z8 | C4*Z9 | C4*Z10 | C4*Z11 | C6*Z12 | C6*Z13 | C6*Z14 | C6*Z15

        vqadd.s16     q14, q10, q13    @ q14: C4*Z0+C4*Z8 | C4*Z1+C4*Z9 | C4*Z2+C4*Z10 | C4*Z3+C4*Z11 | C2*Z4+C6*Z12 | C2*Z5+C6*Z13 | C2*Z6+C6*Z14 | C2*Z7+C6*Z15
                                       @      s0 | s4 | s8 | s12 | s2 | s6 | s10 | s14

        vqsub.s16     q15, q11, q12    @ q15: C4*Z0-C4*Z8 | C4*Z1-C4*Z9 | C4*Z2-C4*Z10 | C4*Z3-C4*Z11 | C6*Z4-C2*Z12 | C6*Z5-C2*Z13 | C6*Z6-C2*Z14 | C6*Z7-C2*Z15
                                       @      s1 | s5 | s9 | s13 | s3 | s7 | s11 | s15

        vswp          d29, d30         @ q14: s0 | s4 | s8  | s12 | s1 | s5 | s9  | s13
                                       @ q15: s2 | s6 | s10 | s14 | s3 | s7 | s11 | s15

        vqadd.s16     q8, q14, q15     @ q8:  x0 | x4 | x8  | x12 | x1 | x5 | x9 | x13
        vqsub.s16     q9, q14, q15     @ q9:  x3 | x7 | x11 | x15 | x2 | x6 | x10 | x14

        vmov.i16      q10, #0           @ q10=[0000000000000000]
        vmov.i16      q11, #255        @ q11=[FFFFFFFFFFFFFFFF] (hex)

        vmov.i16      q0, #128         @ q0 = 128|128|128|128|128|128|128|128

        vqadd.s16     q8, q8, q0       @ Add the offset
        vqadd.s16     q9, q9, q0       @ Add the offset

        vmax.s16      q8, q8, q10      @ clamp > 0
        vmin.s16      q8, q8, q11      @ clamp < 255

        vmax.s16      q9, q9, q10      @ clamp > 0
        vmin.s16      q9, q9, q11      @ clamp < 255

        vst1.16       {d16}, [r1], r2  @  d16:   x0 | x1  | x2  | x3  (LSB | MSB)
        vst1.16       {d17}, [r1], r2  @  d17:   x4 | x5  | x6  | x7  (LSB | MSB)
        vst1.16       {d19}, [r1], r2  @  d18:   x8 | x9  | x10 | x11 (LSB | MSB)
        vst1.16       {d18}, [r1], r2  @  d19:   x12| x13 | x14 | x15 (LSB | MSB)

        bx         lr                  @ Return to caller

                                       @ end of idct_4x4_venum


@==========================================================================
@ Coefficient Definition AREA: define idct kernel, bias
@==========================================================================
    .section ro_data_area              @ AREA  RODataArea
    .data                              @ DATA, READONLY
    .align 5                           @ ALIGN=5

coefficient:                           @ These are the coefficent used by 4x4 iDCT in Q15 format
        .hword 11585, 15137,  11585,  6270, 11585, 15137,  11585,  6270  @ C4, C2, C4, C6, C4, C2, C4, C6 /2
        .hword 11585,  6270,  11585, 15137, 11585,  6270,  11585, 15137  @ C4, C6, C4, C2, C4, C6, C4, C2 /2
        .hword 11585, 11585,  11585, 11585, 15137, 15137,  15137, 15137  @ C4, C4, C4, C4, C2, C2, C2, C2 /2
        .hword 11585, 11585,  11585, 11585,  6270,  6270,   6270,  6270  @ C4, C4, C4, C4, C6, C6, C6, C6 /2

.end
