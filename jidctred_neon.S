/*
 * Copyright (C) ST-Ericsson SA 2010
 *
 * ARM/Neon assembler optimized dequantization and
 * inverse DCT for reduced-size output.
 * See jidctred.c for additional comments.
 */

#define off_COMPINFO_quanttable 80
#define DCTSIZE     8
#define CONST_BITS  13
#define PASS1_BITS  2
#define  VY(x)      ((x)*DCTSIZE*2)


#define FIX_0_211164243  1730   /* FIX(0.211164243) */
#define FIX_0_509795579  4176   /* FIX(0.509795579) */
#define FIX_0_601344887  4926   /* FIX(0.601344887) */
#define FIX_0_720959822  5906   /* FIX(0.720959822) */
#define FIX_0_765366865  6270   /* FIX(0.765366865) */
#define FIX_0_850430095  6967   /* FIX(0.850430095) */
#define FIX_0_899976223  7373   /* FIX(0.899976223) */
#define FIX_1_061594337  8697   /* FIX(1.061594337) */
#define FIX_1_272758580  10426  /* FIX(1.272758580) */
#define FIX_1_451774981  11893  /* FIX(1.451774981) */
#define FIX_1_847759065  15137  /* FIX(1.847759065) */
#define FIX_2_172734803  17799  /* FIX(2.172734803) */
#define FIX_2_562915447  20995  /* FIX(2.562915447) */
#define FIX_3_624509785  29692  /* FIX(3.624509785) */

/*
 * Perform dequantization and inverse DCT on one block of coefficients,
 * producing a reduced-size 4x4 output block.
 */
    .text
    .fpu    neon
    .align

    .global jpeg_idct_4x4
    .func   jpeg_idct_4x4

jpeg_idct_4x4:
    pld           [r2, #0]
    pld           [r2, #32]
    ldr           r1, [r1, #off_COMPINFO_quanttable]         // quanttable
    pld           [r1, #0]

    /* Pass 1: process all columns from input */

    /* Even part */

    /* Usually many of the input coefficients are zero, especially the
     * AC terms. If all column's AC terms are zero, we short-circuiting
     * the IDCT calculation.
     */

    // tmp0 = DEQUANTIZE(inptr[DCTSIZE*0], quantptr[DCTSIZE*0]);  (q0)
    pld           [r2, #80]
    vld1.16       {q0}, [r2]!          // Read row 0 from coef_block
    vld1.16       {q1}, [r2]!          // Read row 1
    pld           [r2, #80]
    vld1.16       {q2}, [r2]!          // Read row 2
    vld1.16       {q3}, [r2]           // Read row 3
    add           r2, #32
    vld1.16       {q8}, [r2]!          // Read row 5
    vorr          q12, q1, q2
    vld1.16       {q9}, [r2]!          // Read row 6
    vld1.16       {q10}, [r2]          // Read row 7
    vorr          q13, q3, q8
    vld1.16       {q11}, [r1]          // Read row 0 from quant table
    vorr          q12, q13
    add           r1, #VY(2)
    vorr          q13, q9, q10
    vmul.i16      q0, q11              // Dequantizised row 0 in q0
    vorr          q12, q13
    pld           [r1, #0]
    vorr          d24, d25
    vmov          r0, r2, d24
    orrs          r0, r2
    /* AC terms all zero */
    beq           .LVHeadZero4x4

    // tmp0 <<= (CONST_BITS+1);  (q0, q11)
    vshll.s16     q11, d1, #(CONST_BITS + 1)
    vld1.16       {q14}, [r1]          // Read row 2 from quant table
    vshll.s16     q0, d0, #(CONST_BITS + 1)

    // z2 = DEQUANTIZE(inptr[DCTSIZE*2], quantptr[DCTSIZE*2]);  (q2)
    add           r1, #VY(4)
    ldr           r0, =FIX_1_847759065
    vmul.i16      q2, q14              // Dequantizised row 2 in q2
    pld           [r1, #0]
    // MULTIPLY(z2, FIX_1_847759065)  (q2, q12)
    vdup.16       q15, r0
    vmull.s16     q12, d5, d31
    vld1.16       {q14}, [r1]          // Read row 6 from quant table
    vmull.s16     q2, d4, d30

    // z3 = DEQUANTIZE(inptr[DCTSIZE*6], quantptr[DCTSIZE*6]);  (q9)
    add           r1, #VY(1)
    ldr           r0, =-FIX_0_765366865
    vmul.i16      q9, q14              // Dequantizised row 6 in q9
    pld           [r1, #0]
    vdup.16       q15, r0

    // tmp2 = MULTIPLY(z2, FIX_1_847759065) + MULTIPLY(z3, - FIX_0_765366865);  (q2, q12)
    vmlal.s16     q12, d19, d31
    vmlal.s16     q2, d18, d30

    // tmp10 = tmp0 + tmp2;  (q13-14)
    vadd.i32      q14, q11, q12
    vadd.i32      q13, q0, q2

    // tmp12 = tmp0 - tmp2;  (q0, q11)
    vsub.i32      q0, q2
    vld1.16       {q15}, [r1]          // Read row 7 from quant table
    vsub.i32      q11, q12

    /* Odd part */

    // z1 = DEQUANTIZE(inptr[DCTSIZE*7], quantptr[DCTSIZE*7]);  (q10)
    sub           r1, #VY(2)
    ldr           r0, =-FIX_0_211164243
    vmul.i16      q10, q15             // Dequantizised row 7 in q10
    pld           [r1, #0]
    vdup.16       q15, r0

    // tmp0 = MULTIPLY(z1, - FIX_0_211164243)  (q2, q9)
    vmull.s16     q2, d20, d30
    ldr           r0, =-FIX_0_509795579
    vmull.s16     q9, d21, d31

    // tmp2 = MULTIPLY(z1, - FIX_0_509795579)  (q10, q12)
    vdup.16       q15, r0
    vmull.s16     q12, d21, d31
    vmull.s16     q10, d20, d30

    // z2 = DEQUANTIZE(inptr[DCTSIZE*5], quantptr[DCTSIZE*5]);  (q8)
    vld1.16       {q15}, [r1]          // Read row 5 from quant table
    ldr           r0, =FIX_1_451774981
    sub           r1, #VY(2)
    vmul.i16      q8, q15              // Dequantizised row 5 in q8
    pld           [r1, #0]
    vdup.16       q15, r0

    // tmp0 += MULTIPLY(z2, FIX_1_451774981)
    vmlal.s16     q2, d16, d30
    ldr           r0, =-FIX_0_601344887
    vmlal.s16     q9, d17, d31

    // tmp2 += MULTIPLY(z2, - FIX_0_601344887)
    vdup.16       q15, r0
    vmlal.s16     q10, d16, d30
    ldr           r0, =-FIX_2_172734803
    vmlal.s16     q12, d17, d31

    // z3 = DEQUANTIZE(inptr[DCTSIZE*3], quantptr[DCTSIZE*3]);  (q3)
    vld1.16       {q15}, [r1]          // Read row 3 from quant table
    vdup.16       q8, r0
    sub           r1, #VY(2)
    vmul.i16      q3, q15              // Dequantizised row 3 in q3
    ldr           r0, =FIX_0_899976223
    pld           [r1, #0]

    // tmp0 += MULTIPLY(z3, - FIX_2_172734803)
    vmlal.s16     q2, d6, d16
    vdup.16       q15, r0
    vmlal.s16     q9, d7, d17

    // tmp2 += MULTIPLY(z3, FIX_0_899976223)
    ldr           r0, =FIX_1_061594337
    vmlal.s16     q10, d6, d30
    vld1.16       {q8}, [r1]           // Read row 1 from quant table
    vmlal.s16     q12, d7, d31

    // z4 = DEQUANTIZE(inptr[DCTSIZE*1], quantptr[DCTSIZE*1]);  (q1)
    ldr           r2, =FIX_2_562915447
    vmul.i16      q1, q8               // Dequantizised row 1 in q1
    vdup.16       q15, r0

    // tmp0 += MULTIPLY(z4, FIX_1_061594337)
    vmlal.s16     q2, d2, d30
    vdup.16       q8, r2
    vmlal.s16     q9, d3, d31

    // tmp2 += MULTIPLY(z4, FIX_2_562915447)
    vmlal.s16     q10, d2, d16
    vmlal.s16     q12, d3, d17

    /* Final calculations for column pass, and rotation for pass 2 */

    // wsptr[DCTSIZE*1] = (int) DESCALE(tmp12 + tmp0, CONST_BITS-PASS1_BITS+1);  (q1)
    vadd.i32      q8, q0, q2
    vrshrn.i32    d2, q8, #(CONST_BITS - PASS1_BITS + 1)
    vadd.i32      q15, q11, q9
    vrshrn.i32    d3, q15, #(CONST_BITS - PASS1_BITS + 1)

    // wsptr[DCTSIZE*2] = (int) DESCALE(tmp12 - tmp0, CONST_BITS-PASS1_BITS+1);  (q2)
    vsub.i32      q8, q0, q2
    vrshrn.i32    d4, q8, #(CONST_BITS - PASS1_BITS + 1)
    vsub.i32      q15, q11, q9
    vrshrn.i32    d5, q15, #(CONST_BITS - PASS1_BITS + 1)

    // wsptr[DCTSIZE*0] = (int) DESCALE(tmp10 + tmp2, CONST_BITS-PASS1_BITS+1);  (q0)
    vadd.i32      q8, q13, q10
    vrshrn.i32    d0, q8, #(CONST_BITS - PASS1_BITS + 1)
    vadd.i32      q15, q14, q12
    vrshrn.i32    d1, q15, #(CONST_BITS - PASS1_BITS + 1)

    // wsptr[DCTSIZE*3] = (int) DESCALE(tmp10 - tmp2, CONST_BITS-PASS1_BITS+1);  (q3)
    vsub.i32      q8, q13, q10
    vtrn.32       q0, q2
    vrshrn.i32    d6, q8, #(CONST_BITS - PASS1_BITS + 1)
    vsub.i32      q15, q14, q12
    vrshrn.i32    d7, q15, #(CONST_BITS - PASS1_BITS + 1)

    vtrn.32       q1, q3

    vtrn.16       q0, q1
    vtrn.16       q2, q3

    /* Pass 2: process 4 rows, store into output array. */
.LVTail4x4:
    /* Even part */

    // tmp0 = ((INT32) wsptr[0]) << (CONST_BITS+1);  (q0)
    ldr           r0, =FIX_1_847759065
    vshll.s16     q0, d0, #(CONST_BITS + 1)

    // tmp2 = MULTIPLY((INT32) wsptr[2], FIX_1_847759065)  (q8)
    vdup.16       d31, r0
    ldr           r1, =-FIX_0_765366865
    vmull.s16     q8, d4, d31
    ldr           r0, =-FIX_0_211164243

    //  + MULTIPLY((INT32) wsptr[6], - FIX_0_765366865);
    vdup.16       d30, r1
    vmlal.s16     q8, d5, d30
    ldr           r1, =FIX_1_451774981
    vdup.16       d31, r0

    // tmp10 = tmp0 + tmp2;
    vadd.i32      q9, q0, q8
    ldr           r0, =-FIX_2_172734803
    vdup.16       d30, r1

    // tmp12 = tmp0 - tmp2;
    ldr           r2, =FIX_1_061594337
    vsub.i32      q0, q8

    /* Odd part */

    // tmp0 = MULTIPLY(z1, - FIX_0_211164243)
    vdup.16       d29, r0
    vmull.s16     q8, d7, d31
    ldr           r0, =-FIX_0_509795579
    vdup.16       d28, r2

    //  + MULTIPLY(z2, FIX_1_451774981)
    vmlal.s16     q8, d3, d30
    ldr           r1, =-FIX_0_601344887
    vdup.16       d31, r0

    //  + MULTIPLY(z3, - FIX_2_172734803)
    vmlal.s16     q8, d6, d29
    vdup.16       d30, r1
    ldr           r2, =FIX_0_899976223

    //  + MULTIPLY(z4, FIX_1_061594337);
    vmlal.s16     q8, d2, d28
    pld           [r3, #0]
    vdup.16       d29, r2

    // tmp2 = MULTIPLY(z1, - FIX_0_509795579)
    ldr           r0, =FIX_2_562915447
    vmull.s16     q10, d7, d31

    //  + MULTIPLY(z2, - FIX_0_601344887)
    vmlal.s16     q10, d3, d30
    vdup.16       d28, r0

    //  + MULTIPLY(z3, FIX_0_899976223)
    vmlal.s16     q10, d6, d29

    //  + MULTIPLY(z4, FIX_2_562915447);
    vmlal.s16     q10, d2, d28

    /* Final output stage: scale down and range-limit */

    // outptr[0] = range_limit[(int) DESCALE(tmp10 + tmp2, CONST_BITS+PASS1_BITS+3+1) & RANGE_MASK];
    vmov.i16      d31, #2048
    vadd.i32      q1, q9, q10
    vrshrn.i32    d22, q1, #(CONST_BITS + PASS1_BITS)
    vadd.i16      d22, d31

    // outptr[1] = range_limit[(int) DESCALE(tmp12 + tmp0, CONST_BITS+PASS1_BITS+3+1) & RANGE_MASK];
    vadd.i32      q2, q0, q8
    vrshrn.i32    d24, q2, #(CONST_BITS + PASS1_BITS)
    vadd.i16      d24, d31

    // outptr[2] = range_limit[(int) DESCALE(tmp12 - tmp0, CONST_BITS+PASS1_BITS+3+1) & RANGE_MASK];
    vsub.i32      q0, q8
    ldr           r1, [r3], #4
    vrshrn.i32    d26, q0, #(CONST_BITS + PASS1_BITS)
    vadd.i16      d26, d31

    // outptr[3] = range_limit[(int) DESCALE(tmp10 - tmp2, CONST_BITS+PASS1_BITS+3+1) & RANGE_MASK];
    vsub.i32      q9, q10
    ldr           r0, [sp, #0]         // output_col array
    vrshrn.i32    d28, q9, #(CONST_BITS + PASS1_BITS)
    vtrn.32       d22, d26
    add           r1, r0
    vadd.i16      d28, d31

    ldr           r2, [r3], #4
    vtrn.32       d24, d28
    add           r2, r0
    vtrn.16       d22, d24
    vtrn.16       d26, d28

    // output = *output_buf++ + output_col
    vqrshrun.s16  d0, q11, #(3 + 1)
    vst1.32       {d0[0]}, [r1]

    vqrshrun.s16  d1, q12, #(3 + 1)
    ldr           r1, [r3], #4
    vst1.32       {d1[0]}, [r2]

    add           r1, r0
    vqrshrun.s16  d2, q13, #(3 + 1)
    ldr           r2, [r3]
    vst1.32       {d2[0]}, [r1]

    add           r2, r0
    vqrshrun.s16  d3, q14, #(3 + 1)
    vst1.32       {d3[0]}, [r2]
    bx            lr

.LVHeadZero4x4:
    /* AC terms all zero */
    vshl.i16      q0, #PASS1_BITS
    vdup.16       d2, d0[1]    // (1)
    vdup.16       d4, d0[2]    // (2)
    vdup.16       d6, d0[3]    // (3)
    vdup.16       d3, d1[1]    // (5)
    vdup.16       d5, d1[2]    // (6)
    vdup.16       d7, d1[3]    // (7)
    vdup.16       d0, d0[0]    // (0)
    b             .LVTail4x4

    .endfunc


/*
 * Perform dequantization and inverse DCT on one block of coefficients,
 * producing a reduced-size 2x2 output block.
 */
    .text
    .fpu    neon
    .align

    .global jpeg_idct_2x2
    .func   jpeg_idct_2x2

jpeg_idct_2x2:
    pld           [r2, #0]
    pld           [r2, #48]
    ldr           r1, [r1, #off_COMPINFO_quanttable]         // quanttable
    pld           [r1, #0]

    /* Pass 1: process all columns from input */

    /* Even part */

    /* Usually many of the input coefficients are zero, especially the
     * AC terms. If all column's AC terms are zero, we short-circuiting
     * the IDCT calculation.
     */

    // z1 = DEQUANTIZE(inptr[DCTSIZE*0], quantptr[DCTSIZE*0]);  (q0)
    pld           [r2, #80]
    vld1.16       {q0}, [r2]!          // Read row 0 from coef_block
    vld1.16       {q13}, [r2]          // Read row 1
    add           r2, #32
    pld           [r2, #80]
    vld1.16       {q8}, [r2]           // Read row 3
    add           r2, #32
    vld1.16       {q1}, [r2]           // Read row 5
    vorr          q11, q13, q8
    add           r2, #32
    vld1.16       {q9}, [r2]           // Read row 7
    vld1.16       {q14}, [r1]          // Read row 0 from quant table
    vorr          q12, q1, q9
    add           r1, #VY(7)
    vorr          q11, q12
    pld           [r1, #0]
    vorr          d22, d23
    vmul.i16      q0, q14              // Dequantizised row 0 in q0
    vmov          r0, r2, d22
    orrs          r0, r2
    /* AC terms all zero */
    adr           r2, .LConst2x2
    pld           [r2, #0]
    beq           .LVHeadZero2x2

    // tmp10 = z1 << (CONST_BITS+2);  (q2-3)
    vshll.s16     q2, d0, #(CONST_BITS + 2)
    vld1.16       {q14}, [r1]          // Read row 7 from quant table
    vshll.s16     q3, d1, #(CONST_BITS + 2)

    /* Odd part */

    // z1 = DEQUANTIZE(inptr[DCTSIZE*7], quantptr[DCTSIZE*7]);
    sub           r1, #VY(2)
    ldr           r0, =-FIX_0_720959822
    vmul.i16      q9, q14              // Dequantizised row 7 in q9
    pld           [r1, #0]
    vdup.16       q15, r0
    // tmp0 = MULTIPLY(z1, - FIX_0_720959822);  (q9-10)
    vmull.s16     q10, d19, d31
    vld1.16       {q14}, [r1]          // Read row 5 from quant table
    vmull.s16     q9, d18, d30

    // z1 = DEQUANTIZE(inptr[DCTSIZE*5], quantptr[DCTSIZE*5]);
    sub           r1, #VY(2)
    ldr           r0, =FIX_0_850430095
    vmul.i16      q1, q14              // Dequantizised row 5 in q9
    pld           [r1, #0]
    vdup.16       q15, r0
    // tmp0 += MULTIPLY(z1, FIX_0_850430095);
    vmlal.s16     q10, d3, d31
    vld1.16       {q14}, [r1]          // Read row 3 from quant table
    vmlal.s16     q9, d2, d30

    // z1 = DEQUANTIZE(inptr[DCTSIZE*3], quantptr[DCTSIZE*3]);
    sub           r1, #VY(2)
    ldr           r0, =-FIX_1_272758580
    vmul.i16      q8, q14              // Dequantizised row 3 in q8
    pld           [r1, #0]
    vdup.16       q15, r0
    // tmp0 += MULTIPLY(z1, - FIX_1_272758580);
    vmlal.s16     q10, d17, d31
    vld1.16       {q14}, [r1]          // Read row 1 from quant table
    vmlal.s16     q9, d16, d30

    // z1 = DEQUANTIZE(inptr[DCTSIZE*1], quantptr[DCTSIZE*1]);
    ldr           r0, =FIX_3_624509785
    vmul.i16      q13, q14             // Dequantizised row 1 in q13
    vdup.16       q15, r0
    // tmp0 += MULTIPLY(z1, FIX_3_624509785);
    vmlal.s16     q10, d27, d31
    vmlal.s16     q9, d26, d30

    /* Final calculations for column pass */
    // wsptr[DCTSIZE*0] = (int) DESCALE(tmp10 + tmp0, CONST_BITS-PASS1_BITS+2);  (q0)
    vadd.i32      q0, q2, q9
    vrshrn.i32    d0, q0, #(CONST_BITS - PASS1_BITS + 2)
    vadd.i32      q1, q3, q10
    vrshrn.i32    d1, q1, #(CONST_BITS - PASS1_BITS + 2)

    // wsptr[DCTSIZE*1] = (int) DESCALE(tmp10 - tmp0, CONST_BITS-PASS1_BITS+2);  (q1)
    vsub.i32      q2, q9
    vrshrn.i32    d2, q2, #(CONST_BITS - PASS1_BITS + 2)
    vsub.i32      q3, q10
    vrshrn.i32    d3, q3, #(CONST_BITS - PASS1_BITS + 2)

.LVTail2x2:
    /* Pass 2: process 2 rows, store into output array. */

    // After the interleave and transpose, q0 will contain element from offsets 0 and 8.
    // q1 will contain element offsets 1/9, 5/13, 3/11, and 7/15
    vld1.16       {q3}, [r2]           // Load multiplication constants for tmp0 later
    vzip.16       q0, q1
    ldr           r0, [sp, #0]         // output_col array
    vtrn.32       q0, q1
    pld           [r3, #0]

    /* Even part */

    // tmp10 = ((INT32) wsptr[0]) << (CONST_BITS+2);  (d0)
    vshll.s16     q0, d0, #(CONST_BITS + 2)

    /* Odd part */

    // tmp0 = MULTIPLY((INT32) wsptr[7], - FIX_0_720959822)  (d4)
    // + MULTIPLY((INT32) wsptr[5], FIX_0_850430095)
    // + MULTIPLY((INT32) wsptr[3], - FIX_1_272758580)
    // + MULTIPLY((INT32) wsptr[1], FIX_3_624509785);
    vmull.s16     q2, d2, d6
    vmov.i16      d16, #4096
    vmlal.s16     q2, d3, d7
    vadd.i32      d4, d5

    /* Final output stage: scale down and range-limit */

    // output = *output_buf++ + output_col
    // outptr[0] = range_limit[(int) DESCALE(tmp10 + tmp0, CONST_BITS+PASS1_BITS+3+2) & RANGE_MASK];
    // outptr[1] = range_limit[(int) DESCALE(tmp10 - tmp0, CONST_BITS+PASS1_BITS+3+2) & RANGE_MASK];
    ldr           r1, [r3], #4
    vsub.i32      d1, d0, d4
    add           r1, r0
    vadd.i32      d0, d4
    vtrn.32       d0, d1
    ldr           r2, [r3]
    vrshrn.i32    d0, q0, #(CONST_BITS + PASS1_BITS)
    vadd.i16      d0, d16
    add           r2, r0
    vqrshrun.s16  d0, q0, #(3 + 2)

    // d0 now contains 4x8-bit element from offsets:
    // 0 + 1+3+5+7, 0 - 1+3+5+7, 8 + 9+11+13+15, and 8 - 9+11+13+15
    vst1.16       {d0[0]}, [r1]
    vst1.16       {d0[1]}, [r2]
    bx            lr

.LVHeadZero2x2:
    /* AC terms all zero */
    vshl.i16      q0, #PASS1_BITS
    vmov.16       q1, q0
    b             .LVTail2x2

    .endfunc

    .align
.LConst2x2:
    .hword FIX_3_624509785, FIX_3_624509785, FIX_0_850430095, FIX_0_850430095
    .hword -FIX_1_272758580, -FIX_1_272758580, -FIX_0_720959822, -FIX_0_720959822


/*
 * Perform dequantization and inverse DCT on one block of coefficients,
 * producing a reduced-size 1x1 output block.
 */
    .text
    .fpu    neon
    .align

    .global jpeg_idct_1x1
    .func   jpeg_idct_1x1

jpeg_idct_1x1:
    ldr           r1, [r1, #off_COMPINFO_quanttable]         // quanttable

    // dcval = DEQUANTIZE(coef_block[0], quantptr[0]);  (r0)
    ldrsh         r0, [r2]
    ldrsh         r2, [r1]
    mov           r1, #128
    ldr           r3, [r3]
#if __ARM_HAVE_HALFWORD_MULTIPLY
    smulbb        r0, r0, r2
#else
    mul           r0, r0, r2
#endif
    //   dcval = (int) DESCALE((INT32) dcval, 3);  (r0)
    add           r0, #4
    ldr           r2, [sp, #0]                               // output_col array
    add           r0, r1, r0, asr #3
    //   output_buf[0][output_col] = range_limit[dcval & RANGE_MASK];
#if __ARM_ARCH__ >= 6
    usat          r0, #8, r0
#else
    cmp           r0, #255
    mvnhi         r0, r0, asr #31
    andhi         r0, #255
#endif
    strb          r0, [r3, r2]

    bx            lr

    .endfunc
