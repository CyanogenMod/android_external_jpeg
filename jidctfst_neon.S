/*
 * Copyright (C) ST-Ericsson SA 2010
 *
 * Neon optimized dequantization and inverse DCT.
 */

#define off_COMPINFO_quanttable 80
#define DCTSIZE                 8
#define VY(x)                   ((x)*DCTSIZE*2)

    .text
    .fpu    neon
    .align

    .global jpeg_idct_ifast
    .func   jpeg_idct_ifast

jpeg_idct_ifast:
    pld           [r2, #0]
    ldr           r1, [r1, #off_COMPINFO_quanttable]         // quanttable
    pld           [r1, #0]
    ldr           r0, [sp, #0]                               // output_col array
    stmdb         sp!, {r4, r5}

    /* Pass 1: process all columns from input */

    /* Even part */

    /* Usually many of the input coefficients are zero, especially the
     * AC terms. If all column's AC terms are zero, we short-circuiting
     * the IDCT calculation.
     */

    // tmp0 = DEQUANTIZE(in[DCTSIZE*0], quant[DCTSIZE*0]   (q13)
    pld           [r2, #32]
    vld1.16       {q13}, [r2]!         // Read row 0 from coef_block
    vld1.16       {q0}, [r2]!          // Read row 1
    vld1.16       {q14}, [r1]          // Read row 0 from quant table
    pld           [r2, #32]
    vld1.16       {q8}, [r2]!          // Read row 2
    add           r1, #VY(4)
    vld1.16       {q1}, [r2]!          // Read row 3
    vorr          q11, q0, q8
    pld           [r2, #32]
    vld1.16       {q9}, [r2]!          // Read row 4
    vld1.16       {q2}, [r2]!          // Read row 5
    vorr          q12, q1, q9
    vld1.16       {q10}, [r2]!         // Read row 6
    vorr          q11, q12
    vld1.16       {q3}, [r2]           // Read row 7
    pld           [r1, #0]
    vorr          q12, q2, q10
    vmul.i16      q13, q14             // Dequantizised row 0 in Q13
    vorr          q11, q12
    vorr          q11, q3
    vorr          d22, d23
    vmov          r4, r5, d22
    orrs          r4, r5
    beq           .LVHeadZero

    // tmp2 = DEQUANTIZE(in[DCTSIZE*4], quant[DCTSIZE*4]   (q9)
    vld1.16       {q14}, [r1]      // Read row 4 from quant table
    sub           r1, #VY(2)
    pld           [r1, #0]
    vmul.i16      q9, q14              // Dequantizised row 4 in Q9

    // tmp10 = tmp0 + tmp2   (q12)
    vadd.i16      q12, q13, q9
    // tmp11 = tmp0 - tmp2   (q11)
    vsub.i16      q11, q13, q9

    // tmp1 = DEQUANTIZE(in[DCTSIZE*2], quant[DCTSIZE*2]   (q8)
    vld1.16       {q14}, [r1]      // Read row 2 from quant table
    add           r1, #VY(4)
    pld           [r1, #0]
    vmul.i16      q8, q14              // Dequantizised row 2 in Q8

    // tmp3 = DEQUANTIZE(in[DCTSIZE*6], quant[DCTSIZE*6]   (q10)
    vld1.16       {q15}, [r1]      // Read row 6 from quant table
    sub           r1, #VY(3)
    pld           [r1, #0]
    vmul.i16      q10, q15             // Dequantizised row 6 in Q10

    // tmp13 = tmp1 + tmp3                                       (q13)
    mov           r4, #360
    vadd.i16      q13, q8, q10
    // tmp12 = MULTIPLY(tmp1 - tmp3, FIX_1_414213562) - tmp13    (q9)
    add           r4, #2
    vdup.16       d31, r4              // FIX(1.414213562) = 362 = 45*8 + 2
    vsub.i16      q8, q10
    vmull.s16     q10, d16, d31
    vrshrn.i32    d18, q10, #8
    vmull.s16     q14, d17, d31
    vrshrn.i32    d19, q14, #8
    vsub.i16      q9, q13

    // tmp1 = tmp11 + tmp12;   (q8)
    vadd.i16      q8, q11, q9
    // tmp2 = tmp11 - tmp12;   (q11)
    vsub.i16      q11, q9
    // tmp0 = tmp10 + tmp13;   (q9)
    vadd.i16      q9, q12, q13
    // tmp3 = tmp10 - tmp13;   (q12)
    vsub.i16      q12, q13

    /* Odd part */

    // tmp5 = DEQUANTIZE( in[DCTSIZE*3], quant[DCTSIZE*3] )   (q1)
    vld1.16       {q14}, [r1]      // Read row 3 from quant table
    add           r1, #VY(2)
    pld           [r1, #0]
    vmul.i16      q1, q14              // Dequantizised row 3 in Q1

    // tmp6 = DEQUANTIZE( in[DCTSIZE*5], quant[DCTSIZE*5] )   (q2)
    vld1.16       {q15}, [r1]      // Read row 5 from quant table
    sub           r1, #VY(4)
    pld           [r1, #0]
    vmul.i16      q2, q15              // Dequantizised row 5 in Q2

    // z13 = tmp6 + tmp5;  (q10)
    vadd.i16      q10, q2, q1
    // z10 = tmp6 - tmp5;  (q2)
    vsub.i16      q2, q1

    // tmp4 = DEQUANTIZE( in[DCTSIZE*1], quant[DCTSIZE*1] )   (q0)
    vld1.16       {q14}, [r1]      // Read row 1 from quant table
    add           r1, #VY(6)
    pld           [r1, #0]
    vmul.i16      q0, q14              // Dequantizised row 1 in Q0

    // tmp7 = DEQUANTIZE( in[DCTSIZE*7], quant[DCTSIZE*7] )   (q3)
    vld1.16       {q15}, [r1]      // Read row 7 from quant table
    vmul.i16      q3, q15              // Dequantizised row 7 in Q3

    // z11 = tmp4 + tmp7;  (q1)
    vadd.i16      q1, q0, q3
    // z12 = tmp4 - tmp7;  (q0)
    vsub.i16      q0, q3

    // tmp7 = z11 + z13;                             (q3)
    vadd.i16      q3, q1, q10
    // tmp11 = MULTIPLY(z11 - z13, FIX_1_414213562); (q10)
    vdup.16       d31, r4              // FIX(1.414213562) = 362 = 45*8 + 2
    vsub.i16      q1, q10
    vmull.s16     q13, d2, d31
    vrshrn.i32    d20, q13, #8
    mov           r5, #472
    vmull.s16     q14, d3, d31
    add           r5, #1
    vrshrn.i32    d21, q14, #8

    // z5 = MULTIPLY(z10 + z12, FIX_1_847759065);        (q13)
    vdup.16       d31, r5              // FIX(1.847759065) = 473 = 59*8 + 1
    vadd.i16      q1, q2, q0
    vmull.s16     q14, d2, d31
    mov           r5, #276
    vrshrn.i32    d26, q14, #8
    add           r5, #1
    vmull.s16     q14, d3, d31
    vdup.16       d30, r5              // FIX(1.082392200) = 277 = 276 + 1
    vrshrn.i32    d27, q14, #8

    // tmp10 = MULTIPLY(z12, FIX_1_082392200) - z5;      (q1)
    vmull.s16     q14, d0, d30
    vrshrn.i32    d2, q14, #8
    mvn           r5, #668
    vmull.s16     q0, d1, d30
    sub           r5, #1
    vrshrn.i32    d3, q0, #8
    vdup.16       d31, r5              // -FIX(2.613125930) = -669 = -668 - 1
    vsub.i16      q1, q13

    // tmp12 = MULTIPLY(z10, - FIX_2_613125930) + z5;    (q0)
    vmull.s16     q14, d4, d31
    vrshrn.i32    d0, q14, #8
    vmull.s16     q2, d5, d31
    vrshrn.i32    d1, q2, #8
    vadd.i16      q0, q13

    // tmp6 = tmp12 - tmp7;  (q0)
    vsub.i16      q0, q3
    // tmp5 = tmp11 - tmp6;  (q10)
    vsub.i16      q10, q0
    // tmp4 = tmp10 + tmp5;  (q1)
    vadd.i16      q1, q10

    /* Final calculations for column pass, and rotation for pass 2 */

    // wsptr[DCTSIZE*0] = (int) (tmp0 + tmp7);     (q15)
    vadd.i16      q15, q9, q3
    // wsptr[DCTSIZE*4] = (int) (tmp3 + tmp4);     (q14)
    vadd.i16      q14, q12, q1
    // wsptr[DCTSIZE*7] = (int) (tmp0 - tmp7);     (q9)
    vsub.i16      q9, q3
    vswp          d31, d28
    // wsptr[DCTSIZE*3] = (int) (tmp3 - tmp4);     (q12)
    vsub.i16      q12, q1
    // wsptr[DCTSIZE*2] = (int) (tmp2 + tmp5);     (q13)
    vadd.i16      q13, q11, q10
    vswp          d25, d18
    // wsptr[DCTSIZE*5] = (int) (tmp2 - tmp5);     (q11)
    vsub.i16      q11, q10
    // wsptr[DCTSIZE*1] = (int) (tmp1 + tmp6);     (q10)
    vadd.i16      q10, q8, q0
    // wsptr[DCTSIZE*6] = (int) (tmp1 - tmp6);     (q8)
    vsub.i16      q8, q0
    vswp          d21, d22
    vswp          d27, d16

    vtrn.32       q15, q13
    vtrn.32       q10, q12

    vtrn.32       q14, q8
    vtrn.32       q11, q9

    vtrn.16       q15, q10
    vtrn.16       q14, q11
    vtrn.16       q13, q12
    vtrn.16       q8, q9

.LVTail:
    /* Pass 2: process rows, store into output array. */

    /* Even part */

    // tmp10 = ((DCTELEM) wsptr[0] + (DCTELEM) wsptr[4]);    (q3)
    vadd.i16      q3, q15, q14
    // tmp11 = ((DCTELEM) wsptr[0] - (DCTELEM) wsptr[4]);    (q15)
    vsub.i16      q15, q14

    // tmp13 = ((DCTELEM) wsptr[2] + (DCTELEM) wsptr[6]);    (q2)
    vadd.i16      q2, q13, q8
    // tmp12 = MULTIPLY((DCTELEM) wsptr[2] - (DCTELEM) wsptr[6], FIX_1_414213562) - tmp13;  (q13)
    vdup.16       d0, r4              // FIX(1.414213562) = 362 = 45*8 + 2
    vsub.i16      q13, q8
    vmull.s16     q14, d26, d0
    vrshrn.i32    d26, q14, #8
    vmull.s16     q1, d27, d0
    vrshrn.i32    d27, q1, #8
    vsub.i16      q13, q2

    // tmp0 = tmp10 + tmp13;   (q0)
    vadd.i16      q0, q3, q2
    // tmp3 = tmp10 - tmp13;   (q3)
    vsub.i16      q3, q2
    // tmp1 = tmp11 + tmp12;   (q1)
    vadd.i16      q1, q15, q13
    // tmp2 = tmp11 - tmp12;   (q2)
    vsub.i16      q2, q15, q13

    /* Odd part */

    // z13 = (DCTELEM) wsptr[5] + (DCTELEM) wsptr[3];  (q8)
    vadd.i16      q8, q11, q12
    // z10 = (DCTELEM) wsptr[5] - (DCTELEM) wsptr[3];  (q11)
    vsub.i16      q11, q12
    // z11 = (DCTELEM) wsptr[1] + (DCTELEM) wsptr[7];  (q13)
    vadd.i16      q13, q10, q9
    // z12 = (DCTELEM) wsptr[1] - (DCTELEM) wsptr[7];  (q10)
    vsub.i16      q10, q9

    // tmp7 = z11 + z13;                             (q9)
    vadd.i16      q9, q13, q8
    // tmp11 = MULTIPLY(z11 - z13, FIX_1_414213562); (q13)
    vdup.16       d31, r4              // FIX(1.414213562) = 362 = 45*8 + 2
    vsub.i16      q13, q8
    vmull.s16     q8, d26, d31
    vrshrn.i32    d26, q8, #8
    mov           r5, #472
    vmull.s16     q12, d27, d31
    add           r5, #1
    vrshrn.i32    d27, q12, #8

    // z5 = MULTIPLY(z10 + z12, FIX_1_847759065);        (q12)
    vdup.16       d31, r5              // FIX(1.847759065) = 473 = 59*8 + 1
    vadd.i16      q8, q11, q10
    vmull.s16     q14, d16, d31
    mov           r5, #276
    vrshrn.i32    d24, q14, #8
    add           r5, #1
    vmull.s16     q14, d17, d31
    vdup.16       d30, r5              // FIX(1.082392200) = 277 = 276 + 1
    vrshrn.i32    d25, q14, #8

    // tmp10 = MULTIPLY(z12, FIX_1_082392200) - z5;      (q14)
    vmull.s16     q8, d20, d30
    vrshrn.i32    d28, q8, #8
    mvn           r5, #668
    vmull.s16     q8, d21, d30
    sub           r5, #1
    vrshrn.i32    d29, q8, #8
    vdup.16       d31, r5              // FIX(2.613125930) = 669 = 668 + 1
    vsub.i16      q14, q12

    // tmp12 = MULTIPLY(z10, - FIX_2_613125930) + z5;    (q8)
    vmull.s16     q10, d22, d31
    vrshrn.i32    d16, q10, #8
    vmull.s16     q15, d23, d31
    vrshrn.i32    d17, q15, #8
    vadd.i16      q8, q12

    // tmp6 = tmp12 - tmp7;  (q8)
    vsub.i16      q8, q9
    // tmp5 = tmp11 - tmp6;  (q13)
    vsub.i16      q13, q8
    // tmp4 = tmp10 + tmp5;  (q14)
    vmov.i16      q15, #4096
    vadd.i16      q14, q13

    /* Final output stage: scale down and range-limit */
    pld           [r3, #0]

    // outptr[0] = range_limit[IDESCALE(tmp0 + tmp7, PASS1_BITS+3) & RANGE_MASK];     (d20)
    vadd.i16      q0, q15
    vadd.i16      q10, q0, q9
    vqrshrun.s16  d20, q10, #5
    // outptr[4] = range_limit[IDESCALE(tmp3 + tmp4, PASS1_BITS+3) & RANGE_MASK];     (d21)
    vadd.i16      q3, q15
    vadd.i16      q11, q3, q14
    vqrshrun.s16  d21, q11, #5
    // outptr[7] = range_limit[IDESCALE(tmp0 - tmp7, PASS1_BITS+3) & RANGE_MASK];     (d18)
    vsub.i16      q0, q9
    vtrn.32       d20, d21
    vqrshrun.s16  d18, q0, #5
    // outptr[3] = range_limit[IDESCALE(tmp3 - tmp4, PASS1_BITS+3) & RANGE_MASK];     (d19)
    vsub.i16      q3, q14
    vqrshrun.s16  d19, q3, #5
    // outptr[2] = range_limit[IDESCALE(tmp2 + tmp5, PASS1_BITS+3) & RANGE_MASK];     (d22)
    vadd.i16      q2, q15
    vtrn.32       d19, d18
    vadd.i16      q12, q2, q13
    vqrshrun.s16  d22, q12, #5
    // outptr[5] = range_limit[IDESCALE(tmp2 - tmp5, PASS1_BITS+3) & RANGE_MASK];     (d23)
    vsub.i16      q2, q13
    vqrshrun.s16  d23, q2, #5
    // outptr[1] = range_limit[IDESCALE(tmp1 + tmp6, PASS1_BITS+3) & RANGE_MASK];     (d24)
    vadd.i16      q1, q15
    vadd.i16      q14, q1, q8
    vqrshrun.s16  d24, q14, #5
    // outptr[6] = range_limit[IDESCALE(tmp1 - tmp6, PASS1_BITS+3) & RANGE_MASK];     (d25)
    vsub.i16      q1, q8
    vtrn.32       d24, d23
    vqrshrun.s16  d25, q1, #5
    ldr           r4, [r3], #4
    vtrn.32       d22, d25

    // output = *output_buf++ + output_col
    add           r4, r0
    vtrn.16       d20, d22
    ldr           r5, [r3], #4
    vtrn.16       d24, d19
    add           r5, r0
    vtrn.8        d20, d24
    vst1.8        {d20}, [r4, :64]
    ldr           r4, [r3], #4
    vtrn.8        d22, d19
    add           r4, r0
    vst1.8        {d24}, [r5, :64]
    ldr           r5, [r3], #4
    vtrn.16       d21, d25
    add           r5, r0
    vst1.8        {d22}, [r4, :64]
    ldr           r4, [r3], #4
    vtrn.16       d23, d18
    add           r4, r0
    vst1.8        {d19}, [r5, :64]
    ldr           r5, [r3], #4
    vtrn.8        d21, d23
    add           r5, r0
    vst1.8        {d21}, [r4, :64]
    ldr           r4, [r3], #4
    vtrn.8        d25, d18
    add           r4, r0
    vst1.8        {d23}, [r5, :64]
    ldr           r5, [r3], #4
    vst1.8        {d25}, [r4, :64]
    add           r5, r0
    vst1.8        {d18}, [r5, :64]

.LExit:
    ldmia         sp!, {r4, r5}
    bx            lr

.LVHeadZero:
    /* AC terms all zero */
    vdup.16       q8, d27[2]     // (6)
    vdup.16       q9, d27[3]     // (7)
    vdup.16       q10, d26[1]    // (1)
    vdup.16       q11, d27[1]    // (5)
    mov           r4, #360       // FIX(1.414213562) = 362 = 45*8 + 2
    vdup.16       q12, d26[3]    // (3)
    add           r4, #2
    vdup.16       q14, d27[0]    // (4)
    vdup.16       q15, d26[0]    // (0)
    vdup.16       q13, d26[2]    // (2)
    b             .LVTail

    .endfunc
