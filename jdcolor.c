/*
 * jdcolor.c
 *
 * Copyright (C) 1991-1997, Thomas G. Lane.
 * This file is part of the Independent JPEG Group's software.
 * For conditions of distribution and use, see the accompanying README file.
 *
 * Copyright (C) ST-Ericsson SA 2010
 *
 * Added neon optimized color conversion for often used Android color formats.
 *
 * Author: Henrik Smiding henrik.smiding@stericsson.com for
 * ST-Ericsson.
 *
 * This file contains output colorspace conversion routines.
 */

#define JPEG_INTERNALS
#include "jinclude.h"
#include "jpeglib.h"
#ifdef NV_ARM_NEON
#include "jsimd_neon.h"
#endif

/* Private subobject */

typedef struct {
  struct jpeg_color_deconverter pub; /* public fields */

  /* Private state for YCC->RGB conversion */
  int * Cr_r_tab;		/* => table for Cr to R conversion */
  int * Cb_b_tab;		/* => table for Cb to B conversion */
  INT32 * Cr_g_tab;		/* => table for Cr to G conversion */
  INT32 * Cb_g_tab;		/* => table for Cb to G conversion */
} my_color_deconverter;

typedef my_color_deconverter * my_cconvert_ptr;


#ifdef ANDROID_RGB

/* Declarations for ordered dithering.
 * 
 * We use 4x4 ordered dither array packed into 32 bits. This array is
 * sufficent for dithering RGB_888 to RGB_565.
 */

#define DITHER_MASK         0x3
#define DITHER_ROTATE(x)    (((x)<<24) | (((x)>>8)&0x00FFFFFF))
static const INT32 dither_matrix[4] = {
  0x0008020A,
  0x0C040E06,
  0x030B0109,
  0x0F070D05
};

#if defined(__ARM_HAVE_NEON)
#include <arm_neon.h>

#if BITS_IN_JSAMPLE == 8
#define ENABLE_NEON_YCC_RGBA_8888
#define ENABLE_NEON_YCC_RGB_565
#define ENABLE_NEON_YCC_RGB_565D

LOCAL(void)
clear_ycc_rgb_table(j_decompress_ptr cinfo)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr)cinfo->cconvert;

  cconvert->Cr_r_tab = NULL;
  cconvert->Cb_b_tab = NULL;
  cconvert->Cr_g_tab = NULL;
  cconvert->Cb_g_tab = NULL;
}
#endif
#endif
#endif


/**************** YCbCr -> RGB conversion: most common case **************/

/*
 * YCbCr is defined per CCIR 601-1, except that Cb and Cr are
 * normalized to the range 0..MAXJSAMPLE rather than -0.5 .. 0.5.
 * The conversion equations to be implemented are therefore
 *	R = Y                + 1.40200 * Cr
 *	G = Y - 0.34414 * Cb - 0.71414 * Cr
 *	B = Y + 1.77200 * Cb
 * where Cb and Cr represent the incoming values less CENTERJSAMPLE.
 * (These numbers are derived from TIFF 6.0 section 21, dated 3-June-92.)
 *
 * To avoid floating-point arithmetic, we represent the fractional constants
 * as integers scaled up by 2^16 (about 4 digits precision); we have to divide
 * the products by 2^16, with appropriate rounding, to get the correct answer.
 * Notice that Y, being an integral input, does not contribute any fraction
 * so it need not participate in the rounding.
 *
 * For even more speed, we avoid doing any multiplications in the inner loop
 * by precalculating the constants times Cb and Cr for all possible values.
 * For 8-bit JSAMPLEs this is very reasonable (only 256 entries per table);
 * for 12-bit samples it is still acceptable.  It's not very reasonable for
 * 16-bit samples, but if you want lossless storage you shouldn't be changing
 * colorspace anyway.
 * The Cr=>R and Cb=>B values can be rounded to integers in advance; the
 * values for the G calculation are left scaled up, since we must add them
 * together before rounding.
 */

#define SCALEBITS	16	/* speediest right-shift on some machines */
#define ONE_HALF	((INT32) 1 << (SCALEBITS-1))
#define FIX(x)		((INT32) ((x) * (1L<<SCALEBITS) + 0.5))


/*
 * Initialize tables for YCC->RGB colorspace conversion.
 */

LOCAL(void)
build_ycc_rgb_table (j_decompress_ptr cinfo)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  int i;
  INT32 x;
  SHIFT_TEMPS

  cconvert->Cr_r_tab = (int *)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int));
  cconvert->Cb_b_tab = (int *)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(int));
  cconvert->Cr_g_tab = (INT32 *)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32));
  cconvert->Cb_g_tab = (INT32 *)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
                                (MAXJSAMPLE+1) * SIZEOF(INT32));

  for (i = 0, x = -CENTERJSAMPLE; i <= MAXJSAMPLE; i++, x++) {
    /* i is the actual input pixel value, in the range 0..MAXJSAMPLE */
    /* The Cb or Cr value we are thinking of is x = i - CENTERJSAMPLE */
    /* Cr=>R value is nearest int to 1.40200 * x */
    cconvert->Cr_r_tab[i] = (int)
                    RIGHT_SHIFT(FIX(1.40200) * x + ONE_HALF, SCALEBITS);
    /* Cb=>B value is nearest int to 1.77200 * x */
    cconvert->Cb_b_tab[i] = (int)
                    RIGHT_SHIFT(FIX(1.77200) * x + ONE_HALF, SCALEBITS);
    /* Cr=>G value is scaled-up -0.71414 * x */
    cconvert->Cr_g_tab[i] = (- FIX(0.71414)) * x;
    /* Cb=>G value is scaled-up -0.34414 * x */
    /* We also add in ONE_HALF so that need not do it in inner loop */
    cconvert->Cb_g_tab[i] = (- FIX(0.34414)) * x + ONE_HALF;
  }
}

/*
 * Convert some rows of samples to the output colorspace.
 *
 * Note that we change from noninterleaved, one-plane-per-component format
 * to interleaved-pixel format.  The output buffer is therefore three times
 * as wide as the input buffer.
 * A starting row offset is provided only for the input buffer.  The caller
 * can easily adjust the passed output_buf value to accommodate any row
 * offset required on that side.
 */

METHODDEF(void)
ycc_rgb_convert (j_decompress_ptr cinfo,
		 JSAMPIMAGE input_buf, JDIMENSION input_row,
		 JSAMPARRAY output_buf, int num_rows)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  register int y, cb, cr;
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  /* copy these pointers into registers if possible */
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  register int * Crrtab = cconvert->Cr_r_tab;
  register int * Cbbtab = cconvert->Cb_b_tab;
  register INT32 * Crgtab = cconvert->Cr_g_tab;
  register INT32 * Cbgtab = cconvert->Cb_g_tab;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      y  = GETJSAMPLE(inptr0[col]);
      cb = GETJSAMPLE(inptr1[col]);
      cr = GETJSAMPLE(inptr2[col]);
      /* Range-limiting is essential due to noise introduced by DCT losses. */
      outptr[RGB_RED] =   range_limit[y + Crrtab[cr]];
      outptr[RGB_GREEN] = range_limit[y +
                              ((int) RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr],
                                                 SCALEBITS))];
      outptr[RGB_BLUE] =  range_limit[y + Cbbtab[cb]];
      outptr += RGB_PIXELSIZE;
    }
  }
}

#ifdef ANDROID_RGB
#define YCC_RGBA_8888_Proc ycc_rgba_8888_convert
#define YCC_RGB_565D_Proc  ycc_rgb_565D_convert
#define YCC_RGB_565_Proc   ycc_rgb_565_convert

METHODDEF(void)
ycc_rgba_8888_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  register int y, cb, cr;
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  /* copy these pointers into registers if possible */
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  register int * Crrtab = cconvert->Cr_r_tab;
  register int * Cbbtab = cconvert->Cb_b_tab;
  register INT32 * Crgtab = cconvert->Cr_g_tab;
  register INT32 * Cbgtab = cconvert->Cb_g_tab;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      y  = GETJSAMPLE(inptr0[col]);
      cb = GETJSAMPLE(inptr1[col]);
      cr = GETJSAMPLE(inptr2[col]);
      /* Range-limiting is essential due to noise introduced by DCT losses. */
      outptr[RGB_RED] =   range_limit[y + Crrtab[cr]];
      outptr[RGB_GREEN] = range_limit[y +
                              ((int) RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr],
                                                 SCALEBITS))];
      outptr[RGB_BLUE] =  range_limit[y + Cbbtab[cb]];
      outptr[RGB_ALPHA] =  0xFF;
      outptr += 4;
    }
  }
}

METHODDEF(void)
ycc_rgb_565_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  register int y, cb, cr;
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  /* copy these pointers into registers if possible */
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  register int * Crrtab = cconvert->Cr_r_tab;
  register int * Cbbtab = cconvert->Cb_b_tab;
  register INT32 * Crgtab = cconvert->Cr_g_tab;
  register INT32 * Cbgtab = cconvert->Cb_g_tab;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int r, g, b;
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    
    if (PACK_NEED_ALIGNMENT(outptr)) {
        y  = GETJSAMPLE(*inptr0++);
        cb = GETJSAMPLE(*inptr1++);
        cr = GETJSAMPLE(*inptr2++);
        r = range_limit[y + Crrtab[cr]];
        g = range_limit[y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS))];
        b = range_limit[y + Cbbtab[cb]];
        rgb = PACK_SHORT_565(r,g,b);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      y  = GETJSAMPLE(*inptr0++);
      cb = GETJSAMPLE(*inptr1++);
      cr = GETJSAMPLE(*inptr2++);
      r = range_limit[y + Crrtab[cr]];
      g = range_limit[y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS))];
      b = range_limit[y + Cbbtab[cb]];
      rgb = PACK_SHORT_565(r,g,b);

      y  = GETJSAMPLE(*inptr0++);
      cb = GETJSAMPLE(*inptr1++);
      cr = GETJSAMPLE(*inptr2++);
      r = range_limit[y + Crrtab[cr]];
      g = range_limit[y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS))];
      b = range_limit[y + Cbbtab[cb]];
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(r,g,b));
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      y  = GETJSAMPLE(*inptr0);
      cb = GETJSAMPLE(*inptr1);
      cr = GETJSAMPLE(*inptr2);
      r = range_limit[y + Crrtab[cr]];
      g = range_limit[y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS))];
      b = range_limit[y + Cbbtab[cb]];
      rgb = PACK_SHORT_565(r,g,b);
      *(INT16*)outptr = rgb;
    }
  }
}

METHODDEF(void)
ycc_rgb_565D_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  register int y, cb, cr;
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  /* copy these pointers into registers if possible */
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  register int * Crrtab = cconvert->Cr_r_tab;
  register int * Cbbtab = cconvert->Cb_b_tab;
  register INT32 * Crgtab = cconvert->Cr_g_tab;
  register INT32 * Cbgtab = cconvert->Cb_g_tab;
  INT32 d0 = dither_matrix[cinfo->output_scanline & DITHER_MASK];
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int r, g, b;
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    if (PACK_NEED_ALIGNMENT(outptr)) {
        y  = GETJSAMPLE(*inptr0++);
        cb = GETJSAMPLE(*inptr1++);
        cr = GETJSAMPLE(*inptr2++);
        r = range_limit[DITHER_565_R(y + Crrtab[cr], d0)];
        g = range_limit[DITHER_565_G(y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS)), d0)];
        b = range_limit[DITHER_565_B(y + Cbbtab[cb], d0)];
        rgb = PACK_SHORT_565(r,g,b);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      y  = GETJSAMPLE(*inptr0++);
      cb = GETJSAMPLE(*inptr1++);
      cr = GETJSAMPLE(*inptr2++);
      r = range_limit[DITHER_565_R(y + Crrtab[cr], d0)];
      g = range_limit[DITHER_565_G(y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS)), d0)];
      b = range_limit[DITHER_565_B(y + Cbbtab[cb], d0)];
      d0 = DITHER_ROTATE(d0);
      rgb = PACK_SHORT_565(r,g,b);
      y  = GETJSAMPLE(*inptr0++);
      cb = GETJSAMPLE(*inptr1++);
      cr = GETJSAMPLE(*inptr2++);
      r = range_limit[DITHER_565_R(y + Crrtab[cr], d0)];
      g = range_limit[DITHER_565_G(y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS)), d0)];
      b = range_limit[DITHER_565_B(y + Cbbtab[cb], d0)];
      d0 = DITHER_ROTATE(d0);
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(r,g,b));
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      y  = GETJSAMPLE(*inptr0);
      cb = GETJSAMPLE(*inptr1);
      cr = GETJSAMPLE(*inptr2);
      r = range_limit[DITHER_565_R(y + Crrtab[cr], d0)];
      g = range_limit[DITHER_565_G(y + ((int)RIGHT_SHIFT(Cbgtab[cb]+Crgtab[cr], SCALEBITS)), d0)];
      b = range_limit[DITHER_565_B(y + Cbbtab[cb], d0)];
      rgb = PACK_SHORT_565(r,g,b);
      *(INT16*)outptr = rgb;
    }
  }
}


// Use neon optimized version?
#if defined(ENABLE_NEON_YCC_RGBA_8888)
#undef YCC_RGBA_8888_Proc
#define YCC_RGBA_8888_Proc ycc_rgba_8888_neon_convert

METHODDEF(void)
ycc_rgba_8888_neon_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  JDIMENSION num_cols = cinfo->output_width;

  // Fallback to non neon method for small conversions
  if (num_cols < 8)
  {
    my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;

    if (cconvert->Cr_r_tab == NULL) {
      build_ycc_rgb_table(cinfo);
    }
    ycc_rgba_8888_convert(cinfo, input_buf, input_row, output_buf, num_rows);
    return;
  }

  asm volatile (
                // Setup constants
                "vmov.u8        d28, #255                       \n\t"   // Set alpha to 0xFF
                "vmov.u8        d29, #128                       \n\t"   // Set center sample constant to CENTERJSAMPLE
                "ldr            r0, =91881                      \n\t"   // Load Cr_r constant
                "vdup.32        q0, r0                          \n\t"   //
                "ldr            r0, =-46802                     \n\t"   // Load Cr_g constant
                "vdup.32        q1, r0                          \n\t"   //
                "ldr            r0, =-22554                     \n\t"   // Load Cb_g constant
                "vdup.32        q2, r0                          \n\t"   //
                "ldr            r0, =116130                     \n\t"   // Load Cb_b constant
                "vdup.32        q3, r0                          \n\t"   //
                // Outer loop (rows)
                "1:                                             \n\t"   //
                "sub            %[num_rows], %[num_rows], #1    \n\t"   // decrement and check num_rows <= 0
                "cmp            %[num_rows], #0                 \n\t"   //
                "blt            3f                              \n\t"   //
                "ldr            r1,[%[input_buf], #0]           \n\t"   // Setup y input pointer from input_buf[0][input_row]
                "ldr            r1,[r1, %[input_row], lsl #2]   \n\t"   //
                "ldr            r2,[%[input_buf], #4]           \n\t"   // Setup cb input pointer from input_buf[1][input_row]
                "pld            [r1]                            \n\t"   //
                "ldr            r2,[r2, %[input_row], lsl #2]   \n\t"   //
                "ldr            r3,[%[input_buf], #8]           \n\t"   // Setup cr input pointer from input_buf[2][input_row]
                "pld            [r2]                            \n\t"   //
                "ldr            r3,[r3, %[input_row], lsl #2]   \n\t"   //
                "add            %[input_row], %[input_row], #1  \n\t"   // input_row++
                "pld            [r3]                            \n\t"   //
                "ldr            r4, [%[output_buf]], #4         \n\t"   // Get output pointer and increment
                "ands           r0, %[num_cols], #7             \n\t"   // Calculate first iteration increment
                "moveq          r0, #8                          \n\t"   // If columns are even eight, do full iteration
                "mov            r5, %[num_cols]                 \n\t"   // Setup loop counter
                // Inner loop (columns)
                "2:                                             \n\t"   //
                // Read values, subtract 128 from cb/cr, and expand to 32-bit
                "vld1.8         {d30}, [r2]                     \n\t"   // Load eight cb values
                "vld1.8         {d31}, [r3]                     \n\t"   // Load eight cr values
                "vsubl.u8       q8, d30, d29                    \n\t"   // Subtract CENTERJSAMPLE from cb and expand to 16-bit
                "vmovl.s16      q10, d16                        \n\t"   // Expand low cb values to 32-bit
                "vmovl.s16      q11, d17                        \n\t"   // Expand high cb values to 32-bit
                "add            r2, r2, r0                      \n\t"   // Increment input pointer for cb
                "vsubl.u8       q9, d31, d29                    \n\t"   // Subtract CENTERJSAMPLE from cr and expand to 16-bit
                "pld            [r2]                            \n\t"   //
                "vmovl.s16      q12, d18                        \n\t"   // Expand low cr values to 32-bit
                "vmovl.s16      q13, d19                        \n\t"   // Expand high cr values to 32-bit
                "add            r3, r3, r0                      \n\t"   // Increment input pointer for cr
                "vld1.8         {d30}, [r1]                     \n\t"   // Load eight y values
                "pld            [r3]                            \n\t"   //
                // Multiply with the constants. Split into RGB (Vector multiply)
                "vmul.i32       q8, q10, q2                     \n\t"   // Calculate green low/high
                "vmul.i32       q9, q11, q2                     \n\t"   //
                "vmla.i32       q8, q12, q1                     \n\t"   //
                "vmla.i32       q9, q13, q1                     \n\t"   //
                "vmul.i32       q10, q10, q3                    \n\t"   // Calculate blue low/high
                "vmul.i32       q11, q11, q3                    \n\t"   //
                "vmul.i32       q12, q12, q0                    \n\t"   // Calculate red low/high
                "vmul.i32       q13, q13, q0                    \n\t"   //
                "add            r1, r1, r0                      \n\t"   // Increment input pointer for y
                // Shift and combine RGB result (Vector rounding narrowing shift right by constant)
                "vrshrn.i32     d16, q8, #16                    \n\t"   // Shift green
                "pld            [r1]                            \n\t"   //
                "vrshrn.i32     d17, q9, #16                    \n\t"   //
                "vrshrn.i32     d20, q10, #16                   \n\t"   // Shift blue
                "vrshrn.i32     d21, q11, #16                   \n\t"   //
                "vrshrn.i32     d24, q12, #16                   \n\t"   // Shift red
                "vrshrn.i32     d25, q13, #16                   \n\t"   //
                // // Add y (Vector add)
                "vmovl.u8       q15, d30                        \n\t"   // Expand y to 16-bit
                "vadd.i16       q8, q15                         \n\t"   // Add y to green
                "vadd.i16       q10, q15                        \n\t"   // Add y to blue
                "vadd.i16       q12, q15                        \n\t"   // Add y to red
                "subs           r5, r5, r0                      \n\t"   // Decrement loop counter
                // Convert result from signed 16-bit to unsinged 8-bit with range limitation.
                // Range-limiting is essential due to noise introduced by DCT losses.
                // (Vector Saturating Move and Narrow, signed operand with Unsigned result)
                "vqmovun.s16    d26, q8                         \n\t"   // Convert green
                "vqmovun.s16    d27, q10                        \n\t"   // Convert blue
                "vqmovun.s16    d25, q12                        \n\t"   // Convert red
                "vst4.8         {d25, d26, d27, d28}, [r4]      \n\t"   // Write result to memory
                // Increase pointers and counters
                "add            r4, r4, r0, lsl #2              \n\t"   // Increment output buffer pointer
                "mov            r0, #8                          \n\t"   // Set next loop iteration length
                "bne            2b                              \n\t"   // If inner loop counter != 0, loop
                "b              1b                              \n\t"   // Outer loop
                "3:                                             \n\t"   //
                : [input_row] "+r" (input_row), [output_buf] "+r" (output_buf), [num_rows] "+r" (num_rows)
                : [input_buf] "r" (input_buf), [num_cols] "r" (num_cols)
                : "cc", "memory", "r0", "r1", "r2", "r3", "r4", "r5", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "d29", "d30", "d31"
                );
}
#endif


#if defined(ENABLE_NEON_YCC_RGB_565)
#undef YCC_RGB_565_Proc
#define YCC_RGB_565_Proc ycc_rgb_565_neon_convert

METHODDEF(void)
ycc_rgb_565_neon_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  JDIMENSION num_cols = cinfo->output_width;

  // Fallback to non neon method for small conversions
  if (num_cols < 8)
  {
    my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;

    if (cconvert->Cr_r_tab == NULL) {
      build_ycc_rgb_table(cinfo);
    }
    ycc_rgb_565_convert(cinfo, input_buf, input_row, output_buf, num_rows);
    return;
  }

  asm volatile (
                // Setup constants
                "vmov.u8        d28, #255                       \n\t"   // Set alpha to 0xFF
                "vmov.u8        d29, #128                       \n\t"   // Set center sample constant to CENTERJSAMPLE
                "ldr            r0, =91881                      \n\t"   // Load Cr_r constant
                "vdup.32        q0, r0                          \n\t"   //
                "ldr            r0, =-46802                     \n\t"   // Load Cr_g constant
                "vdup.32        q1, r0                          \n\t"   //
                "ldr            r0, =-22554                     \n\t"   // Load Cb_g constant
                "vdup.32        q2, r0                          \n\t"   //
                "ldr            r0, =116130                     \n\t"   // Load Cb_b constant
                "vdup.32        q3, r0                          \n\t"   //
                // Outer loop (rows)
                "1:                                             \n\t"   //
                "sub            %[num_rows], %[num_rows], #1    \n\t"   // decrement and check num_rows <= 0
                "cmp            %[num_rows], #0                 \n\t"   //
                "blt            3f                              \n\t"   //
                "ldr            r1,[%[input_buf], #0]           \n\t"   // Setup y input pointer from input_buf[0][input_row]
                "ldr            r1,[r1, %[input_row], lsl #2]   \n\t"   //
                "ldr            r2,[%[input_buf], #4]           \n\t"   // Setup cb input pointer from input_buf[1][input_row]
                "pld            [r1]                            \n\t"   //
                "ldr            r2,[r2, %[input_row], lsl #2]   \n\t"   //
                "ldr            r3,[%[input_buf], #8]           \n\t"   // Setup cr input pointer from input_buf[2][input_row]
                "pld            [r2]                            \n\t"   //
                "ldr            r3,[r3, %[input_row], lsl #2]   \n\t"   //
                "add            %[input_row], %[input_row], #1  \n\t"   // input_row++
                "pld            [r3]                            \n\t"   //
                "ldr            r4, [%[output_buf]], #4         \n\t"   // Get output pointer and increment
                "ands           r0, %[num_cols], #7             \n\t"   // Calculate first iteration increment
                "moveq          r0, #8                          \n\t"   // If columns are even eight, do full iteration
                "mov            r5, %[num_cols]                 \n\t"   // Setup loop counter
                // Inner loop (columns)
                "2:                                             \n\t"   //
                // Read values, subtract 128 from cb/cr, and expand to 32-bit
                "vld1.8         {d30}, [r2]                     \n\t"   // Load eight cb values
                "vld1.8         {d31}, [r3]                     \n\t"   // Load eight cr values
                "vsubl.u8       q8, d30, d29                    \n\t"   // Subtract CENTERJSAMPLE from cb and expand to 16-bit
                "vmovl.s16      q10, d16                        \n\t"   // Expand low cb values to 32-bit
                "vmovl.s16      q11, d17                        \n\t"   // Expand high cb values to 32-bit
                "add            r2, r2, r0                      \n\t"   // Increment input pointer for cb
                "vsubl.u8       q9, d31, d29                    \n\t"   // Subtract CENTERJSAMPLE from cr and expand to 16-bit
                "pld            [r2]                            \n\t"   //
                "vmovl.s16      q12, d18                        \n\t"   // Expand low cr values to 32-bit
                "vmovl.s16      q13, d19                        \n\t"   // Expand high cr values to 32-bit
                "add            r3, r3, r0                      \n\t"   // Increment input pointer for cr
                "vld1.8         {d30}, [r1]                     \n\t"   // Load eight y values
                "pld            [r3]                            \n\t"   //
                // Multiply with the constants. Split into RGB (Vector multiply)
                "vmul.i32       q8, q10, q2                     \n\t"   // Calculate green low/high
                "vmul.i32       q9, q11, q2                     \n\t"   //
                "vmla.i32       q8, q12, q1                     \n\t"   //
                "vmla.i32       q9, q13, q1                     \n\t"   //
                "vmul.i32       q10, q10, q3                    \n\t"   // Calculate blue low/high
                "vmul.i32       q11, q11, q3                    \n\t"   //
                "vmul.i32       q12, q12, q0                    \n\t"   // Calculate red low/high
                "vmul.i32       q13, q13, q0                    \n\t"   //
                "add            r1, r1, r0                      \n\t"   // Increment input pointer for y
                // Shift and combine RGB result (Vector rounding narrowing shift right by constant)
                "vrshrn.i32     d16, q8, #16                    \n\t"   // Shift green
                "pld            [r1]                            \n\t"   //
                "vrshrn.i32     d17, q9, #16                    \n\t"   //
                "vrshrn.i32     d20, q10, #16                   \n\t"   // Shift blue
                "vrshrn.i32     d21, q11, #16                   \n\t"   //
                "vrshrn.i32     d24, q12, #16                   \n\t"   // Shift red
                "vrshrn.i32     d25, q13, #16                   \n\t"   //
                // // Add y (Vector add)
                "vmovl.u8       q15, d30                        \n\t"   // Expand y to 16-bit
                "vadd.i16       q8, q15                         \n\t"   // Add y to green
                "vadd.i16       q10, q15                        \n\t"   // Add y to blue
                "vadd.i16       q12, q15                        \n\t"   // Add y to red
                "subs           r5, r5, r0                      \n\t"   // Decrement loop counter
                // Convert result from signed 16-bit to unsinged 8-bit with range limitation.
                // Range-limiting is essential due to noise introduced by DCT losses.
                // (Vector Saturating Move and Narrow, signed operand with Unsigned result)
                "vqmovun.s16    d16, q8                         \n\t"   // Convert green
                "vqmovun.s16    d20, q10                        \n\t"   // Convert blue
                "vqmovun.s16    d24, q12                        \n\t"   // Convert red
                // Pack to 565 format
                "vshll.u8       q8, d16, #8                     \n\t"   // Shift green and expand to 16-bit
                "vshll.u8       q12, d24, #8                    \n\t"   // Shift red and expand to 16-bit
                "vshll.u8       q10, d20, #8                    \n\t"   // Shift blue and expand to 16-bit
                "vsri.u16       q12, q8, #5                     \n\t"   // Insert green into red
                "vsri.u16       q12, q10, #11                   \n\t"   // Insert blue into red
                "vst1.16        {q12}, [r4]                     \n\t"   // Write result to memory
                // Increase pointers and counters
                "add            r4, r4, r0, lsl #1              \n\t"   // Increment output buffer pointer
                "mov            r0, #8                          \n\t"   // Set next loop iteration length
                "bne            2b                              \n\t"   // If inner loop counter != 0, loop
                "b              1b                              \n\t"   // Outer loop
                "3:                                             \n\t"   //
                : [input_row] "+r" (input_row), [output_buf] "+r" (output_buf), [num_rows] "+r" (num_rows)
                : [input_buf] "r" (input_buf), [num_cols] "r" (num_cols)
                : "cc", "memory", "r0", "r1", "r2", "r3", "r4", "r5", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "d29", "d30", "d31"
                );
}
#endif


#if defined(ENABLE_NEON_YCC_RGB_565D)
#undef YCC_RGB_565D_Proc
#define YCC_RGB_565D_Proc ycc_rgb_565D_neon_convert

/* Declarations for ordered dithering for neon code.
 *
 * We use 4x4 ordered dither array, repeated three times to
 * allow an 8-byte load from offsets 0, 1, 2 or 3.
 * sufficent for dithering RGB_888 to RGB_565.
 */
static const uint8_t dither_matrix_neon[48] = {
  10,  2,  8,  0, 10,  2,  8,  0, 10,  2,  8,  0,
   6, 14,  4, 12,  6, 14,  4, 12,  6, 14,  4, 12,
   9,  1, 11,  3,  9,  1, 11,  3,  9,  1, 11,  3,
   5, 13,  7, 15,  5, 13,  7, 15,  5, 13,  7, 15
};

METHODDEF(void)
ycc_rgb_565D_neon_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  JDIMENSION num_cols = cinfo->output_width;
  const uint8_t* matrix = dither_matrix_neon + ((cinfo->output_scanline & DITHER_MASK) * 12);

  // Fallback to non neon method for small conversions
  if (num_cols < 8)
  {
    my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;

    if (cconvert->Cr_r_tab == NULL) {
      build_ycc_rgb_table(cinfo);
    }
    ycc_rgb_565D_convert(cinfo, input_buf, input_row, output_buf, num_rows);
    return;
  }

  asm volatile (
                // Setup constants
                "vmov.u8        d28, #128                       \n\t"   // Set center sample constant to CENTERJSAMPLE
                "ldr            r0, =91881                      \n\t"   // Load Cr_r constant
                "vdup.32        q0, r0                          \n\t"   //
                "ldr            r0, =-46802                     \n\t"   // Load Cr_g constant
                "vdup.32        q1, r0                          \n\t"   //
                "ldr            r0, =-22554                     \n\t"   // Load Cb_g constant
                "vdup.32        q2, r0                          \n\t"   //
                "ldr            r0, =116130                     \n\t"   // Load Cb_b constant
                "vdup.32        q3, r0                          \n\t"   //
                // Outer loop (rows)
                "1:                                             \n\t"   //
                "sub            %[num_rows], %[num_rows], #1    \n\t"   // decrement and check num_rows <= 0
                "cmp            %[num_rows], #0                 \n\t"   //
                "blt            5f                              \n\t"   //
                "ldr            r1,[%[input_buf], #0]           \n\t"   // Setup y input pointer from input_buf[0][input_row]
                "ldr            r1,[r1, %[input_row], lsl #2]   \n\t"   //
                "ldr            r2,[%[input_buf], #4]           \n\t"   // Setup cb input pointer from input_buf[1][input_row]
                "pld            [r1]                            \n\t"   //
                "ldr            r2,[r2, %[input_row], lsl #2]   \n\t"   //
                "ldr            r3,[%[input_buf], #8]           \n\t"   // Setup cr input pointer from input_buf[2][input_row]
                "pld            [r2]                            \n\t"   //
                "ldr            r3,[r3, %[input_row], lsl #2]   \n\t"   //
                "add            %[input_row], %[input_row], #1  \n\t"   // input_row++
                "pld            [r3]                            \n\t"   //
                "ldr            r4, [%[output_buf]], #4         \n\t"   // Get output pointer and increment
                "ands           r0, %[num_cols], #7             \n\t"   // Calculate first iteration increment
                "vld1.8         {d14}, [%[matrix]]              \n\t"   // Load dither values
                "beq            2f                              \n\t"   //
                "and            r5, r0, #0x3                    \n\t"   // If columns are not even eight, calculate offset in matrix array
                "add            r5, %[matrix]                   \n\t"   //
                "vld1.8         {d15}, [r5]                     \n\t"   // ...and load iteration 2+ dither values
                "b              3f                              \n\t"   //
                "2:                                             \n\t"   //
                "vmov           d15, d14                        \n\t"   // If columns are even eight, use the same dither matrix for all iterations
                "mov            r0, #8                          \n\t"   // ...and do full iteration
                "3:                                             \n\t"   //
                "mov            r5, %[num_cols]                 \n\t"   // Setup loop counter
                // Inner loop (columns)
                "4:                                             \n\t"   //
                // Read values, subtract 128 from cb/cr, and expand to 32-bit
                "vld1.8         {d30}, [r2]                     \n\t"   // Load eight cb values
                "vld1.8         {d31}, [r3]                     \n\t"   // Load eight cr values
                "vsubl.u8       q8, d30, d28                    \n\t"   // Subtract CENTERJSAMPLE from cb and expand to 16-bit
                "vmovl.s16      q10, d16                        \n\t"   // Expand low cb values to 32-bit
                "vmovl.s16      q11, d17                        \n\t"   // Expand high cb values to 32-bit
                "add            r2, r2, r0                      \n\t"   // Increment input pointer for cb
                "vsubl.u8       q9, d31, d28                    \n\t"   // Subtract CENTERJSAMPLE from cr and expand to 16-bit
                "pld            [r2]                            \n\t"   //
                "vmovl.s16      q12, d18                        \n\t"   // Expand low cr values to 32-bit
                "vmovl.s16      q13, d19                        \n\t"   // Expand high cr values to 32-bit
                "add            r3, r3, r0                      \n\t"   // Increment input pointer for cr
                "vld1.8         {d30}, [r1]                     \n\t"   // Load eight y values
                "pld            [r3]                            \n\t"   //
                // Multiply with the constants. Split into RGB (Vector multiply)
                "vmul.i32       q8, q10, q2                     \n\t"   // Calculate green low/high
                "vmul.i32       q9, q11, q2                     \n\t"   //
                "vmla.i32       q8, q12, q1                     \n\t"   //
                "vmla.i32       q9, q13, q1                     \n\t"   //
                "vmul.i32       q10, q10, q3                    \n\t"   // Calculate blue low/high
                "vmul.i32       q11, q11, q3                    \n\t"   //
                "vmul.i32       q12, q12, q0                    \n\t"   // Calculate red low/high
                "vmul.i32       q13, q13, q0                    \n\t"   //
                "add            r1, r1, r0                      \n\t"   // Increment input pointer for y
                // Shift and combine RGB result (Vector rounding narrowing shift right by constant)
                "vrshrn.i32     d16, q8, #16                    \n\t"   // Shift green
                "pld            [r1]                            \n\t"   //
                "vrshrn.i32     d17, q9, #16                    \n\t"   //
                "vrshrn.i32     d20, q10, #16                   \n\t"   // Shift blue
                "vrshrn.i32     d21, q11, #16                   \n\t"   //
                "vrshrn.i32     d24, q12, #16                   \n\t"   // Shift red
                "vrshrn.i32     d25, q13, #16                   \n\t"   //
                // // Add y (Vector add)
                "vmovl.u8       q15, d30                        \n\t"   // Expand y to 16-bit
                "vadd.i16       q8, q15                         \n\t"   // Add y to green
                "vadd.i16       q10, q15                        \n\t"   // Add y to blue
                "vadd.i16       q12, q15                        \n\t"   // Add y to red
                "subs           r5, r5, r0                      \n\t"   // Decrement loop counter
                // Do the dither
                "vaddw.s8       q10, d14                        \n\t"   // Add dither to blue
                "vaddw.s8       q12, d14                        \n\t"   // Add dither to red
                "vshr.s8        d14, #1                         \n\t"   // Shift green dither by one, since green will use 6 bits
                "vaddw.s8       q8, d14                         \n\t"   // Add dither to green
                // Convert result from signed 16-bit to unsinged 8-bit with range limitation.
                // Range-limiting is essential due to noise introduced by DCT losses.
                // (Vector Saturating Move and Narrow, signed operand with Unsigned result)
                "vqmovun.s16    d16, q8                         \n\t"   // Convert green
                "vqmovun.s16    d20, q10                        \n\t"   // Convert blue
                "vqmovun.s16    d24, q12                        \n\t"   // Convert red
                // Pack to 565 format
                "vshll.u8       q8, d16, #8                     \n\t"   // Shift green and expand to 16-bit
                "vshll.u8       q12, d24, #8                    \n\t"   // Shift red and expand to 16-bit
                "vshll.u8       q10, d20, #8                    \n\t"   // Shift blue and expand to 16-bit
                "vsri.u16       q12, q8, #5                     \n\t"   // Insert green into red
                "vsri.u16       q12, q10, #11                   \n\t"   // Insert blue into red
                "vst1.16        {q12}, [r4]                     \n\t"   // Write result to memory
                // Increase pointers and counters
                "add            r4, r4, r0, lsl #1              \n\t"   // Increment output buffer pointer
                "vmov.u8        d14, d15                        \n\t"   // Set dither matrix to iteration 2+ values
                "mov            r0, #8                          \n\t"   // Set next loop iteration length
                "bne            4b                              \n\t"   // If inner loop counter != 0, loop
                "b              1b                              \n\t"   // Outer loop
                "5:                                             \n\t"   //
                : [input_row] "+r" (input_row), [output_buf] "+r" (output_buf), [num_rows] "+r" (num_rows)
                : [input_buf] "r" (input_buf), [num_cols] "r" (num_cols), [matrix] "r" (matrix)
                : "cc", "memory", "r0", "r1", "r2", "r3", "r4", "r5", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", "d26", "d27", "d28", "d30", "d31"
                );
}
#endif

#endif


/**************** Cases other than YCbCr -> RGB(A) **************/

#ifdef ANDROID_RGB
METHODDEF(void)
rgb_rgba_8888_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      *outptr++ = *inptr0++;
      *outptr++ = *inptr1++;
      *outptr++ = *inptr2++;
      *outptr++ = 0xFF;
    }
  }
}

METHODDEF(void)
rgb_rgb_565_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int r, g, b;
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    if (PACK_NEED_ALIGNMENT(outptr)) {
        r = GETJSAMPLE(*inptr0++);
        g = GETJSAMPLE(*inptr1++);
        b = GETJSAMPLE(*inptr2++);
        rgb = PACK_SHORT_565(r,g,b);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      r = GETJSAMPLE(*inptr0++);
      g = GETJSAMPLE(*inptr1++);
      b = GETJSAMPLE(*inptr2++);
      rgb = PACK_SHORT_565(r,g,b);
      r = GETJSAMPLE(*inptr0++);
      g = GETJSAMPLE(*inptr1++);
      b = GETJSAMPLE(*inptr2++);
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(r,g,b));
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      r = GETJSAMPLE(*inptr0);
      g = GETJSAMPLE(*inptr1);
      b = GETJSAMPLE(*inptr2);
      rgb = PACK_SHORT_565(r,g,b);
      *(INT16*)outptr = rgb;
    }
  }
}


METHODDEF(void)
rgb_rgb_565D_convert (j_decompress_ptr cinfo,
         JSAMPIMAGE input_buf, JDIMENSION input_row,
         JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2;
  register JDIMENSION col;
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  JDIMENSION num_cols = cinfo->output_width;
  INT32 d0 = dither_matrix[cinfo->output_scanline & DITHER_MASK];
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int r, g, b;
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    input_row++;
    outptr = *output_buf++;
    if (PACK_NEED_ALIGNMENT(outptr)) {
        r = range_limit[DITHER_565_R(GETJSAMPLE(*inptr0++), d0)];
        g = range_limit[DITHER_565_G(GETJSAMPLE(*inptr1++), d0)];
        b = range_limit[DITHER_565_B(GETJSAMPLE(*inptr2++), d0)];
        rgb = PACK_SHORT_565(r,g,b);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      r = range_limit[DITHER_565_R(GETJSAMPLE(*inptr0++), d0)];
      g = range_limit[DITHER_565_G(GETJSAMPLE(*inptr1++), d0)];
      b = range_limit[DITHER_565_B(GETJSAMPLE(*inptr2++), d0)];
      d0 = DITHER_ROTATE(d0);
      rgb = PACK_SHORT_565(r,g,b);
      r = range_limit[DITHER_565_R(GETJSAMPLE(*inptr0++), d0)];
      g = range_limit[DITHER_565_G(GETJSAMPLE(*inptr1++), d0)];
      b = range_limit[DITHER_565_B(GETJSAMPLE(*inptr2++), d0)];
      d0 = DITHER_ROTATE(d0);
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(r,g,b));
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      r = range_limit[DITHER_565_R(GETJSAMPLE(*inptr0), d0)];
      g = range_limit[DITHER_565_G(GETJSAMPLE(*inptr1), d0)];
      b = range_limit[DITHER_565_B(GETJSAMPLE(*inptr2), d0)];
      rgb = PACK_SHORT_565(r,g,b);
      *(INT16*)outptr = rgb;
    }
  }
}

#endif

/*
 * Color conversion for no colorspace change: just copy the data,
 * converting from separate-planes to interleaved representation.
 */

METHODDEF(void)
null_convert (j_decompress_ptr cinfo,
	      JSAMPIMAGE input_buf, JDIMENSION input_row,
	      JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW inptr, outptr;
  register JDIMENSION count;
  register int num_components = cinfo->num_components;
  JDIMENSION num_cols = cinfo->output_width;
  int ci;

  while (--num_rows >= 0) {
    for (ci = 0; ci < num_components; ci++) {
      inptr = input_buf[ci][input_row];
      outptr = output_buf[0] + ci;
      for (count = num_cols; count > 0; count--) {
	*outptr = *inptr++;	/* needn't bother with GETJSAMPLE() here */
	outptr += num_components;
      }
    }
    input_row++;
    output_buf++;
  }
}


/*
 * Color conversion for grayscale: just copy the data.
 * This also works for YCbCr -> grayscale conversion, in which
 * we just copy the Y (luminance) component and ignore chrominance.
 */

METHODDEF(void)
grayscale_convert (j_decompress_ptr cinfo,
		   JSAMPIMAGE input_buf, JDIMENSION input_row,
		   JSAMPARRAY output_buf, int num_rows)
{
  jcopy_sample_rows(input_buf[0], (int) input_row, output_buf, 0,
		    num_rows, cinfo->output_width);
}


/*
 * Convert grayscale to RGB: just duplicate the graylevel three times.
 * This is provided to support applications that don't want to cope
 * with grayscale as a separate case.
 */

METHODDEF(void)
gray_rgb_convert (j_decompress_ptr cinfo,
		  JSAMPIMAGE input_buf, JDIMENSION input_row,
		  JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW inptr, outptr;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;

  while (--num_rows >= 0) {
    inptr = input_buf[0][input_row++];
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      /* We can dispense with GETJSAMPLE() here */
      outptr[RGB_RED] = outptr[RGB_GREEN] = outptr[RGB_BLUE] = inptr[col];
      outptr += RGB_PIXELSIZE;
    }
  }
}

#ifdef ANDROID_RGB
METHODDEF(void)
gray_rgba_8888_convert (j_decompress_ptr cinfo,
          JSAMPIMAGE input_buf, JDIMENSION input_row,
          JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW inptr, outptr;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;

  while (--num_rows >= 0) {
    inptr = input_buf[0][input_row++];
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      /* We can dispense with GETJSAMPLE() here */
      outptr[RGB_RED] = outptr[RGB_GREEN] = outptr[RGB_BLUE] = inptr[col];
      outptr[RGB_ALPHA] = 0xff;
      outptr += 4;
    }
  }
}

METHODDEF(void)
gray_rgb_565_convert (j_decompress_ptr cinfo,
          JSAMPIMAGE input_buf, JDIMENSION input_row,
          JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW inptr, outptr;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int g;
    inptr = input_buf[0][input_row++];
    outptr = *output_buf++;
    if (PACK_NEED_ALIGNMENT(outptr)) {
        g = *inptr++;
        rgb = PACK_SHORT_565(g, g, g);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      g = *inptr++;
      rgb = PACK_SHORT_565(g, g, g);
      g = *inptr++;
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(g, g, g));
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      g = *inptr;
      rgb = PACK_SHORT_565(g, g, g);
      *(INT16*)outptr = rgb;
    }
  }
}

METHODDEF(void)
gray_rgb_565D_convert (j_decompress_ptr cinfo,
          JSAMPIMAGE input_buf, JDIMENSION input_row,
          JSAMPARRAY output_buf, int num_rows)
{
  register JSAMPROW inptr, outptr;
  register JDIMENSION col;
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  JDIMENSION num_cols = cinfo->output_width;
  INT32 d0 = dither_matrix[cinfo->output_scanline & DITHER_MASK];

  while (--num_rows >= 0) {
    INT32 rgb;
    unsigned int g;
    inptr = input_buf[0][input_row++];
    outptr = *output_buf++;
    if (PACK_NEED_ALIGNMENT(outptr)) {
        g = *inptr++;
        g = range_limit[DITHER_565_R(g, d0)];
        rgb = PACK_SHORT_565(g, g, g);
        *(INT16*)outptr = rgb;
        outptr += 2;
        num_cols--;
    }
    for (col = 0; col < (num_cols>>1); col++) {
      g = *inptr++;
      g = range_limit[DITHER_565_R(g, d0)];
      rgb = PACK_SHORT_565(g, g, g);
      d0 = DITHER_ROTATE(d0);
      g = *inptr++;
      g = range_limit[DITHER_565_R(g, d0)];
      rgb = PACK_TWO_PIXELS(rgb, PACK_SHORT_565(g, g, g));
      d0 = DITHER_ROTATE(d0);
      WRITE_TWO_ALIGNED_PIXELS(outptr, rgb);
      outptr += 4;
    }
    if (num_cols&1) {
      g = *inptr;
      g = range_limit[DITHER_565_R(g, d0)];
      rgb = PACK_SHORT_565(g, g, g);
      *(INT16*)outptr = rgb;
    }
  }
}
#endif

/*
 * Adobe-style YCCK->CMYK conversion.
 * We convert YCbCr to R=1-C, G=1-M, and B=1-Y using the same
 * conversion as above, while passing K (black) unchanged.
 * We assume build_ycc_rgb_table has been called.
 */

METHODDEF(void)
ycck_cmyk_convert (j_decompress_ptr cinfo,
		   JSAMPIMAGE input_buf, JDIMENSION input_row,
		   JSAMPARRAY output_buf, int num_rows)
{
  my_cconvert_ptr cconvert = (my_cconvert_ptr) cinfo->cconvert;
  register int y, cb, cr;
  register JSAMPROW outptr;
  register JSAMPROW inptr0, inptr1, inptr2, inptr3;
  register JDIMENSION col;
  JDIMENSION num_cols = cinfo->output_width;
  /* copy these pointers into registers if possible */
  register JSAMPLE * range_limit = cinfo->sample_range_limit;
  register int * Crrtab = cconvert->Cr_r_tab;
  register int * Cbbtab = cconvert->Cb_b_tab;
  register INT32 * Crgtab = cconvert->Cr_g_tab;
  register INT32 * Cbgtab = cconvert->Cb_g_tab;
  SHIFT_TEMPS

  while (--num_rows >= 0) {
    inptr0 = input_buf[0][input_row];
    inptr1 = input_buf[1][input_row];
    inptr2 = input_buf[2][input_row];
    inptr3 = input_buf[3][input_row];
    input_row++;
    outptr = *output_buf++;
    for (col = 0; col < num_cols; col++) {
      y  = GETJSAMPLE(inptr0[col]);
      cb = GETJSAMPLE(inptr1[col]);
      cr = GETJSAMPLE(inptr2[col]);
      /* Range-limiting is essential due to noise introduced by DCT losses. */
      outptr[0] = range_limit[MAXJSAMPLE - (y + Crrtab[cr])];   /* red */
      outptr[1] = range_limit[MAXJSAMPLE - (y +                 /* green */
                              ((int) RIGHT_SHIFT(Cbgtab[cb] + Crgtab[cr],
                                                 SCALEBITS)))];
      outptr[2] = range_limit[MAXJSAMPLE - (y + Cbbtab[cb])];   /* blue */
      /* K passes through unchanged */
      outptr[3] = inptr3[col];	/* don't need GETJSAMPLE here */
      outptr += 4;
    }
  }
}


/*
 * Empty method for start_pass.
 */

METHODDEF(void)
start_pass_dcolor (j_decompress_ptr cinfo)
{
  /* no work needed */
}


/*
 * Module initialization routine for output colorspace conversion.
 */

GLOBAL(void)
jinit_color_deconverter (j_decompress_ptr cinfo)
{
  my_cconvert_ptr cconvert;
  int ci;

  cconvert = (my_cconvert_ptr)
    (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
				SIZEOF(my_color_deconverter));
  cinfo->cconvert = (struct jpeg_color_deconverter *) cconvert;
  cconvert->pub.start_pass = start_pass_dcolor;

  /* Make sure num_components agrees with jpeg_color_space */
  switch (cinfo->jpeg_color_space) {
  case JCS_GRAYSCALE:
    if (cinfo->num_components != 1)
      ERREXIT(cinfo, JERR_BAD_J_COLORSPACE);
    break;

  case JCS_RGB:
  case JCS_YCbCr:
    if (cinfo->num_components != 3)
      ERREXIT(cinfo, JERR_BAD_J_COLORSPACE);
    break;

  case JCS_CMYK:
  case JCS_YCCK:
    if (cinfo->num_components != 4)
      ERREXIT(cinfo, JERR_BAD_J_COLORSPACE);
    break;

  default:			/* JCS_UNKNOWN can be anything */
    if (cinfo->num_components < 1)
      ERREXIT(cinfo, JERR_BAD_J_COLORSPACE);
    break;
  }

  /* Set out_color_components and conversion method based on requested space.
   * Also clear the component_needed flags for any unused components,
   * so that earlier pipeline stages can avoid useless computation.
   */

  switch (cinfo->out_color_space) {
  case JCS_GRAYSCALE:
    cinfo->out_color_components = 1;
    if (cinfo->jpeg_color_space == JCS_GRAYSCALE ||
	cinfo->jpeg_color_space == JCS_YCbCr) {
      cconvert->pub.color_convert = grayscale_convert;
      /* For color->grayscale conversion, only the Y (0) component is needed */
      for (ci = 1; ci < cinfo->num_components; ci++)
	cinfo->comp_info[ci].component_needed = FALSE;
    } else
      ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    break;

  case JCS_RGB:
    cinfo->out_color_components = RGB_PIXELSIZE;
    if (cinfo->jpeg_color_space == JCS_YCbCr) {
      cconvert->pub.color_convert = ycc_rgb_convert;
      build_ycc_rgb_table(cinfo);
    } else if (cinfo->jpeg_color_space == JCS_GRAYSCALE) {
      cconvert->pub.color_convert = gray_rgb_convert;
    } else if (cinfo->jpeg_color_space == JCS_RGB && RGB_PIXELSIZE == 3) {
      cconvert->pub.color_convert = null_convert;
    } else
      ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    break;

#ifdef ANDROID_RGB
  case JCS_RGBA_8888:
    cinfo->out_color_components = 4;
    if (cinfo->jpeg_color_space == JCS_YCbCr) {
      cconvert->pub.color_convert = YCC_RGBA_8888_Proc;
#if defined(ENABLE_NEON_YCC_RGBA_8888)
      clear_ycc_rgb_table(cinfo);
#else
      build_ycc_rgb_table(cinfo);
#endif
    } else if (cinfo->jpeg_color_space == JCS_GRAYSCALE) {
      cconvert->pub.color_convert = gray_rgba_8888_convert;
    } else if (cinfo->jpeg_color_space == JCS_RGB) {
      cconvert->pub.color_convert = rgb_rgba_8888_convert;
    } else
      ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    break;

  case JCS_RGB_565:
    cinfo->out_color_components = RGB_PIXELSIZE;
    if (cinfo->dither_mode == JDITHER_NONE) {
      if (cinfo->jpeg_color_space == JCS_YCbCr) {
        cconvert->pub.color_convert = YCC_RGB_565_Proc;
#if defined(ENABLE_NEON_YCC_RGB_565)
        clear_ycc_rgb_table(cinfo);
#else
        build_ycc_rgb_table(cinfo);
#endif
      } else if (cinfo->jpeg_color_space == JCS_GRAYSCALE) {
        cconvert->pub.color_convert = gray_rgb_565_convert;
      } else if (cinfo->jpeg_color_space == JCS_RGB) {
        cconvert->pub.color_convert = rgb_rgb_565_convert;
      } else
        ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    } else {
      /* only ordered dither is supported */
      if (cinfo->jpeg_color_space == JCS_YCbCr) {
        cconvert->pub.color_convert = YCC_RGB_565D_Proc;
#if defined(ENABLE_NEON_YCC_RGB_565D)
        clear_ycc_rgb_table(cinfo);
#else
        build_ycc_rgb_table(cinfo);
#endif
      } else if (cinfo->jpeg_color_space == JCS_GRAYSCALE) {
        cconvert->pub.color_convert = gray_rgb_565D_convert;
      } else if (cinfo->jpeg_color_space == JCS_RGB) {
        cconvert->pub.color_convert = rgb_rgb_565D_convert;
      } else
        ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    }
    break;
#endif
    
  case JCS_CMYK:
    cinfo->out_color_components = 4;
    if (cinfo->jpeg_color_space == JCS_YCCK) {
      cconvert->pub.color_convert = ycck_cmyk_convert;
      build_ycc_rgb_table(cinfo);
    } else if (cinfo->jpeg_color_space == JCS_CMYK) {
      cconvert->pub.color_convert = null_convert;
    } else
      ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    break;

  default:
    /* Permit null conversion to same output space */
    if (cinfo->out_color_space == cinfo->jpeg_color_space) {
      cinfo->out_color_components = cinfo->num_components;
      cconvert->pub.color_convert = null_convert;
    } else			/* unsupported non-null conversion */
      ERREXIT(cinfo, JERR_CONVERSION_NOTIMPL);
    break;
  }

  if (cinfo->quantize_colors)
    cinfo->output_components = 1; /* single colormapped output component */
  else
    cinfo->output_components = cinfo->out_color_components;
}
