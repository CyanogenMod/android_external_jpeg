;==========================================================================
; jdcolor-armv7.s
;
;  Copyright (c) 2010, Code Aurora Forum. All rights reserved.
;
;  Redistribution and use in source and binary forms, with or without
;  modification, are permitted provided that the following conditions are
;  met:
;      * Redistributions of source code must retain the above copyright
;        notice, this list of conditions and the following disclaimer.
;      * Redistributions in binary form must reproduce the above
;        copyright notice, this list of conditions and the following
;        disclaimer in the documentation and/or other materials provided
;        with the distribution.
;      * Neither the name of Code Aurora Forum, Inc. nor the names of its
;        contributors may be used to endorse or promote products derived
;        from this software without specific prior written permission.
;
;  THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT
;  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;==========================================================================

;==========================================================================
;                         FUNCTION LIST
;--------------------------------------------------------------------------
;
; - yvu2rgb565_venum
; - yyvu2rgb565_venum
; - yvu2bgr888_venum
; - yyvu2bgr888_venum
;
;==========================================================================

    AREA  |.text|, CODE, READONLY
    CODE32

;==========================================================================
;   ARM Registers
;==========================================================================

p_luma    RN 0
p_chroma  RN 1
p_rgb     RN 2
length    RN 3

;==========================================================================
;   Main Routine
;==========================================================================

    EXPORT yvu2rgb565_venum
    EXPORT yyvu2rgb565_venum
    EXPORT yvu2bgr888_venum
    EXPORT yyvu2bgr888_venum

;==========================================================================
;   Constants
;==========================================================================

; coefficients in color conversion matrix multiplication
COEFF_Y       EQU   298               ; contribution of Y
COEFF_V_RED   EQU   409               ; contribution of V for red
COEFF_U_GREEN EQU  -100               ; contribution of U for green
COEFF_V_GREEN EQU  -208               ; contribution of V for green
COEFF_U_BLUE  EQU   516               ; contribution of U for blue

; Clamping constants 0x0 and 0xFF
COEFF_0      EQU    0
COEFF_255    EQU    255

; Bias coefficients for red, green and blue
COEFF_BIAS_R EQU  -56992              ; Red   bias = -298*16 - 409*128 + 128
COEFF_BIAS_G EQU   34784              ; Green bias = -298*16 + 308*128 + 128
COEFF_BIAS_B EQU  -70688              ; Blue  bias = -298*16 - 516*128 + 128


;==========================================================================
; FUNCTION     : yvu2rgb565_venum
;--------------------------------------------------------------------------
; DESCRIPTION  : Perform YVU to RGB565 conversion.
;--------------------------------------------------------------------------
; C PROTOTYPE  : void yvu2rgb565_venum(uint8_t  *p_luma,
;                                uint8_t  *p_chroma,
;                                uint8_t  *p_rgb565,
;                                uint32_t  length)
;--------------------------------------------------------------------------
; REG INPUT    : R0: uint8_t  *p_luma
;                      pointer to the input Luma Line
;                R1: uint8_t  *p_chroma
;                      pointer to the input Chroma Line
;                R2: uint8_t  *p_rgb565
;                      pointer to the output RGB Line
;                R3: uint32_t  length
;                      width of Line
;--------------------------------------------------------------------------
; STACK ARG    : None
;--------------------------------------------------------------------------
; REG OUTPUT   : None
;--------------------------------------------------------------------------
; MEM INPUT    : p_luma   - a line of luma pixels
;                p_chroma - a line of chroma pixels
;                length   - the width of the input line
;--------------------------------------------------------------------------
; MEM OUTPUT   : p_rgb565 - the converted rgb pixels
;--------------------------------------------------------------------------
; REG AFFECTED : ARM:  R0-R4
;                NEON: Q0-Q15
;--------------------------------------------------------------------------
; STACK USAGE  : none
;--------------------------------------------------------------------------
; CYCLES       : none
;
;--------------------------------------------------------------------------
; NOTES        :
;==========================================================================
yvu2rgb565_venum  FUNCTION

    ;==========================================================================
    ; Store stack registers
    ;==========================================================================
    STMFD SP!, {R4, LR}

    PLD [R0, R3]                      ; preload luma line

    ADR   R4, constants

    VLD1.S16  {D6, D7}, [R4]!         ; D6, D7: 409 | -100 | -208 | 516 | 298 | 0 | 255 | 0
    VLD1.S32  Q15,      [R4]          ; Q15   :  -56992    |    34784   |  -70688 |     X

    ;==========================================================================
    ; Load clamping parameters to duplicate vector elements
    ;==========================================================================
    VDUP.S16  Q4,  D7[1]              ; Q4:  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0
    VDUP.S16  Q5,  D7[2]              ; Q5: 255 | 255 | 255 | 255 | 255 | 255 | 255 | 255

    ;==========================================================================
    ; Read bias
    ;==========================================================================
    VDUP.S32  Q0,   Q15[0]            ; Q0:  -56992 | -56992 | -56992 | -56992
    VDUP.S32  Q1,   Q15[1]            ; Q1:   34784 |  34784 |  34784 |  34784
    VDUP.S32  Q2,   Q15[2]            ; Q2:  -70688 | -70688 | -70688 | -70688


    ;==========================================================================
    ; The main loop
    ;==========================================================================
loop_yvu2rgb565

    ;==========================================================================
    ; Load input from Y, V and U
    ; D12     : Y0  Y1  Y2  Y3  Y4  Y5  Y6  Y7
    ; D14, D15: V0  V1  V2  V3  V4  V5  V6  V7,  U0  U1  U2  U3  U4  U5   U6  U7
    ;==========================================================================
    VLD1.U8  {D12},     [p_luma]!     ; Load 8 Luma elements (uint8) to D12
    VLD2.U8  {D14,D15}, [p_chroma]!   ; Load and scatter 8 Chroma elements pairs
                                      ; (uint8) to D14, D15

    ;==========================================================================
    ; Expand uint8 value to uint16
    ; D18, D19: Y0 Y1 Y2 Y3 Y4 Y5 Y6 Y7
    ; D20, D21: V0 V1 V2 V3 V4 V5 V6 V7
    ; D22, D23: U0 U1 U2 U3 U4 U5 U6 U7
    ;==========================================================================
    VMOVL.U16.U8 Q9,  D12
    VMOVL.U16.U8 Q10, D14
    VMOVL.U16.U8 Q11, D15

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q12, D20, D6[0]        ; Q12:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q13, D22, D6[1]        ; Q13: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q13, D20, D6[2]        ; Q13: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q14, D22, D6[3]        ; Q14:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q12, Q0                 ; Q12 add Red   bias -56992
    VADD.S32  Q13, Q1                 ; Q13 add Green bias  34784
    VADD.S32  Q14, Q2                 ; Q14 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMLAL.S16  Q12, D18, D7[0]        ; Q12: R0, R1, R2, R3 in 32-bit Q8 format
    VMLAL.S16  Q13, D18, D7[0]        ; Q13: G0, G1, G2, G3 in 32-bit Q8 format
    VMLAL.S16  Q14, D18, D7[0]        ; Q14: B0, B1, B2, B3 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D18 , Q12, #8         ; D18: R0, R1, R2, R3 in 16-bit Q0 format
    VSHRN.S32   D20 , Q13, #8         ; D20: G0, G1, G2, G3 in 16-bit Q0 format
    VSHRN.S32   D22,  Q14, #8         ; D22: B0, B1, B2, B3 in 16-bit Q0 format

    ;==========================================================================
    ; Done with the first 4 elements, continue on the next 4 elements
    ;==========================================================================

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q12, D21, D6[0]        ; Q12:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q13, D23, D6[1]        ; Q13: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q13, D21, D6[2]        ; Q13: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q14, D23, D6[3]        ; Q14:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q12, Q0                 ; Q12 add Red   bias -56992
    VADD.S32  Q13, Q1                 ; Q13 add Green bias  34784
    VADD.S32  Q14, Q2                 ; Q14 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMLAL.S16  Q12, D19, D7[0]        ; Q12: R0, R1, R2, R3 in 32-bit Q8 format
    VMLAL.S16  Q13, D19, D7[0]        ; Q13: G0, G1, G2, G3 in 32-bit Q8 format
    VMLAL.S16  Q14, D19, D7[0]        ; Q14: B0, B1, B2, B3 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D19 , Q12, #8         ; D18: R0, R1, R2, R3 in 16-bit Q0 format
    VSHRN.S32   D21 , Q13, #8         ; D20: G0, G1, G2, G3 in 16-bit Q0 format
    VSHRN.S32   D23,  Q14, #8         ; D22: B0, B1, B2, B3 in 16-bit Q0 format

    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q9, Q9, Q4              ; if Q9 <   0, Q9 =   0
    VMIN.S16  Q9, Q9, Q5              ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16  D28, Q9              ; store Red to D28, narrow the value from int16 to int8

    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D27, Q10            ; store Green to D27, narrow the value from int16 to int8

    VMAX.S16  Q11, Q11, Q4            ; if Q11 <   0, Q11 =   0
    VMIN.S16  Q11, Q11, Q5            ; if Q11 > 255, Q11 = 255
    VQMOVUN.S16   D26, Q11            ; store Blue to D26, narrow the value from int16 to int8.

    ;==========================================================================
    ; D27:  3 bits of Green + 5 bits of Blue
    ; D28:  5 bits of Red   + 3 bits of Green
    ;==========================================================================
    VSRI.8   D28, D27, #5             ; right shift G by 5 and insert to R
    VSHL.U8, D27, D27, #3             ; left  shift G by 3
    VSRI.8   D27, D26, #3             ; right shift B by 3 and insert to G

    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yvu2rgb565          ; jump to trailing processing if remaining length is less than 8

    VST2.U8  {D27, D28}, [p_rgb]!     ; vector store Red, Green, Blue to destination
                                      ; Blue at LSB

    BHI loop_yvu2rgb565               ; loop if more than 8 pixels left

    BEQ  end_yvu2rgb565               ; done if exactly 8 pixels processed in the loop


trailing_yvu2rgb565
    ;==========================================================================
    ; There are from 1 ~ 7 pixels left in the trailing part.
    ; First adding 7 to the length so the length would be from 0 ~ 6.
    ; eg: 1 pixel left in the trailing part, so 1-8+7 = 0.
    ; Then save 1 pixel unconditionally since at least 1 pixel left in the
    ; trailing part.
    ;==========================================================================
    ADDS length, length, #7             ; there are 7 or less in the trailing part

    VST2.U8 {D27[0], D28[0]}, [p_rgb]!  ; at least 1 pixel left in the trailing part
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[1], D28[1]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[2], D28[2]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[3], D28[3]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[4], D28[4]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[5], D28[5]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2rgb565	                ; done if 0 pixel left

    SUBS length, length, #1             ; update length counter
    VST2.U8 {D27[6], D28[6]}, [p_rgb]!  ; store one more pixel

end_yvu2rgb565
    LDMFD SP!, {R4, PC}

    ENDFUNC                             ; end of yvu2rgb565


;==========================================================================
; FUNCTION     : yyvu2rgb565_venum
;--------------------------------------------------------------------------
; DESCRIPTION  : Perform YYVU to RGB565 conversion.
;--------------------------------------------------------------------------
; C PROTOTYPE  : void yyvu2rgb565_venum(uint8_t  *p_luma,
;                                 uint8_t  *p_chroma,
;                                 uint8_t  *p_rgb565,
;                                 uint32_t  length)
;--------------------------------------------------------------------------
; REG INPUT    : R0: uint8_t  *p_luma
;                      pointer to the input Luma Line
;                R1: uint8_t  *p_chroma
;                      pointer to the input Chroma Line
;                R2: uint8_t  *p_rgb565
;                      pointer to the output RGB Line
;                R3: uint32_t  length
;                      width of Line
;--------------------------------------------------------------------------
; STACK ARG    : None
;--------------------------------------------------------------------------
; REG OUTPUT   : None
;--------------------------------------------------------------------------
; MEM INPUT    : p_luma   - a line of luma pixels
;                p_chroma - a line of chroma pixels
;                length   - the width of the input line
;--------------------------------------------------------------------------
; MEM OUTPUT   : p_rgb565 - the converted rgb pixels
;--------------------------------------------------------------------------
; REG AFFECTED : ARM:  R0-R4
;                NEON: Q0-Q15
;--------------------------------------------------------------------------
; STACK USAGE  : none
;--------------------------------------------------------------------------
; CYCLES       : none
;
;--------------------------------------------------------------------------
; NOTES        :
;==========================================================================
yyvu2rgb565_venum  FUNCTION

    ;==========================================================================
    ; Store stack registers
    ;==========================================================================
    STMFD SP!, {R4, LR}

    PLD [R0, R3]                      ; preload luma line

    ADR   R4, constants

    VLD1.S16  {D6, D7}, [R4]!         ; D6, D7: 409 | -100 | -208 | 516 | 298 | 0 | 255 | 0
    VLD1.S32  Q15,      [R4]          ; Q15   :  -56992    |    34784   |  -70688 |     X

    ;==========================================================================
    ; Load clamping parameters to duplicate vector elements
    ;==========================================================================
    VDUP.S16  Q4,  D7[1]              ; Q4:  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0
    VDUP.S16  Q5,  D7[2]              ; Q5: 255 | 255 | 255 | 255 | 255 | 255 | 255 | 255

    ;==========================================================================
    ; Read bias
    ;==========================================================================
    VDUP.S32  Q0,   Q15[0]            ; Q0:  -56992 | -56992 | -56992 | -56992
    VDUP.S32  Q1,   Q15[1]            ; Q1:   34784 |  34784 |  34784 |  34784
    VDUP.S32  Q2,   Q15[2]            ; Q2:  -70688 | -70688 | -70688 | -70688


    ;==========================================================================
    ; The main loop
    ;==========================================================================
loop_yyvu2rgb565

    ;==========================================================================
    ; Load input from Y, V and U
    ; D12, D13: Y0 Y2 Y4 Y6 Y8 Y10 Y12 Y14, Y1 Y3 Y5 Y7 Y9 Y11 Y13 Y15
    ; D14, D15: V0 V1 V2 V3 V4 V5  V6  V7 , U0 U1 U2 U3 U4 U5  U6  U7
    ;==========================================================================
    VLD2.U8  {D12,D13}, [p_luma]!     ; Load 16 Luma elements (uint8) to D12, D13
    VLD2.U8  {D14,D15}, [p_chroma]!   ; Load and scatter 8 Chroma elements pairs
                                      ; (uint8) to D14, D15

    ;==========================================================================
    ; Expand uint8 value to uint16
    ; D24, D25: Y0 Y2 Y4 Y6 Y8 Y10 Y12 Y14
    ; D26, D27: Y1 Y3 Y5 Y7 Y9 Y11 Y13 Y15
    ; D28, D29: V0 V1 V2 V3 V4 V5  V6  V7
    ; D30, D31: U0 U1 U2 U3 U4 U5  U6  U7
    ;==========================================================================
    VMOVL.U16.U8 Q12, D12
    VMOVL.U16.U8 Q13, D13
    VMOVL.U16.U8 Q14, D14
    VMOVL.U16.U8 Q15, D15

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q6, D28, D6[0]         ; Q6:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q7, D30, D6[1]         ; Q7: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q7, D28, D6[2]         ; q7: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q8, D30, D6[3]         ; q8:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q6, Q0                  ; Q6 add Red   bias -56992
    VADD.S32  Q7, Q1                  ; Q7 add Green bias  34784
    VADD.S32  Q8, Q2                  ; Q8 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMOV.S32   Q9, Q6
    VMLAL.S16  Q6, D24, D7[0]         ; Q6: R0, R2, R4, R6 in 32-bit Q8 format
    VMLAL.S16  Q9, D26, D7[0]         ; Q9: R1, R3, R5, R7 in 32-bit Q8 format

    VMOV.S32   Q10, Q7
    VMLAL.S16  Q7,  D24, D7[0]        ; Q7:  G0, G2, G4, G6 in 32-bit Q8 format
    VMLAL.S16  Q10, D26, D7[0]        ; Q10: G1, G3, G5, G7 in 32-bit Q8 format

    VMOV.S32   Q11, Q8
    VMLAL.S16  Q8,  D24, D7[0]        ; Q8:  B0, B2, B4, B6 in 32-bit Q8 format
    VMLAL.S16  Q11, D26, D7[0]        ; Q11: B1, B3, B5, B7 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D12, Q6,  #8          ; D12: R0 R2 R4 R6 in 16-bit Q0 format
    VSHRN.S32   D13, Q9,  #8          ; D13: R1 R3 R5 R7 in 16-bit Q0 format
    VZIP.16     D12, D13              ; Q6 : R0 R1 R2 R3 R4 R5 R6 R7

    VSHRN.S32   D18, Q7,  #8          ; D18: G0 G2 G4 G6 in 16-bit Q0 format
    VSHRN.S32   D19, Q10, #8          ; D19: G1 G3 G5 G7 in 16-bit Q0 format
    VZIP.16     D18, D19              ; Q9 : G0 G1 G2 G3 G4 G5 G6 G7

    VSHRN.S32   D20, Q8,  #8          ; D20: B0 B2 B4 B6 in 16-bit Q0 format
    VSHRN.S32   D21, Q11, #8          ; D21: B1 B3 B5 B7 in 16-bit Q0 format
    VZIP.16     D20, D21              ; Q10: B0 B1 B2 B3 B4 B5 B6 B7

    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q6, Q6, Q4              ; if Q6 <   0, Q6 =   0
    VMIN.S16  Q6, Q6, Q5              ; if Q6 > 255, Q6 = 255
    VQMOVUN.S16  D23, Q6              ; store Red to D23, narrow the value from int16 to int8

    VMAX.S16  Q9, Q9, Q4              ; if Q9 <   0, Q9 =   0
    VMIN.S16  Q9, Q9, Q5              ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16  D22, Q9              ; store Green to D22, narrow the value from int16 to int8

    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D21, Q10            ; store Blue to D21, narrow the value from int16 to int8

    ;==========================================================================
    ; D22:  3 bits of Green + 5 bits of Blue
    ; D23:  5 bits of Red   + 3 bits of Green
    ;==========================================================================
    VSRI.8   D23, D22, #5	          ; right shift G by 5 and insert to R
    VSHL.U8, D22, D22, #3             ; left shift G by 3
    VSRI.8   D22, D21, #3             ; right shift B by 3 and insert to G
    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yyvu2rgb565         ; jump to trailing processing if remaining length is less than 8

    VST2.U8  {D22,D23}, [p_rgb]!      ; vector store Red, Green, Blue to destination
                                      ; Blue at LSB

    BEQ  end_yyvu2rgb565              ; done if exactly 8 pixels processed in the loop

    ;==========================================================================
    ; Done with the first 8 elements, continue on the next 8 elements
    ;==========================================================================

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q6, D29, D6[0]         ; Q6: 409*(V4,V5,V6,V7)       Red
    VMULL.S16  Q7, D31, D6[1]         ; Q7: -100*(U4,U5,U6,U7)      Green
    VMLAL.S16  Q7, D29, D6[2]         ; Q7: -100*(U4,U5,U6,U7) - 208*(V4,V5,V6,V7)
    VMULL.S16  Q8, D31, D6[3]         ; Q8: 516*(U4,U5,U6,U7)       Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q6, Q0                  ; Q6 add Red   bias -56992
    VADD.S32  Q7, Q1                  ; Q7 add Green bias  34784
    VADD.S32  Q8, Q2                  ; Q8 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMOV.S32   Q9, Q6
    VMLAL.S16  Q6, D25, D7[0]         ; Q6: R8 R10 R12 R14 in 32-bit Q8 format
    VMLAL.S16  Q9, D27, D7[0]         ; Q9: R9 R11 R13 R15 in 32-bit Q8 format

    VMOV.S32   Q10, Q7
    VMLAL.S16  Q7,  D25, D7[0]        ; Q7: G0, G2, G4, G6 in 32-bit Q8 format
    VMLAL.S16  Q10, D27, D7[0]        ; Q10 : G1, G3, G5, G7 in 32-bit Q8 format

    VMOV.S32   Q11, Q8
    VMLAL.S16  Q8,  D25, D7[0]        ; Q8: B0, B2, B4, B6 in 32-bit Q8 format
    VMLAL.S16  Q11, D27, D7[0]        ; Q11 : B1, B3, B5, B7 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D12, Q6,  #8          ; D12: R8 R10 R12 R14 in 16-bit Q0 format
    VSHRN.S32   D13, Q9,  #8          ; D13: R9 R11 R13 R15 in 16-bit Q0 format
    VZIP.16     D12, D13              ; Q6: R8 R9 R10 R11 R12 R13 R14 R15

    VSHRN.S32   D18, Q7,  #8          ; D18: G8 G10 G12 G14 in 16-bit Q0 format
    VSHRN.S32   D19, Q10, #8          ; D19: G9 G11 G13 G15 in 16-bit Q0 format
    VZIP.16     D18, D19              ; Q9:  G8 G9 G10 G11 G12 G13 G14 G15

    VSHRN.S32   D20, Q8,  #8          ; D20: B8 B10 B12 B14 in 16-bit Q0 format
    VSHRN.S32   D21, Q11, #8          ; D21: B9 B11 B13 B15 in 16-bit Q0 format
    VZIP.16     D20, D21              ; Q10: B8 B9 B10 B11 B12 B13 B14 B15
    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q6, Q6, Q4              ; if Q6 <   0, Q6 =   0
    VMIN.S16  Q6, Q6, Q5              ; if Q6 > 255, Q6 = 255
    VQMOVUN.S16  D23, Q6              ; store Red to D23, narrow the value from int16 to int8

    VMAX.S16  Q9, Q9, Q4              ; if Q9 <   0, Q9 =   0
    VMIN.S16  Q9, Q9, Q5              ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16  D22, Q9              ; store Green to D22, narrow the value from int16 to int8

    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D21, Q10            ; store Blue to D21, narrow the value from int16 to int8

    ;==========================================================================
    ; D22:  3 bits of Green + 5 bits of Blue
    ; D23:  5 bits of Red   + 3 bits of Green
    ;==========================================================================
    VSRI.8   D23, D22, #5	          ; right shift G by 5 and insert to R
    VSHL.U8, D22, D22, #3             ; left shift G by 3
    VSRI.8   D22, D21, #3             ; right shift B by 3 and insert to G

    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yyvu2rgb565         ; jump to trailing processing if remaining length is less than 8

    VST2.U8  {D22,D23}, [p_rgb]!      ; vector store Red, Green, Blue to destination
                                      ; Blue at LSB

    BHI loop_yyvu2rgb565              ; loop if more than 8 pixels left

    BEQ  end_yyvu2rgb565              ; done if exactly 8 pixels processed in the loop


trailing_yyvu2rgb565
    ;==========================================================================
    ; There are from 1 ~ 7 pixels left in the trailing part.
    ; First adding 7 to the length so the length would be from 0 ~ 6.
    ; eg: 1 pixel left in the trailing part, so 1-8+7 = 0.
    ; Then save 1 pixel unconditionally since at least 1 pixel left in the
    ; trailing part.
    ;==========================================================================
    ADDS length, length, #7           ; there are 7 or less in the trailing part

    VST2.U8 {D22[0],D23[0]}, [p_rgb]! ; at least 1 pixel left in the trailing part
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[1],D23[1]}, [p_rgb]! ; store one more pixel
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[2],D23[2]}, [p_rgb]! ; store one more pixel
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[3],D23[3]}, [p_rgb]! ; store one more pixel
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[4],D23[4]}, [p_rgb]! ; store one more pixel
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[5],D23[5]}, [p_rgb]! ; store one more pixel
    BEQ end_yyvu2rgb565               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST2.U8 {D22[6],D23[6]}, [p_rgb]! ; store one more pixel

end_yyvu2rgb565
    LDMFD SP!, {R4, PC}

    ENDFUNC                            ;end of yyvu2rgb565


;==========================================================================
; These are the constants that would be used in the function.
;==========================================================================
constants
    DCW (COEFF_V_RED),  (COEFF_U_GREEN), (COEFF_V_GREEN), (COEFF_U_BLUE) ;   409  | -100  |  -208  | 516
    DCW (COEFF_Y),      (COEFF_0),       (COEFF_255)    , (COEFF_0)      ;   298  |   0   |   255  |  0
    DCD (COEFF_BIAS_R), (COEFF_BIAS_G),  (COEFF_BIAS_B)                  ; -56992 | 34784 | -70688 |  X


;==========================================================================
; FUNCTION     : yvu2bgr888_venum
;--------------------------------------------------------------------------
; DESCRIPTION  : Perform YVU to BGR888 conversion.
;--------------------------------------------------------------------------
; C PROTOTYPE  : void yvu2bgr888_venum(uint8_t  *p_luma,
;                                 uint8_t  *p_chroma,
;                                 uint8_t  *p_bgr888,
;                                 uint32_t  length)
;--------------------------------------------------------------------------
; REG INPUT    : R0: uint8_t  *p_luma
;                      pointer to the input Luma Line
;                R1: uint8_t  *p_chroma
;                      pointer to the input Chroma Line
;                R2: uint8_t  *p_bgr888
;                      pointer to the output BGR Line
;                R3: uint32_t  length
;                      width of Line
;--------------------------------------------------------------------------
; STACK ARG    : None
;--------------------------------------------------------------------------
; REG OUTPUT   : None
;--------------------------------------------------------------------------
; MEM INPUT    : p_luma   - a line of luma pixels
;                p_chroma - a line of chroma pixels
;                length   - the width of the input line
;--------------------------------------------------------------------------
; MEM OUTPUT   : p_bgr888 - the converted bgr pixels
;--------------------------------------------------------------------------
; REG AFFECTED : ARM:  R0-R4
;                NEON: Q0-Q15
;--------------------------------------------------------------------------
; STACK USAGE  : none
;--------------------------------------------------------------------------
; CYCLES       : none
;
;--------------------------------------------------------------------------
; NOTES        :
;==========================================================================
yvu2bgr888_venum  FUNCTION

    ;==========================================================================
    ; Store stack registers
    ;==========================================================================
    STMFD SP!, {R4, LR}

    PLD [R0, R3]                      ; preload luma line

    ADR   R4, constants

    VLD1.S16  {D6, D7}, [R4]!         ; D6, D7: 409 | -100 | -208 | 516 | 298 | 0 | 255 | 0
    VLD1.S32  Q15,      [R4]          ; Q15   :  -56992    |    34784   |  -70688 |     X

    ;==========================================================================
    ; Load clamping parameters to duplicate vector elements
    ;==========================================================================
    VDUP.S16  Q4,  D7[1]              ; Q4:  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0
    VDUP.S16  Q5,  D7[2]              ; Q5: 255 | 255 | 255 | 255 | 255 | 255 | 255 | 255

    ;==========================================================================
    ; Read bias
    ;==========================================================================
    VDUP.S32  Q0,   Q15[0]            ; Q0:  -56992 | -56992 | -56992 | -56992
    VDUP.S32  Q1,   Q15[1]            ; Q1:   34784 |  34784 |  34784 |  34784
    VDUP.S32  Q2,   Q15[2]            ; Q2:  -70688 | -70688 | -70688 | -70688


    ;==========================================================================
    ; The main loop
    ;==========================================================================
loop_yvu2bgr888

    ;==========================================================================
    ; Load input from Y, V and U
    ; D12     : Y0  Y1  Y2  Y3  Y4  Y5  Y6  Y7
    ; D14, D15: V0  V1  V2  V3  V4  V5  V6  V7,  U0  U1  U2  U3  U4  U5   U6  U7
    ;==========================================================================
    VLD1.U8  {D12},     [p_luma]!     ; Load 8 Luma elements (uint8) to D12
    VLD2.U8  {D14,D15}, [p_chroma]!   ; Load and scatter 8 Chroma elements pairs
                                      ; (uint8) to D14, D15

    ;==========================================================================
    ; Expand uint8 value to uint16
    ; D18, D19: Y0 Y1 Y2 Y3 Y4 Y5 Y6 Y7
    ; D20, D21: V0 V1 V2 V3 V4 V5 V6 V7
    ; D22, D23: U0 U1 U2 U3 U4 U5 U6 U7
    ;==========================================================================
    VMOVL.U16.U8 Q9,  D12
    VMOVL.U16.U8 Q10, D14
    VMOVL.U16.U8 Q11, D15

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q12, D20, D6[0]        ; Q12:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q13, D22, D6[1]        ; Q13: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q13, D20, D6[2]        ; Q13: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q14, D22, D6[3]        ; Q14:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q12, Q0                 ; Q12 add Red   bias -56992
    VADD.S32  Q13, Q1                 ; Q13 add Green bias  34784
    VADD.S32  Q14, Q2                 ; Q14 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMLAL.S16  Q12, D18, D7[0]        ; Q12: R0, R1, R2, R3 in 32-bit Q8 format
    VMLAL.S16  Q13, D18, D7[0]        ; Q13: G0, G1, G2, G3 in 32-bit Q8 format
    VMLAL.S16  Q14, D18, D7[0]        ; Q14: B0, B1, B2, B3 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D18 , Q12, #8         ; D18: R0, R1, R2, R3 in 16-bit Q0 format
    VSHRN.S32   D20 , Q13, #8         ; D20: G0, G1, G2, G3 in 16-bit Q0 format
    VSHRN.S32   D22,  Q14, #8         ; D22: B0, B1, B2, B3 in 16-bit Q0 format

    ;==========================================================================
    ; Done with the first 4 elements, continue on the next 4 elements
    ;==========================================================================

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q12, D21, D6[0]        ; Q12:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q13, D23, D6[1]        ; Q13: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q13, D21, D6[2]        ; Q13: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q14, D23, D6[3]        ; Q14:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q12, Q0                 ; Q12 add Red   bias -56992
    VADD.S32  Q13, Q1                 ; Q13 add Green bias  34784
    VADD.S32  Q14, Q2                 ; Q14 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMLAL.S16  Q12, D19, D7[0]        ; Q12: R0, R1, R2, R3 in 32-bit Q8 format
    VMLAL.S16  Q13, D19, D7[0]        ; Q13: G0, G1, G2, G3 in 32-bit Q8 format
    VMLAL.S16  Q14, D19, D7[0]        ; Q14: B0, B1, B2, B3 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D19 , Q12, #8         ; D18: R0, R1, R2, R3 in 16-bit Q0 format
    VSHRN.S32   D21 , Q13, #8         ; D20: G0, G1, G2, G3 in 16-bit Q0 format
    VSHRN.S32   D23,  Q14, #8         ; D22: B0, B1, B2, B3 in 16-bit Q0 format

    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q11, Q11, Q4            ; if Q11 <   0, Q11 =   0
    VMIN.S16  Q11, Q11, Q5            ; if Q11 > 255, Q11 = 255
    VQMOVUN.S16   D28, Q11            ; store Blue to D28, narrow the value from int16 to int8

    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D27, Q10            ; store Green to D27, narrow the value from int16 to int8

    VMAX.S16    Q9, Q9, Q4            ; if Q9 <   0, Q9 =   0
    VMIN.S16    Q9, Q9, Q5            ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16    D26, Q9            ; store Red to D26, narrow the value from int16 to int8.

    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yvu2bgr888          ; jump to trailing processing if remaining length is less than 8

    VST3.U8  {D26,D27,D28}, [p_rgb]!  ; vector store Red, Green, Blue to destination
                                      ; Blue at LSB

    BHI loop_yvu2bgr888               ; loop if more than 8 pixels left

    BEQ  end_yvu2bgr888               ; done if exactly 8 pixel processed in the loop


trailing_yvu2bgr888
    ;==========================================================================
    ; There are from 1 ~ 7 pixels left in the trailing part.
    ; First adding 7 to the length so the length would be from 0 ~ 6.
    ; eg: 1 pixel left in the trailing part, so 1-8+7 = 0.
    ; Then save 1 pixel unconditionally since at least 1 pixels left in the
    ; trailing part.
    ;==========================================================================
    ADDS length, length, #7           ; there are 7 or less in the trailing part

    VST3.U8 {D26[0], D27[0], D28[0]}, [p_rgb]! ; at least 1 pixel left in the trailing part
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[1], D27[1], D28[1]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[2], D27[2], D28[2]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[3], D27[3], D28[3]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[4], D27[4], D28[4]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[5], D27[5], D28[5]}, [p_rgb]!  ; store one more pixel
    BEQ  end_yvu2bgr888	              ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D26[6], D27[6], D28[6]}, [p_rgb]!	; store one more pixel

end_yvu2bgr888
    LDMFD SP!, {R4, PC}

    ENDFUNC                           ; end of yvu2bgr888


;==========================================================================
; FUNCTION     : yyvu2bgr888_venum
;--------------------------------------------------------------------------
; DESCRIPTION  : Perform YYVU to BGR888 conversion.
;--------------------------------------------------------------------------
; C PROTOTYPE  : void yyvu2bgr888_venum(uint8_t  *p_luma,
;                                 uint8_t  *p_chroma,
;                                 uint8_t  *p_bgr888,
;                                 uint32_t  length)
;--------------------------------------------------------------------------
; REG INPUT    : R0: uint8_t  *p_luma
;                      pointer to the input Luma Line
;                R1: uint8_t  *p_chroma
;                      pointer to the input Chroma Line
;                R2: uint8_t  *p_bgr888
;                      pointer to the output BGR Line
;                R3: uint32_t  length
;                      width of Line
;--------------------------------------------------------------------------
; STACK ARG    : None
;--------------------------------------------------------------------------
; REG OUTPUT   : None
;--------------------------------------------------------------------------
; MEM INPUT    : p_luma   - a line of luma pixels
;                p_chroma - a line of chroma pixels
;                length   - the width of the input line
;--------------------------------------------------------------------------
; MEM OUTPUT   : p_bgr888 - the converted bgr pixels
;--------------------------------------------------------------------------
; REG AFFECTED : ARM:  R0-R4
;                NEON: Q0-Q15
;--------------------------------------------------------------------------
; STACK USAGE  : none
;--------------------------------------------------------------------------
; CYCLES       : none
;
;--------------------------------------------------------------------------
; NOTES        :
;==========================================================================
yyvu2bgr888_venum  FUNCTION

    ;==========================================================================
    ; Store stack registers
    ;==========================================================================
    STMFD SP!, {R4, LR}

    PLD [R0, R3]                      ; preload luma line

    ADR   R4, constants

    VLD1.S16  {D6, D7}, [R4]!         ; D6, D7: 409 | -100 | -208 | 516 | 298 | 0 | 255 | 0
    VLD1.S32  Q15,      [R4]          ; Q15   :  -56992    |    34784   |  -70688 |     X

    ;==========================================================================
    ; Load clamping parameters to duplicate vector elements
    ;==========================================================================
    VDUP.S16  Q4,  D7[1]              ; Q4:  0  |  0  |  0  |  0  |  0  |  0  |  0  |  0
    VDUP.S16  Q5,  D7[2]              ; Q5: 255 | 255 | 255 | 255 | 255 | 255 | 255 | 255

    ;==========================================================================
    ; Read bias
    ;==========================================================================
    VDUP.S32  Q0,   Q15[0]            ; Q0:  -56992 | -56992 | -56992 | -56992
    VDUP.S32  Q1,   Q15[1]            ; Q1:   34784 |  34784 |  34784 |  34784
    VDUP.S32  Q2,   Q15[2]            ; Q2:  -70688 | -70688 | -70688 | -70688


    ;==========================================================================
    ; The main loop
    ;==========================================================================
loop_yyvu2bgr888

    ;==========================================================================
    ; Load input from Y, V and U
    ; D12, D13: Y0 Y2 Y4 Y6 Y8 Y10 Y12 Y14, Y1 Y3 Y5 Y7 Y9 Y11 Y13 Y15
    ; D14, D15: V0 V1 V2 V3 V4 V5  V6  V7 , U0 U1 U2 U3 U4 U5  U6  U7
    ;==========================================================================
    VLD2.U8  {D12,D13}, [p_luma]!     ; Load 16 Luma elements (uint8) to D12, D13
    VLD2.U8  {D14,D15}, [p_chroma]!   ; Load and scatter 8 Chroma elements pairs
                                      ; (uint8) to D14, D15

    ;==========================================================================
    ; Expand uint8 value to uint16
    ; D24, D25: Y0 Y2 Y4 Y6 Y8 Y10 Y12 Y14
    ; D26, D27: Y1 Y3 Y5 Y7 Y9 Y11 Y13 Y15
    ; D28, D29: V0 V1 V2 V3 V4 V5  V6  V7
    ; D30, D31: U0 U1 U2 U3 U4 U5  U6  U7
    ;==========================================================================
    VMOVL.U16.U8 Q12, D12
    VMOVL.U16.U8 Q13, D13
    VMOVL.U16.U8 Q14, D14
    VMOVL.U16.U8 Q15, D15

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q6, D28, D6[0]         ; Q6:  409*(V0,V1,V2,V3)     Red
    VMULL.S16  Q7, D30, D6[1]         ; Q7: -100*(U0,U1,U2,U3)     Green
    VMLAL.S16  Q7, D28, D6[2]         ; q7: -100*(U0,U1,U2,U3) - 208*(V0,V1,V2,V3)
    VMULL.S16  Q8, D30, D6[3]         ; q8:  516*(U0,U1,U2,U3)     Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q6, Q0                  ; Q6 add Red   bias -56992
    VADD.S32  Q7, Q1                  ; Q7 add Green bias  34784
    VADD.S32  Q8, Q2                  ; Q8 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMOV.S32   Q9, Q6
    VMLAL.S16  Q6, D24, D7[0]         ; Q6: R0, R2, R4, R6 in 32-bit Q8 format
    VMLAL.S16  Q9, D26, D7[0]         ; Q9: R1, R3, R5, R7 in 32-bit Q8 format

    VMOV.S32   Q10, Q7
    VMLAL.S16  Q7,  D24, D7[0]        ; Q7:  G0, G2, G4, G6 in 32-bit Q8 format
    VMLAL.S16  Q10, D26, D7[0]        ; Q10: G1, G3, G5, G7 in 32-bit Q8 format

    VMOV.S32   Q11, Q8
    VMLAL.S16  Q8,  D24, D7[0]        ; Q8:  B0, B2, B4, B6 in 32-bit Q8 format
    VMLAL.S16  Q11, D26, D7[0]        ; Q11: B1, B3, B5, B7 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D12, Q6,  #8          ; D12: R0 R2 R4 R6 in 16-bit Q0 format
    VSHRN.S32   D13, Q9,  #8          ; D13: R1 R3 R5 R7 in 16-bit Q0 format
    VZIP.16     D12, D13              ; Q6 : R0 R1 R2 R3 R4 R5 R6 R7

    VSHRN.S32   D18, Q7,  #8          ; D18: G0 G2 G4 G6 in 16-bit Q0 format
    VSHRN.S32   D19, Q10, #8          ; D19: G1 G3 G5 G7 in 16-bit Q0 format
    VZIP.16     D18, D19              ; Q9 : G0 G1 G2 G3 G4 G5 G6 G7

    VSHRN.S32   D20, Q8,  #8          ; D20: B0 B2 B4 B6 in 16-bit Q0 format
    VSHRN.S32   D21, Q11, #8          ; D21: B1 B3 B5 B7 in 16-bit Q0 format
    VZIP.16     D20, D21              ; Q10: B0 B1 B2 B3 B4 B5 B6 B7

    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D23, Q10            ; store Blue to D23, narrow the value from int16 to int8

    VMAX.S16  Q9, Q9, Q4              ; if Q9 <   0, Q9 =   0
    VMIN.S16  Q9, Q9, Q5              ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16  D22, Q9              ; store Green to D22, narrow the value from int16 to int8

    VMAX.S16  Q6, Q6, Q4              ; if Q6 <   0, Q6 =   0
    VMIN.S16  Q6, Q6, Q5              ; if Q6 > 255, Q6 = 255
    VQMOVUN.S16  D21, Q6              ; store Red to D21, narrow the value from int16 to int8

    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yyvu2bgr888         ; jump to trailing processing if remaining length is less than 8

    VST3.U8  {D21,D22,D23}, [p_rgb]!  ; vector store Blue, Green, Red to destination
                                      ; Red at LSB

    BEQ  end_yyvu2bgr888              ; done if exactly 8 pixels processed in the loop

    ;==========================================================================
    ; Done with the first 8 elements, continue on the next 8 elements
    ;==========================================================================

    ;==========================================================================
    ; Multiply contribution from chrominance, results are in 32-bit
    ;==========================================================================
    VMULL.S16  Q6, D29, D6[0]         ; Q6: 409*(V4,V5,V6,V7)       Red
    VMULL.S16  Q7, D31, D6[1]         ; Q7: -100*(U4,U5,U6,U7)      Green
    VMLAL.S16  Q7, D29, D6[2]         ; Q7: -100*(U4,U5,U6,U7) - 208*(V4,V5,V6,V7)
    VMULL.S16  Q8, D31, D6[3]         ; Q8: 516*(U4,U5,U6,U7)       Blue

    ;==========================================================================
    ; Add bias
    ;==========================================================================
    VADD.S32  Q6, Q0                  ; Q6 add Red   bias -56992
    VADD.S32  Q7, Q1                  ; Q7 add Green bias  34784
    VADD.S32  Q8, Q2                  ; Q8 add Blue  bias -70688

    ;==========================================================================
    ; Calculate Red, Green, Blue
    ;==========================================================================
    VMOV.S32   Q9, Q6
    VMLAL.S16  Q6, D25, D7[0]         ; Q6: R8 R10 R12 R14 in 32-bit Q8 format
    VMLAL.S16  Q9, D27, D7[0]         ; Q9: R9 R11 R13 R15 in 32-bit Q8 format

    VMOV.S32   Q10, Q7
    VMLAL.S16  Q7,  D25, D7[0]        ; Q7: G0, G2, G4, G6 in 32-bit Q8 format
    VMLAL.S16  Q10, D27, D7[0]        ; Q10 : G1, G3, G5, G7 in 32-bit Q8 format

    VMOV.S32   Q11, Q8
    VMLAL.S16  Q8,  D25, D7[0]        ; Q8: B0, B2, B4, B6 in 32-bit Q8 format
    VMLAL.S16  Q11, D27, D7[0]        ; Q11 : B1, B3, B5, B7 in 32-bit Q8 format

    ;==========================================================================
    ; Right shift eight bits with rounding
    ;==========================================================================
    VSHRN.S32   D12, Q6,  #8          ; D12: R8 R10 R12 R14 in 16-bit Q0 format
    VSHRN.S32   D13, Q9,  #8          ; D13: R9 R11 R13 R15 in 16-bit Q0 format
    VZIP.16     D12, D13              ; Q6: R8 R9 R10 R11 R12 R13 R14 R15

    VSHRN.S32   D18, Q7,  #8          ; D18: G8 G10 G12 G14 in 16-bit Q0 format
    VSHRN.S32   D19, Q10, #8          ; D19: G9 G11 G13 G15 in 16-bit Q0 format
    VZIP.16     D18, D19              ; Q9:  G8 G9 G10 G11 G12 G13 G14 G15

    VSHRN.S32   D20, Q8,  #8          ; D20: B8 B10 B12 B14 in 16-bit Q0 format
    VSHRN.S32   D21, Q11, #8          ; D21: B9 B11 B13 B15 in 16-bit Q0 format
    VZIP.16     D20, D21              ; Q10: B8 B9 B10 B11 B12 B13 B14 B15
    ;==========================================================================
    ; Clamp the value to be within [0~255]
    ;==========================================================================
    VMAX.S16  Q10, Q10, Q4            ; if Q10 <   0, Q10 =   0
    VMIN.S16  Q10, Q10, Q5            ; if Q10 > 255, Q10 = 255
    VQMOVUN.S16   D23, Q10            ; store Blue to D23, narrow the value from int16 to int8

    VMAX.S16  Q9, Q9, Q4              ; if Q9 <   0, Q9 =   0
    VMIN.S16  Q9, Q9, Q5              ; if Q9 > 255, Q9 = 255
    VQMOVUN.S16  D22, Q9              ; store Green to D22, narrow the value from int16 to int8

    VMAX.S16  Q6, Q6, Q4              ; if Q6 <   0, Q6 =   0
    VMIN.S16  Q6, Q6, Q5              ; if Q6 > 255, Q6 = 255
    VQMOVUN.S16  D21, Q6              ; store Red to D21, narrow the value from int16 to int8


    SUBS length, length, #8           ; check if the length is less than 8

    BMI  trailing_yyvu2bgr888         ; jump to trailing processing if remaining length is less than 8

    VST3.U8  {D21,D22,D23}, [p_rgb]!  ; vector store Blue, Green, Red to destination
                                      ; Red at LSB

    BHI loop_yyvu2bgr888              ; loop if more than 8 pixels left

    BEQ  end_yyvu2bgr888              ; done if exactly 8 pixels processed in the loop


trailing_yyvu2bgr888
    ;==========================================================================
    ; There are from 1 ~ 7 pixels left in the trailing part.
    ; First adding 7 to the length so the length would be from 0 ~ 6.
    ; eg: 1 pixel left in the trailing part, so 1-8+7 = 0.
    ; Then save 1 pixel unconditionally since at least 1 pixel left in the
    ; trailing part.
    ;==========================================================================
    ADDS length, length, #7           ; there are 7 or less in the trailing part

    VST3.U8 {D21[0],D22[0],D23[0]}, [p_rgb]! ; at least 1 pixel left in the trailing part
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[1],D22[1],D23[1]}, [p_rgb]!  ; store one more pixel
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[2],D22[2],D23[2]}, [p_rgb]!  ; store one more pixel
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[3],D22[3],D23[3]}, [p_rgb]!  ; store one more pixel
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[4],D22[4],D23[4]}, [p_rgb]!  ; store one more pixel
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[5],D22[5],D23[5]}, [p_rgb]!  ; store one more pixel
    BEQ end_yyvu2bgr888               ; done if 0 pixel left

    SUBS length, length, #1           ; update length counter
    VST3.U8 {D21[6],D22[6],D23[6]}, [p_rgb]!  ; store one more pixel

end_yyvu2bgr888
    LDMFD SP!, {R4, PC}

    ENDFUNC                           ; end of yyvu2bgr888

    END
