# Copyright (c) 2010, Code Aurora Forum. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#     * Neither the name of Code Aurora Forum, Inc. nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT
# ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
# BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

ARM_COMPILER_VERSION  = 3.1
ARM_COMPILER_REVISION = BLD640
ARM_HOST              = linux-pentium
ARM_BUILD             = 569

# Set ARM toolchain paths
ARMROOT =
ARMBIN  =
ARMINC  =

ARMASM = $(ARMBIN)/armasm
ARMCC  = $(ARMBIN)/armcc
ARMAR  = $(ARMBIN)/armar

CPU = Cortex-A8

ARM_ASMFLAGS = --cpu=$(CPU) --fpu=none --apcs /interwork -g --diag_warning 1356 \
               --no_hide_all --list $(@D)/$(*F).lst --width 255 --length 0
ARM_CFLAGS   = --cpu=$(CPU) --fpu=none -O3 -Otime --asm --interleave --gnu --wchar32 \
               --enum_is_int --library_interface=aeabi_glibc --apcs /noswst/interwork/fpic \
	       --no_hide_all --littleend --dwarf2

JPEG_VENUM_LIB = ../../precompiled/libjpeg-venum.a

ASM_SRCS  = jdidct-armv7.s
ASM_SRCS += jdcolor-armv7.s

OBJDIR = obj

LSTS = $(ASM_SRCS:%.s=$(OBJDIR)/%.lst)
OBJS = $(ASM_SRCS:%.s=$(OBJDIR)/%.o)

all: $(JPEG_VENUM_LIB)

$(JPEG_VENUM_LIB): $(OBJS)
	$(ARMAR) -rcs $(JPEG_VENUM_LIB) $^
	rm -rf $(OBJS) $(LSTS)
	rm -rf $(OBJDIR)

$(OBJDIR)/%.o: %.s
	@mkdir -p $(OBJDIR)
	$(ARMASM) $(ARM_ASMFLAGS) -o $@ $<

clean:
	rm -rf $(JPEG_VENUM_LIB) $(OBJS) $(LSTS)
