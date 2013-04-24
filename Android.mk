LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_ARM_MODE := arm

# Set ANDROID_JPEG_USE_VENUM to true to enable VeNum optimizations
ANDROID_JPEG_USE_VENUM := true

# Disable VeNum optimizations if they are not supported on the build target
ifneq ($(ARCH_ARM_HAVE_VFP),true)
ANDROID_JPEG_USE_VENUM := false
else
ifneq ($(ARCH_ARM_HAVE_NEON),true)
ANDROID_JPEG_USE_VENUM := false
endif
endif

LOCAL_SRC_FILES := \
	jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c \
	jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c \
	jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c \
	jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c \
	jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c \
	jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c \
	jfdctint.c jidctflt.c jquant1.c \
	jquant2.c jutils.c jmemmgr.c armv6_idct.S

ifeq (,$(TARGET_BUILD_APPS))
# building against master
# use ashmem as libjpeg decoder's backing store
LOCAL_CFLAGS += -DUSE_ANDROID_ASHMEM
LOCAL_SRC_FILES += \
    jmem-ashmem.c
else
# unbundled branch, built against NDK.
LOCAL_SDK_VERSION := 17
# the original android memory manager.
# use sdcard as libjpeg decoder's backing store
LOCAL_SRC_FILES += \
    jmem-android.c
endif

ifeq ($(ANDROID_JPEG_USE_VENUM),true)
LOCAL_SRC_FILES += jidctvenum.c
LOCAL_SRC_FILES += asm/armv7/jdcolor-armv7.S
LOCAL_SRC_FILES += asm/armv7/jdcolor-android-armv7.S
LOCAL_SRC_FILES += asm/armv7/jdidct-armv7.S
LOCAL_CFLAGS    += -DANDROID_JPEG_USE_VENUM
else # ANDROID_JPEG_USE_VENUM, false
LOCAL_SRC_FILES += jidctint.c jidctfst.c jidctred.c
endif # ANDROID_JPEG_USE_VENUM

LOCAL_CFLAGS += -DAVOID_TABLES 
LOCAL_CFLAGS += -O3 -fstrict-aliasing -fprefetch-loop-arrays

# enable tile based decode
LOCAL_CFLAGS += -DANDROID_TILE_BASED_DECODE

ifeq ($(TARGET_ARCH_VARIANT),x86-atom)
  LOCAL_CFLAGS += -DANDROID_INTELSSE2_IDCT
  LOCAL_SRC_FILES += jidctintelsse.c
endif

# enable armv6 idct assembly
ifeq ($(strip $(TARGET_ARCH)),arm)
  LOCAL_CFLAGS += -DANDROID_ARMV6_IDCT
endif

# use mips assembler IDCT implementation if MIPS DSP-ASE is present
ifeq ($(strip $(TARGET_ARCH)),mips)
  ifeq ($(strip $(ARCH_MIPS_HAS_DSP)),true)
  LOCAL_CFLAGS += -DANDROID_MIPS_IDCT
  LOCAL_SRC_FILES += \
      mips_jidctfst.c \
      mips_idct_le.S
  endif
endif

LOCAL_MODULE := libjpeg_static

include $(BUILD_STATIC_LIBRARY)



# Build shared library
include $(CLEAR_VARS)

LOCAL_MODULE := libjpeg

LOCAL_MODULE_TAGS := optional

LOCAL_LDFLAGS += -Wl,--no-fatal-warnings

LOCAL_WHOLE_STATIC_LIBRARIES = libjpeg_static

ifeq (,$(TARGET_BUILD_APPS))
#ifneq(, $(filter userdebug eng, $(TARGET_BUILD_VARIANT)))
ifneq ($(filter eng userdebug, $(TARGET_BUILD_VARIANT)),)
  LOCAL_STRIP_MODULE := false
endif

LOCAL_SHARED_LIBRARIES := \
    libcutils
else
# unbundled branch, built against NDK.
LOCAL_SDK_VERSION := 17
endif

include $(BUILD_SHARED_LIBRARY)

include $(CLEAR_VARS)
LOCAL_ARM_MODE := arm
LOCAL_SRC_FILES := \
	cjpeg.c cdjpeg.h jinclude.h jconfig.h jpeglib.h jmorecfg.h jerror.h cderror.h jversion.h rdswitch.c cdjpeg.c rdtarga.c rdppm.c rdgif.c rdbmp.c
LOCAL_MODULE:= cjpeg
LOCAL_MODULE_TAGS := eng
LOCAL_SHARED_LIBRARIES := libc libcutils libjpeg
include $(BUILD_EXECUTABLE)

include $(CLEAR_VARS)
LOCAL_ARM_MODE := arm
LOCAL_SRC_FILES := \
	djpeg.c cdjpeg.h jinclude.h jconfig.h jpeglib.h jmorecfg.h jerror.h cderror.h jversion.h cdjpeg.c wrppm.c wrgif.c wrbmp.c rdcolmap.c wrtarga.c
LOCAL_MODULE:= djpeg
LOCAL_MODULE_TAGS := eng
LOCAL_SHARED_LIBRARIES := libc libcutils libjpeg
include $(BUILD_EXECUTABLE)
