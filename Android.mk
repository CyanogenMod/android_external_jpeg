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

# use ashmem as libjpeg decoder's backing store
LOCAL_CFLAGS += -DUSE_ANDROID_ASHMEM
LOCAL_SRC_FILES += \
	jmem-ashmem.c

# the original android memory manager.
# use sdcard as libjpeg decoder's backing store
#LOCAL_SRC_FILES += \
#	jmem-android.c

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

ifdef NEEDS_ARM_ERRATA_754319_754320
asm_flags := \
	--defsym NEEDS_ARM_ERRATA_754319_754320_ASM=1

LOCAL_CFLAGS+= \
	$(foreach f,$(asm_flags),-Wa,"$(f)")
endif

# enable armv6 idct assembly
LOCAL_CFLAGS += -DANDROID_ARMV6_IDCT

LOCAL_MODULE:= libjpeg

#ifneq(, $(filter userdebug eng, $(TARGET_BUILD_VARIANT)))
ifneq ($(filter eng userdebug, $(TARGET_BUILD_VARIANT)),)
  LOCAL_STRIP_MODULE := false
endif

LOCAL_SHARED_LIBRARIES := \
	libcutils

include $(BUILD_SHARED_LIBRARY)
