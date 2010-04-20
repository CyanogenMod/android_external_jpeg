LOCAL_PATH:= $(call my-dir)

common_SRC_FILES := \
	jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c \
	jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c \
	jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c \
	jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c \
	jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c \
	jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c \
	jfdctint.c jidctflt.c jidctred.c jquant1.c \
	jquant2.c jutils.c jmemmgr.c \
	jmem-android.c

# the assembler is only for the ARM version, don't break the Linux sim
ifneq ($(TARGET_ARCH),arm)
ANDROID_JPEG_NO_ASSEMBLER := true
endif

# temp fix until we understand why this broke cnn.com
#ANDROID_JPEG_NO_ASSEMBLER := true

ifeq ($(strip $(ANDROID_JPEG_NO_ASSEMBLER)),true)
common_SRC_FILES += jidctint.c jidctfst.c
else
common_SRC_FILES += jidctint.c jidctfst.S
endif

common_CFLAGS := -DAVOID_TABLES
common_CFLAGS += -O3 -fstrict-aliasing -fprefetch-loop-arrays
#common_CFLAGS += -march=armv6j

include $(CLEAR_VARS)

LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := $(common_SRC_FILES)
LOCAL_CFLAGS += $(common_CFLAGS)

LOCAL_MODULE:= libjpeg

include $(BUILD_STATIC_LIBRARY)


include $(CLEAR_VARS)

LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := $(common_SRC_FILES)
LOCAL_CFLAGS += $(common_CFLAGS)

LOCAL_MODULE:= libjpeg

LOCAL_PRELINK_MODULE := false

include $(BUILD_SHARED_LIBRARY)
