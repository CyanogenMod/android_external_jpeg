LIB_JPEG_PATH:= $(call my-dir)

# ---------------------------------------------------------------------------------
#      Build options
# ---------------------------------------------------------------------------------

# Set ANDROID_JPEG_USE_VENUM to true to enable VeNum instructions to accelerate libjpeg.
ANDROID_JPEG_USE_VENUM := true

# Disable VeNum instructions if they are not supported on the build target
ifneq ($(ARCH_ARM_HAVE_VFP),true)
ANDROID_JPEG_USE_VENUM := false
else
ifneq ($(ARCH_ARM_HAVE_NEON),true)
ANDROID_JPEG_USE_VENUM := false
endif
endif

# ---------------------------------------------------------------------------------
#      Make the prebuilt library (libjpeg-venum)
# ---------------------------------------------------------------------------------
ifeq ($(ANDROID_JPEG_USE_VENUM),true)

# Include precompiled static library
include $(CLEAR_VARS)
LOCAL_PATH           := $(LIB_JPEG_PATH)/precompiled
LOCAL_PREBUILT_LIBS  := libjpeg-venum.a
include $(BUILD_MULTI_PREBUILT)

endif #  ANDROID_JPEG_USE_VENUM


common_SRC_FILES := \
	jcapimin.c jcapistd.c jccoefct.c jccolor.c jcdctmgr.c jchuff.c \
	jcinit.c jcmainct.c jcmarker.c jcmaster.c jcomapi.c jcparam.c \
	jcphuff.c jcprepct.c jcsample.c jctrans.c jdapimin.c jdapistd.c \
	jdatadst.c jdatasrc.c jdcoefct.c jdcolor.c jddctmgr.c jdhuff.c \
	jdinput.c jdmainct.c jdmarker.c jdmaster.c jdmerge.c jdphuff.c \
	jdpostct.c jdsample.c jdtrans.c jerror.c jfdctflt.c jfdctfst.c \
	jfdctint.c jidctflt.c jquant1.c \
	jquant2.c jutils.c jmemmgr.c \
	jmem-android.c

# the assembler is only for the ARM version, don't break the Linux sim
ifneq ($(TARGET_ARCH),arm)
ANDROID_JPEG_NO_ASSEMBLER := true
endif

# temp fix until we understand why this broke cnn.com
#ANDROID_JPEG_NO_ASSEMBLER := true

ifeq ($(strip $(ANDROID_JPEG_NO_ASSEMBLER)),true)
common_SRC_FILES += jidctint.c jidctfst.c jidctred.c
else
ifeq ($(ANDROID_JPEG_USE_VENUM),true)
common_SRC_FILES += jidctvenum.c
else # ANDROID_JPEG_USE_VENUM, false
common_SRC_FILES += jidctint.c jidctfst.S jidctred.c
endif # ANDROID_JPEG_USE_VENUM
endif

common_CFLAGS := -DAVOID_TABLES
common_CFLAGS += -O3 -fstrict-aliasing -fprefetch-loop-arrays
#common_CFLAGS += -march=armv6j

ifeq ($(ANDROID_JPEG_USE_VENUM),true)
common_CFLAGS += -DANDROID_JPEG_USE_VENUM
endif

# ---------------------------------------------------------------------------------
#      Build libjpeg
# ---------------------------------------------------------------------------------
include $(CLEAR_VARS)

LOCAL_PATH:= $(LIB_JPEG_PATH)

LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := $(common_SRC_FILES)
LOCAL_CFLAGS += $(common_CFLAGS)

ifeq ($(ANDROID_JPEG_USE_VENUM),true)
LOCAL_WHOLE_STATIC_LIBRARIES := libjpeg-venum
endif

LOCAL_MODULE:= libjpeg

include $(BUILD_STATIC_LIBRARY)


include $(CLEAR_VARS)

LOCAL_PATH:= $(LIB_JPEG_PATH)

LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := $(common_SRC_FILES)
LOCAL_CFLAGS += $(common_CFLAGS)

ifeq ($(ANDROID_JPEG_USE_VENUM),true)
LOCAL_WHOLE_STATIC_LIBRARIES := libjpeg-venum
endif

LOCAL_MODULE:= libjpeg

LOCAL_PRELINK_MODULE := false

include $(BUILD_SHARED_LIBRARY)
