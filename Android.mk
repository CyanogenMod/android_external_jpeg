LIB_JPEG_PATH := $(call my-dir)
include $(CLEAR_VARS)

# ---------------------------------------------------------------------------------
#      Build options
# ---------------------------------------------------------------------------------

# Set QC_LIBS_AVAILABLE to true if QC proprietary library libjpeg-rvct is
# available.  QC libs are only supported on Scorpion processors.
QC_LIBS_AVAILABLE := false

# Verify that QC libs is supported for this target.
ifneq "$(TARGET_ARCH_VARIANT)" "armv7-a"
QC_LIBS_AVAILABLE := false
endif

# ---------------------------------------------------------------------------------
#      Make the prebuilt library (libjpeg-rvct)
# ---------------------------------------------------------------------------------

include $(CLEAR_VARS)
LOCAL_PATH:= $(LIB_JPEG_PATH)

ifeq ($(QC_LIBS_AVAILABLE),true)

LOCAL_PATH := $(LIB_JPEG_PATH)/precompiled
LOCAL_PREBUILT_LIBS := libjpeg-rvct.so
LOCAL_PRELINK_MODULE   := false

include $(BUILD_MULTI_PREBUILT)

endif #  QC_LIBS_AVAILABLE

# ---------------------------------------------------------------------------------
#      Build libjpeg
# ---------------------------------------------------------------------------------
include $(CLEAR_VARS)
LOCAL_PATH:= $(LIB_JPEG_PATH)

LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := \
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

ifeq ($(QC_LIBS_AVAILABLE),true)
LOCAL_SRC_FILES += jidctqc.c
else # QC_LIBS_AVAILABLE, false
LOCAL_SRC_FILES += jidctred.c
ifeq ($(strip $(ANDROID_JPEG_NO_ASSEMBLER)),true)
LOCAL_SRC_FILES += jidctint.c jidctfst.c
else
LOCAL_SRC_FILES += jidctint.c jidctfst.S
endif
endif # QC_LIBS_AVAILABLE

LOCAL_CFLAGS += -DAVOID_TABLES 
LOCAL_CFLAGS += -O3 -fstrict-aliasing -fprefetch-loop-arrays
#LOCAL_CFLAGS += -march=armv6j

ifeq ($(QC_LIBS_AVAILABLE),true)
LOCAL_CFLAGS           += -DQC_LIBS_SUPPORTED -DQC_SHARED_LIB_NAME=\"libjpeg-rvct.so\"
LOCAL_SHARED_LIBRARIES := libjpeg-rvct
endif

LOCAL_MODULE:= libjpeg

include $(BUILD_STATIC_LIBRARY)
