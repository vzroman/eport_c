# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

CJSON_INCLUDE_DIR ?= $(CURDIR)/cJSON/build/_install/include
CJSON_LIB64_DIR ?= $(CURDIR)/cJSON/build/_install/lib64
CJSON_LIB_DIR ?= $(CURDIR)/cJSON/build/_install/lib

EPORT_C_INCLUDE_DIR ?= $(CURDIR)/eport_c/include

INCLUDE_DIR = -I $(CJSON_INCLUDE_DIR) -I $(EPORT_C_INCLUDE_DIR)

C_SRC_DIR = $(CURDIR)
INSTALL_DIR = $(BASEDIR)/priv
C_SRC_OUTPUT ?= $(INSTALL_DIR)/lib$(PROJECT).a

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -arch x86_64 -finline-functions -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -Wall
endif

CC += -shared -o

CFLAGS += -fPIC $(INCLUDE_DIR)
CXXFLAGS += -fPIC $(INCLUDE_DIR)

LDLIBS += -L $(CJSON_LIB_DIR) -L $(CJSON_LIB64_DIR) -lcjson -lcjson_utils
# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type d \( -name "cJSON" \) -prune -false -o -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

build: $(C_SRC_OUTPUT)
	@mkdir -p $(BASEDIR)/priv/
	@cp $(CJSON_LIB_DIR)/libcjson.a $(INSTALL_DIR)
	@cp $(CJSON_LIB_DIR)/libcjson_utils.a $(INSTALL_DIR)
	@cp -R $(EPORT_C_INCLUDE_DIR) $(INSTALL_DIR)/include
	@cp -R $(CJSON_INCLUDE_DIR)/cjson $(INSTALL_DIR)/include

$(C_SRC_OUTPUT): $(OBJECTS)
	$(AR) rcs $@ $<

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -rf $(C_SRC_OUTPUT) $(OBJECTS) $(INSTALL_DIR)/include $(INSTALL_DIR)/libcjson.a $(INSTALL_DIR)/libcjson_utils.a
