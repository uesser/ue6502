CA65 = ca65
LD65 = ld65

# Values to fit os and basic into the same EPROM, which size is 32kB (32768 bytes), but only the upper 16kB of EPROM used.
# The OS is placed at 0x2900 (10496) and the BASIC at 0x0000 (0), overall at an offset of 0x4000 (16384) (upper 16kB).
EPROMSIZE := 32768
ROM_OFFSET := 16384
BASIC_OFFSET := 0
OS_OFFSET := 10496

BUILD_DIR := build

OS_DIR := rom/os
OS_CFG := $(OS_DIR)/ue65c02.cfg
OS_SRC_DIR := $(OS_DIR)/src
OS_BUILD_DIR := $(OS_DIR)/build
OS_OBJ_DIR := $(OS_BUILD_DIR)/obj
OS_INCLUDES := -I $(OS_SRC_DIR)/include
OS_SRCS := $(OS_SRC_DIR)/kernel.s \
	$(filter-out $(OS_SRC_DIR)/kernel.s,$(wildcard $(OS_SRC_DIR)/*.s))
OS_OBJS := $(patsubst $(OS_SRC_DIR)/%.s,$(OS_OBJ_DIR)/%.o,$(OS_SRCS))
OS_DEPS := $(OS_OBJS:.o=.d)
OS_LD_IMAGE := $(OS_BUILD_DIR)/os
OS_IMAGE := $(OS_BUILD_DIR)/os.bin

BASIC_DIR := rom/basic
BASIC_CFG := $(BASIC_DIR)/ue65c02-basic.cfg
BASIC_SRC_DIR := $(BASIC_DIR)/src
BASIC_INCLUDE_DIR := $(BASIC_DIR)/src/include
BASIC_BUILD_DIR := $(BASIC_DIR)/build
BASIC_SRCS := $(wildcard $(BASIC_SRC_DIR)/*.s $(BASIC_INCLUDE_DIR)/*.* $(BASIC_CFG) $(BASIC_DIR)/Makefile)
BASIC_IMAGE := $(BASIC_BUILD_DIR)/basic.bin

ROM_16_TARGET := $(BUILD_DIR)/rom16.bin
ROM_32_TARGET := $(BUILD_DIR)/rom32.bin

# --cpu wird in den AFLAGS nicht gesetzt, da .setcpu "65816" in den sources gesetzt wird (s. cpu.inc).
AFLAGS = --debug-info $(OS_INCLUDES)
LDFLAGS = -C $(OS_CFG) --dbgfile $(OS_BUILD_DIR)/os.dbg

.PHONY: all clean burn

all: $(ROM_32_TARGET)

-include $(OS_DEPS)

# Alle anzulegenden Build/Obj-Verzeichnisse
ALL_BUILD_DIRS := $(BUILD_DIR) $(OS_BUILD_DIR) $(OS_OBJ_DIR)
$(ALL_BUILD_DIRS):
	@mkdir -p $@

$(OS_OBJ_DIR)/%.o: $(OS_SRC_DIR)/%.s | $(OS_OBJ_DIR)
	$(CA65) $(AFLAGS) --create-dep $(OS_OBJ_DIR)/$*.d $< -o $@

$(BASIC_IMAGE): $(BASIC_SRCS)
	$(MAKE) -C $(BASIC_DIR)

$(OS_IMAGE): $(OS_OBJS) $(OS_CFG) | $(OS_BUILD_DIR)
	$(LD65) $(LDFLAGS) -o $(OS_LD_IMAGE) $(OS_OBJS)
	rm -f $(OS_LD_IMAGE)

define CREATE_ROM_16_SCRIPT
from pathlib import Path
basic = Path(r'$(BASIC_IMAGE)').read_bytes()
os = Path(r'$(OS_IMAGE)').read_bytes()
out = bytearray([0xFF]) * 16384
out[$(BASIC_OFFSET):$(BASIC_OFFSET)+len(basic)] = basic
out[$(OS_OFFSET):$(OS_OFFSET)+len(os)] = os
Path(r'$(ROM_16_TARGET)').write_bytes(out)
endef
export CREATE_ROM_16_SCRIPT

$(ROM_16_TARGET): $(OS_IMAGE) $(BASIC_IMAGE) | $(BUILD_DIR)
	python3 -c "$$CREATE_ROM_16_SCRIPT"

define CREATE_ROM_32_SCRIPT
from pathlib import Path
rom = Path(r'$(ROM_16_TARGET)').read_bytes()
out = bytearray([0xFF]) * $(EPROMSIZE)
out[$(ROM_OFFSET):$(ROM_OFFSET)+len(rom)] = rom
Path(r'$(ROM_32_TARGET)').write_bytes(out)
endef
export CREATE_ROM_32_SCRIPT

$(ROM_32_TARGET): $(ROM_16_TARGET)
	python3 -c "$$CREATE_ROM_32_SCRIPT"

burn: $(ROM_32_TARGET)
	minipro -p at28c256 -u -w $(ROM_32_TARGET)

clean:
	rm -rf $(BASIC_BUILD_DIR)
	rm -rf $(OS_BUILD_DIR)
	rm -rf $(BUILD_DIR)
