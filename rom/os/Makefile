CA65 = ca65
LD65 = ld65

CFG := ue65c02.cfg
SRC_DIR := src
BUILD_DIR := build
OBJ_DIR := $(BUILD_DIR)/obj

BASIC_DIR := ../basic
BASIC_CFG := $(BASIC_DIR)/ue65c02-basic.cfg
BASIC_SRC_DIR := $(BASIC_DIR)/src
BASIC_BUILD_DIR := $(BASIC_DIR)/build
BASIC_OBJ_DIR := $(BASIC_BUILD_DIR)/obj

INCLUDES := -I ./src/include

# --cpu wird in den AFLAGS nicht gesetzt, da .setcpu "65816" in den sources gesetzt wird (s. cpu.inc).
AFLAGS = --debug-info $(INCLUDES)
LDFLAGS = -C $(CFG) --dbgfile $(BUILD_DIR)/os.dbg

SRCS := $(SRC_DIR)/kernel.s \
	$(filter-out $(SRC_DIR)/kernel.s,$(wildcard $(SRC_DIR)/*.s))
OBJS := $(patsubst $(SRC_DIR)/%.s,$(OBJ_DIR)/%.o,$(SRCS))
DEPS := $(OBJS:.o=.d)
RAW_IMAGE := $(BUILD_DIR)/os.raw
AUTO_RAW_IMAGE := $(RAW_IMAGE).bin
ROM_TARGET := $(BUILD_DIR)/os.bin
BASIC_IMAGE := $(BASIC_BUILD_DIR)/basic.bin
BASIC_SOURCES := $(wildcard $(BASIC_SRC_DIR)/*.s $(BASIC_CFG) $(BASIC_DIR)/Makefile)
COMBINED_IMAGE := $(BUILD_DIR)/rom16.bin
EPROMSIZE := 32768
ROM_OFFSET := 16384
BASIC_OFFSET := 0
OS_OFFSET := 10496

.PHONY: all clean burn

all: $(ROM_TARGET)

-include $(DEPS)

$(BUILD_DIR) $(OBJ_DIR):
	@mkdir -p $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.s | $(OBJ_DIR)
	$(CA65) $(AFLAGS) --create-dep $(OBJ_DIR)/$*.d $< -o $@

$(BASIC_IMAGE): $(BASIC_SOURCES)
	$(MAKE) -C $(BASIC_DIR)

$(RAW_IMAGE): $(OBJS) | $(BUILD_DIR)
	$(LD65) $(LDFLAGS) -o $@ $(OBJS)
	cp $(AUTO_RAW_IMAGE) $@
	rm -f $(AUTO_RAW_IMAGE)

$(COMBINED_IMAGE): $(BASIC_IMAGE) $(RAW_IMAGE)
	python3 -c "from pathlib import Path; basic = Path('$(BASIC_IMAGE)').read_bytes(); os = Path('$(RAW_IMAGE)').read_bytes(); out = bytearray([0xFF]) * 16384; out[$(BASIC_OFFSET):$(BASIC_OFFSET)+len(basic)] = basic; out[$(OS_OFFSET):$(OS_OFFSET)+len(os)] = os; Path('$(COMBINED_IMAGE)').write_bytes(out)"

$(ROM_TARGET): $(COMBINED_IMAGE)
	python3 -c "from pathlib import Path; rom = Path('$(COMBINED_IMAGE)').read_bytes(); out = bytearray([0xFF]) * $(EPROMSIZE); out[$(ROM_OFFSET):$(ROM_OFFSET)+len(rom)] = rom; Path('$(ROM_TARGET)').write_bytes(out)"

burn: $(ROM_TARGET)
	minipro -p at28c256 -u -w $(ROM_TARGET)

clean:
	rm -rf $(BASIC_BUILD_DIR)
	rm -rf $(BUILD_DIR)
