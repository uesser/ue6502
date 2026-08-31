CA65 = ca65
LD65 = ld65

BUILD_DIR := build
OBJ_DIR := $(BUILD_DIR)/obj
SRC_DIR := src
CFG := ue65c02.cfg
INCLUDES := -I ./src/include

# --cpu 65816 muss gesetzt werden, weil die Original 65C02 CPU den opcode wai nicht kannte,
# nur die 65C02W. Diese CPU kennt aber der ca65 nicht. Da 65C02W und 65816 opcode äquivalent sind,
# ist es ok, hier 65816 zu setzen.
AFLAGS = --cpu 65816 --debug-info $(INCLUDES)
LDFLAGS = -C $(CFG) --dbgfile $(BUILD_DIR)/os.dbg

SRCS := $(SRC_DIR)/kernel.s \
	$(filter-out $(SRC_DIR)/kernel.s,$(wildcard $(SRC_DIR)/*.s))
OBJS := $(patsubst $(SRC_DIR)/%.s,$(OBJ_DIR)/%.o,$(SRCS))
DEPS := $(OBJS:.o=.d)
RAW_IMAGE := $(BUILD_DIR)/os.raw
AUTO_RAW_IMAGE := $(RAW_IMAGE).bin
ROM_TARGET := $(BUILD_DIR)/os.bin
EPROMSIZE := 32768
ROM_OFFSET := 16384

.PHONY: all clean burn

all: $(ROM_TARGET)

-include $(DEPS)

$(BUILD_DIR) $(OBJ_DIR):
	@mkdir -p $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.s | $(OBJ_DIR)
	$(CA65) $(AFLAGS) --create-dep $(OBJ_DIR)/$*.d $< -o $@

$(RAW_IMAGE): $(OBJS) | $(BUILD_DIR)
	$(LD65) $(LDFLAGS) -o $@ $(OBJS)
	cp $(AUTO_RAW_IMAGE) $@
	rm -f $(AUTO_RAW_IMAGE)

$(ROM_TARGET): $(RAW_IMAGE)
	python3 -c "from pathlib import Path; rom = Path('$(RAW_IMAGE)').read_bytes(); out = bytearray([0xFF]) * $(EPROMSIZE); out[$(ROM_OFFSET):$(ROM_OFFSET)+len(rom)] = rom; Path('$(ROM_TARGET)').write_bytes(out)"

burn: $(ROM_TARGET)
	minipro -p at28c256 -u -w $(ROM_TARGET)

clean:
	rm -rf $(BUILD_DIR)
