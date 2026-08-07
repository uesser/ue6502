# make -d --dry-run
# -d == debug all

LDCFG = ue65c02.cfg
LDMAP = os.map
LDLBL = os.lbl
TARGET = os

ASFLAGS = -t none --cpu 65c02

VPATH = src src/include

SRCS = kernel.s \
	   sysram.s \
	   kernelUtils.s \
	   via.s \
	   acia.s \
	   keyboard.s \
	   lcd.s

OBJDIR = ./obj
OBJS := $(addprefix $(OBJDIR)/,$(patsubst %.s,%.o,$(SRCS)))

all: $(TARGET)

$(OBJS): $(SRCS)
	ca65 $(ASFLAGS) -I ./src/include -o $@ $<

$(TARGET): $(OBJS)
	ld65 -C ./$(LDCFG) -v -Ln $(LDLBL) -vm -m $(LDMAP) -o $(TARGET) $(OBJS)

$(OBJS): | $(OBJDIR)

$(OBJDIR):
	mkdir $(OBJDIR)
		
.PHONY: clean
clean:
	rm -f $(TARGET).rom $(LDLBL) $(LDMAP) $(OBJS)
