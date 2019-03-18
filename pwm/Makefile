# Project setup
PROJ      = pwm
BUILD     = ./build
DEVICE    = 1k
FOOTPRINT = tq144
PINMAP    = icestick.pcf

CLASH = clash

# Files
FILES = src/Top.hs

.PHONY: all clean verilog bitstream burn

all: bitstream

verilog: $(FILES)
	# if build folder doesn't exist, create it
	mkdir -p $(BUILD)
	# TODO copy native verilog files over
	$(CLASH) -v --verilog $(FILES) -outputdir $(BUILD)

bitstream: verilog
	# synthesize using Yosys
	yosys -p "synth_ice40 -blif $(BUILD)/$(PROJ).blif" $$(find $(BUILD)/verilog/ -type f -iname '*.v' | grep -v testbench.v)
	# Place and route using arachne
	arachne-pnr -d $(DEVICE) -P $(FOOTPRINT) -o $(BUILD)/$(PROJ).asc -p $(PINMAP) $(BUILD)/$(PROJ).blif
	# Convert to bitstream using IcePack
	icepack $(BUILD)/$(PROJ).asc $(BUILD)/$(PROJ).bin

burn: bitstream
	iceprog $(BUILD)/$(PROJ).bin

show: verilog
	yosys -p "show" $$(find $(BUILD)/verilog/ -type f -iname '*.v' | grep -v testbench.v)

clean:
	rm ./$(BUILD)/* -rf
