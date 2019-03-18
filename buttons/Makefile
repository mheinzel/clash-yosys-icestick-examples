# Project setup
PROJ      = buttons
BUILD     = ./build
DEVICE    = 1k
FOOTPRINT = tq144
PINMAP    = icestick.pcf

CLASH = clash -isrc

# Files
FILES_HS = src/Top.hs
FILES_V  = $(shell find verilog -type f -name "*.v")

.PHONY: all clean verilog bitstream burn

all: bitstream

verilog: $(FILES_HS) $(FILES_V)
	# if build folder doesn't exist, create it
	mkdir -p $(BUILD)
	mkdir -p $(BUILD)/verilog
	if [ -e verilog ]; then cp verilog/* -r $(BUILD)/verilog/; fi
	$(CLASH) -v --verilog $(FILES_HS) -outputdir $(BUILD)

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
	yosys -p "show" $(BUILD)/verilog/Top/$(PROJ)/$(PROJ).v

clean:
	rm ./$(BUILD)/* -rf
