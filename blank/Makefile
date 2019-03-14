# Project setup
PROJ      = blinky
DEVICE    = 1k
FOOTPRINT = tq144

FILES_HS  = $(find src/ -type f -name '*.hs')
# FILES_V   = $(find verilog/ -type f -name '*.vs')

upload: build/top.bin
	iceprog build/top.bin

test: build/Top
	./build/Top

clean:
	@rm -rf build

build/Top: build/Top.hs
	clash build/Top.hs

build/verilog/Main/blinker.v: Top.hs
	clash --verilog Top.hs

top.blif: verilog/Main/blinker.v
	(cd verilog/Main; yosys -p "synth_ice40 -blif ../../top.blif" $$(ls *.v|grep -v testbench.v))

top.txt: top.blif
	arachne-pnr -d 1k -p icestick.pcf top.blif -o top.txt

top.bin: top.txt
	icepack top.txt top.bin

