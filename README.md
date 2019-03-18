Haskell deployed using fully open source toolchain to iCE40
===========================================================

This is a simple demo for compiling [Haskell](http://www.haskell.org)
code into descriptions of digital circuits and flashing them on hardware.
It is based on
[mgajda/clash-yosys-demo](https://github.com/mgajda/clash-yosys-demo)
for the README,
[wd5gnr/icestick](https://github.com/wd5gnr/icestick)
for the `icestick.pcf` file and
[nesl/ice40_examples](https://github.com/nesl/ice40_examples)
for a few nice examples I could port to Clash.

[_Field Programmable Gate Arrays_](https://en.wikipedia.org/wiki/Field-programmable_gate_array)
(FPGAs) are a cheap and fast tool for prototyping hardware descriptions that
can be later used for creating chips.

[_Haskell_](http://www.haskell.org) is a lazy functional programming language,
which lends itself readily to both very high level descriptions of software,
and very low level descriptions of hardware, due its solid mathematical
underpinnings.

This demo uses fully open source toolchain:

  * [CλaSH](http://www.clash-lang.org/)
    for compiling Haskell into Verilog,
  * [Yosys](http://www.clifford.at/yosys/)
    to compile [Verilog](https://en.wikipedia.org/wiki/Verilog) code into the
    `.blif` format,
  * [Arachne Place-N-Route](https://github.com/cseed/arachne-pnr)
    to perform routing onto the _Lattice ICE40 H1K_ device,
  * [IceStorm](http://www.clifford.at/icestorm/) toolchain
    in order to generate FPGA bitstream and upload it onto the
    [Lattice IceStick](http://latticesemi.com/iCEstick) device.

This repository contains the following projects:
  * `blank` - a project template you can copy to create something new
  * `blinky` - a simple counter that shows its state on the 5 board LEDs
  * `pwm` - an example of gradually changing LED brightness using pulse width modulation
  * `buttons` - control the 4 green LEDs using buttons connected to PMOD pins 1-4

Each project then consists of the following files:
  * `src` - the Haskell code describing the circuit
  * `icestick.pcf` - assignment of names to _Lattice iCE40 H1K_ pins on the [iCEStick](http://latticesemi.com/iCEstick) board.

Installing toolchain:
---------------------
With nix (recommended):
1. Install [nix](https://nixos.org/nix/)
2. Drop into a
    ```
    nix-shell
    ```

Or to install the dependencies manually:
1. First install the [IceStorm](http://www.clifford.at/icestorm/) toolchain:
    On the latest _Ubuntu_ you may install from the repository:
    ```
    sudo apt-get install -y fpga-icestorm yosys arachne-pnr
    ```

    Otherwise you might compile from the latest source:
    * IceStorm utilities themselves:

        ```bash
        git clone https://github.com/cliffordwolf/icestorm.git icestorm
        make -j4 -DPREFIX=$HOME/icestorm
        make     -DPREFIX=$HOME/icestorm install
        ```

        For Linux you also might want to enable write access through FTDI USB device:

        ```
        cat - <<EOF > /etc/udev/rules.d/53-lattice-ftdi.rules
        ACTION=="add", ATTR{idVendor}=="0403", ATTR{idProduct}=="6010", MODE:="666"
        EOF
        ```

    * [Arachne PNR](https://github.com/cseed/arachne-pnr) tool:

        ```bash
        git clone https://github.com/cseed/arachne-pnr.git arachne-pnr
        cd arachne-pnr
        make -j$(nproc) -DDEST_DIR=$HOME/icestorm -DICEBOX=$HOME/icestorm/share/icebox/
        make install
        ```

    * Yosys Verilog compiler:

        ```bash
        git clone https://github.com/cliffordwolf/yosys.git yosys
        cd yosys
        make -j$(nproc) -DPREFIX=$HOME/icestorm
        make            -DPREFIX=$HOME/icestorm install
        ```
2. [CλaSH](http://www.clash-lang.org/) compiler based on [GHC](https://www.haskell.org/ghc/):
    * To install GHC and Cabal on Linux:

        ```
        apt-get install ghc cabal-install
        ```
    * To install GHC on Windows it is recommended to either use `.msi` package
      of [Haskell Platform](https://www.haskell.org/platform/)
      or [Stack](http://docs.haskellstack.org/en/stable/README/) installation utility.
    * From within this environment, use `cabal-install` to setup `clash-ghc` package:

        ```bash
        cabal install clash-ghc
        ```

Compilation steps:
------------------
While there will be Makefile, you might want to look through the build process step by step:

1. For simulation just compile it with `clash` as a Haskell program, and run:

    ```bash
    clash Demo.hs
    ```

2. For Verilog generation:

    * run interpreter: `clash --interactive Demo.hs`
    * enter `:verilog` in the interpreter to generate `.verilog` code

3. To compile Verilog into `.blif` netlist format:

    ```
    yosys -p "synth_ice40 -blif demo.blif" Verilog/Main/Demo_TopEntity.v
    ```

4. To route `.blif` netlist onto Lattice IceStick device:

    ```
    arachne-pnr -d 1k -p demo.pcf demo.blif -o demo.txt
    ```

5. To compile routed netlist into bitstream:

    ```
    icepack demo.txt demo.bin
    ```

6. To upload bitstream onto the FPGA:

    ```
    iceprog demo.bin
    ```
    NOTE: If you forgot to add the [relevant udev rule](https://stackoverflow.com/questions/36633819/),
    you might need to use `sudo` here.

Or use `Makefile`:

```
make test
make upload
```
