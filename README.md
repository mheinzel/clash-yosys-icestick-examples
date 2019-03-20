From Haskell to Hardware using Clash, Yosys and IceStorm
========================================================

This is a simple demo for compiling [Haskell](http://www.haskell.org)
code into descriptions of digital circuits and flashing them on hardware.
It is based on
[mgajda/clash-yosys-demo](https://github.com/mgajda/clash-yosys-demo)
for the README,
[wd5gnr/icestick](https://github.com/wd5gnr/icestick)
for the `icestick.pcf` file and
[nesl/ice40_examples](https://github.com/nesl/ice40_examples)
for a few nice examples I could port to Clash.

This demo uses a fully open source toolchain:

  * [CλaSH](http://www.clash-lang.org/)
    for compiling Haskell into Verilog,
  * [Yosys](http://www.clifford.at/yosys/)
    to compile [Verilog](https://en.wikipedia.org/wiki/Verilog) code into the
    `.blif` format,
  * [Arachne Place-N-Route](https://github.com/cseed/arachne-pnr)
    to perform routing onto the _Lattice ICE40 H1K_ device,
  * [IceStorm](http://www.clifford.at/icestorm/) toolchain
    to generate an FPGA bitstream and upload it onto the
    [Lattice IceStick](http://latticesemi.com/iCEstick) device.


Installing Clash:
-----------------

I had issues setting up a full Nix-based installation, but ended up using Stack:

* Install Stack following the [instructions](https://docs.haskellstack.org/en/stable/README/#how-to-install).
* Install Clash:
  `stack install --resolver lts-12.12 clash-ghc`

Alternatively, you can use Cabal to follow the [official installation instructions](https://clash-lang.org/downloads).
This, however, will interfere with your system level GHC installation.


Installing the IceStorm toolchain:
----------------------------------

### Nix

Install the packages:
`yosys arachne-pnr icestorm`
(we will also use `gnumake`).

### Archlinux

Use the Arch User Repository to install the packages:
`icestorm-git arachne-pnr-git yosys-git`

### From Source

```bash
git clone https://github.com/cliffordwolf/icestorm.git icestorm
sudo make install -j4 -C icestorm

git clone https://github.com/cseed/arachne-pnr.git arachne-pnr
sudo make install -j4 -C arachne-pnr

git clone https://github.com/cliffordwolf/yosys.git yosys
sudo make install -j4 -C yosys
```

### OSX

Follow <http://www.clifford.at/icestorm/notes_osx.html>.


Setting up rules for the USB device (Linux):
--------------------------------------------

For Linux you also might want to enable write access through FTDI USB device.

Create the file `/etc/udev/rules.d/53-lattice-ftdi.rules` with the content:

```
ACTION=="add", ATTR{idVendor}=="0403", ATTR{idProduct}=="6010", MODE:="666"
```


Test that everything works:
---------------------------

There should be no errors when you do:

```bash
cd blank
make
```


Play around!
------------

This repository contains the following projects:
  * `blank` - a project template you can copy to create something new
  * `blinky` - a simple counter that shows its state on the 5 board LEDs
  * `pwm` - an example of gradually changing LED brightness using pulse width modulation
  * `buttons` - control the 4 green LEDs using buttons connected to PMOD pins 1-4
  * `calculator` - a simple state machine using buttons and LEDs

Each of those can be compiled into a bitstream using `make` and burned onto the device using `make burn`.
