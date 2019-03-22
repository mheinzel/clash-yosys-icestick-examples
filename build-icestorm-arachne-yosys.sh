#!/usr/bin/env bash

UNAME_STR=`uname`

if [[ "$UNAME_STR" == "Darwin" ]] && hash brew 2>/dev/null; then

    PYTHONVERSION=$(python3 --version 2>&1 | egrep -o '3\.[0-9]+')
    OLDPATH=$PATH
    PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

    git clone https://github.com/cliffordwolf/icestorm.git icestorm
    PYTHONPATH=$(brew --prefix)/lib/python$PYTHONVERSION/site-packages/ make install -j4 -C icestorm

    git clone https://github.com/cseed/arachne-pnr.git arachne-pnr
    make install -j4 -C arachne-pnr

    git clone https://github.com/cliffordwolf/yosys.git yosys
    PYTHONPATH=$(brew --prefix)/lib/python$PYTHONVERSION/site-packages/ make install -j4 -C yosys

    PATH=$OLDPATH

else

    git clone https://github.com/cliffordwolf/icestorm.git icestorm
    sudo make install -j4 -C icestorm

    git clone https://github.com/cseed/arachne-pnr.git arachne-pnr
    sudo make install -j4 -C arachne-pnr

    git clone https://github.com/cliffordwolf/yosys.git yosys
    sudo make install -j4 -C yosys

fi
