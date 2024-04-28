#!/bin/bash -e

OUT_DIR=out

rm -rf $OUT_DIR
mkdir $OUT_DIR

python3 gen.py
ca65 --cpu 65816 -o $OUT_DIR/game.o game.s -g
ld65 -C memmap.cfg $OUT_DIR/game.o -o $OUT_DIR/pentris.smc --dbgfile $OUT_DIR/pentris.dbg