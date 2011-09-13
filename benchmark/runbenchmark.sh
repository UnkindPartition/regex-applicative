#!/bin/sh

if [ "$1" = "prof" ]
then
    echo "Profiling build"
    ghc -i.. -odir . --make -O2 -fforce-recomp -prof -auto-all -rtsopts benchmark.hs
    for i in `seq 1 800`; do echo -n a; done | time ./benchmark +RTS -H100M -p
else
    echo "Non-Profiling build"
    ghc -i.. -odir . --make -O2 -fforce-recomp -rtsopts benchmark.hs
    for i in `seq 1 800`; do echo -n a; done | time ./benchmark +RTS -s -H100M
fi
