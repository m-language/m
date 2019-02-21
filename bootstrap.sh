#!/bin/bash

echo "[bootstrap.sh] Building original jar"
bash build.sh

echo "[bootstrap.sh] Entering .boot"
if [ -d .boot ]; then
    rm -rf .boot
fi
mkdir .boot
cd .boot


# Classpath segment
DEP=../mc-jvm.jar

echo "[bootstrap.sh] Building all boostrapped versions"
java -jar ../mc-out/jar/mc.jar ../mc.m mc2
echo "[bootstrap.sh] Built mc2 with mc"

for i in {2..10}
do
    java -cp $DEP:mc$i mc ../mc.m mc$(($i + 1))
    echo "[bootstrap.sh] Built mc$((i + 1)) with mc$i"
done
