#!/bin/bash


# Remove artifacts from previous builds
if [ -d mc-out ]; then
    rm -rf mc-out
fi

if [ ! -d m-jvm ]; then
    git clone https://github.com/m-language/m-jvm
fi

# Build the jvm compiler
echo "[build.sh] Building the jvm compiler"
cd m-jvm
gradle build fatJar --warning-mode none
cd ..

# Build the compiler
echo "[build.sh] Building mc.m with m-jvm"
cp m-jvm/build/libs/*.jar mc-jvm.jar
java -jar mc-jvm.jar mc.m mc-out

# Create FatJar from dependencies
echo "[build.sh] Creating fat jar from dependencies"
cd mc-out
mkdir jar
cp ../mc.mf ../mc-jvm.jar .
jar xf mc-jvm.jar
# Final jar
jar cfm jar/mc.jar mc.mf -C . io/github/m/*.class $(find kotlin -name "*.class") mc.class mc-jvm.jar
echo "[build.sh] Built mc-out/jar/mc.jar"
