#!/usr/bin/env bash

CACHE="$HOME/.cache"
mkdir -p "$CACHE"

kotlin_url="https://github.com/JetBrains/kotlin/releases/download/v1.3.31/kotlin-compiler-1.3.31.zip"
wget $kotlin_url --output-document="$CACHE/kotlin.zip"
unzip "$CACHE/kotlin.zip" -d "$CACHE"