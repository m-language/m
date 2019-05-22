#!/usr/bin/env bash
set -e

SDKMAN_PATH="$HOME/.sdkman/candidates"
SUBPATH=$PATH:"$SDKMAN_PATH/kotlin/current/bin":"$SDKMAN_PATH/gradle/current/bin"

env PATH=$SUBPATH bash ./mc.sh build