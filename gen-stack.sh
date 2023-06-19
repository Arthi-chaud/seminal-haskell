#!/bin/bash
# This script takes the root stack.yaml file (used for dev), and creates a new one,
# based on the given LTS version, and the matching ghc-lib version we want.

declare -A vmap=(
	# 9.2.8
	["20.23"]="9.2.7.20230228"
	# 9.2.7
    ["20.21"]="9.2.7.20230228"
	# 9.2.6
    ["20.12"]="9.2.7.20230228"	
	# 9.2.5
    ["20.11"]="9.2.7.20230228"			
	# ghc 9.0.2
	["19.33"]="9.2.7.20230228"
)

LTS_VERSION="$1"

IN="stack.yaml"
OUT=stack-lts-"$LTS_VERSION".yaml

rm -f "$OUT"

sed -r "s/^(\s*)(resolver\s*:\s*lts-\d{2}\.\d{2}\s*$)/resolver: lts-$LTS_VERSION/" "$IN" > "$OUT"

echo -e "extra-deps:
- ghc-lib-${vmap[$LTS_VERSION]}
- ghc-lib-parser-${vmap[$LTS_VERSION]}" >> "$OUT"