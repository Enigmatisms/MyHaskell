#!/bin/bash

full_name=$1
name=${full_name%.*}
echo "Start to compile file ${full_name}"

if [ ! -d "build" ]; then
    mkdir build
fi

cd build
ghc ../${full_name} -o ${name}

exts=('.hi' '.o' '.s')
for ext in ${exts[@]}; do
    file=${name}${ext}
    if [ -f ../${file} ]; then
        mv ../${file} ${file} 
    fi
done

echo "Done compiling file ${full_name}"