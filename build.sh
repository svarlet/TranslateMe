#!/bin/sh

if [ ! -d "./dist" ]
then
    mkdir ./dist
fi

elm make Main.elm --output dist/Main.js
cp -R ./template/* dist/
