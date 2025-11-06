#!/bin/sh
find . -iname '*.typ' -exec typst compile {} -f svg \;
