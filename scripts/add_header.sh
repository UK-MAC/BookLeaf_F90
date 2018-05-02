#!/bin/bash

# Echo the BookLeaf license
function license()
{
    echo "! @HEADER@"

    head LICENSE.txt -n1 | \
        awk '{print "! " $0}'

    echo "!"
    echo "! This file is part of BookLeaf."
    echo "!"

    tail LICENSE.txt -n+3 | \
        awk '{print "! " $0}'

    echo "! @HEADER@"
}

sources=$(find src/ -name '*.f90')

# Open temporary file for prepending
tmpfile=$(mktemp /tmp/add-header.XXXXXX)

for f in ${sources}; do
    license > "$tmpfile"
    cat "$f" | sed '/@HEADER@/,/@HEADER@/d' >> "$tmpfile"
    cat "$tmpfile" > "$f"
done

rm "$tmpfile"
