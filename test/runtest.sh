#!/bin/bash

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 [program] [testfile] [test count]"
    exit 1
fi

prog=$1
file=$2
lim=$3

for (( i=0; i<$lim; i++ ))
do
    for (( j=0; j<$lim; j++ ))
    do
        RETVAL=$($prog $file "N$i" "N$j" 2>&1 >/dev/null)

        if [ "$RETVAL" != "" ];
        then
            echo "ERROR in test case (Node $i -> Node $j) !!!"
            exit 1
        else
            echo "Test passed (Node $i -> Node $j)"
        fi
    done
done

echo "All tests passed."
