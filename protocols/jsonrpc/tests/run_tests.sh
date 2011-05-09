#!/bin/bash

PROG=./test_parser

function errmsg {
    case $1 in
	fail1.json|fail10.json|fail18.json)
	    ;;
	*)
            echo $2
	    ;;
    esac
}

for f in `ls *pass*.json`; do
    $PROG $f -no-print-value
    ec=$?
    if [ $ec -eq 1 ] ; then
	errmsg $f "Test case $f should pass, but failed."
    elif [ $ec -eq 255 ] ; then
	errmsg $f "Test case $f triggered an internal error!"
    fi
done
for f in `ls *fail*.json`; do
    $PROG $f -no-print-value
    ec=$?
    if [ $ec -eq 0 ] ; then
	errmsg $f "Test case $f should fail, but passed."
    elif [ $ec -eq 255 ] ; then
	errmsg $f "Test case $f triggered an internal error!"
    fi
done
