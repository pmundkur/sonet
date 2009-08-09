#!/bin/bash

PROG=./test_parser

for f in `ls *pass*.json`; do
	$PROG $f -no-print-value
	ec=$?
	if [ $ec -eq 1 ] ; then
		echo "Test case $f should pass, but failed."
	elif [ $ec -eq 255 ] ; then
		echo "Test case $f triggered an internal error!"
	fi
done
for f in `ls *fail*.json`; do
	$PROG $f -no-print-value
	ec=$?
	if [ $ec -eq 0 ] ; then
		echo "Test case $f should fail, but passed."
	elif [ $ec -eq 255 ] ; then
		echo "Test case $f triggered an internal error!"
	fi
done
