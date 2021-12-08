#!/bin/bash

move_files() {
	echo $1 
	for file in "${1}"/*.rkt
	do
		echo "${file%.*}"
		mkdir "${file%.*}"
		mv "${file}" "${file%.*}"/
	done;
}

for i in $1/*
do
	# echo $i
	move_files "${i}"
done;
