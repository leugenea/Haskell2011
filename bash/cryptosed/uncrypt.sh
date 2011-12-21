#! /bin/bash

for i in {0..9}
do
	for j in {0..9}
	do
		for k in {0..9}
		do
			for l in {0..9}
			do
				pw="$i$j$k$l"
				7z e -y -p"$pw" tex2html-test.tar.7z > /dev/null
				if (( $? == 0))
				then
					echo "$pw is right pw"
					exit 0
				else
					echo "$pw is wrong pw"
				fi
			done
		done
	done
done
