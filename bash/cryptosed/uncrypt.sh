#! /bin/bash

for i in {0..9}
do
	for j in {0..9}
	do
		for k in {0..9}
		do
			for l in {0..9}
			do
				pw = "$i$j$k$l"
				7z e -y -p"$pw" tex2html-text.tar.7z
				if (( $? == 0))
				then
					echo $pw
					exit 0
				fi
			done
		done
	done
done
