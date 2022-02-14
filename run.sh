#!/bin/bash
set -e

files=$(dkdep -s --ignore ctslib/*.dk -I ctslib/)
whitelist=$(sed -e '/^[[:space:]]*$/d' -e 's/^/-w /' whitelist.txt)

# for f in $(dkdep --ignore -s ctslib/*.dk); do dkcheck -e $f -I ./theory; done

mkdir -p meta
mkdir -p middle
mkdir -p poly

rm -f meta/meta.dk
rm -f middle/*.dk
rm -f poly/*

touch meta/meta.dk

for f in $files; do
	bf=$(basename $f)
	echo "metafying          $bf..."
	dkmeta -m meta/meta.dk -I theory/ -I poly/ $f > middle/$bf
	echo "metafied to        $bf"
	echo "polymorphising     $bf"
	OCAMLRUNPARAM='b' dune exec -- \
		./main.exe -I poly/ $whitelist -m meta/meta.dk middle/$bf > poly/$bf
	echo "polymorphisised to $bf"
	echo "checking           $bf"
	dkcheck -e poly/$bf -I poly/ -I theory/
	echo "checked            $bf"
	echo "Done               $bf"
done
