#!/bin/bash
set -e

# files=$(dkdep -s --ignore ctslib/*.dk -I ctslib/)
files="ctslib/connectives.dk ctslib/logic.dk"
whitelist="leibniz"
rm -f middle/*.dk
rm -f meta/meta.dk

# for f in $(dkdep --ignore -s ctslib/*.dk); do dkcheck -e $f -I ./theory; done

mkdir -p meta
mkdir -p middle
rm -f middle/*.dk
rm -f poly/*

touch meta/meta.dk

for f in $files; do
	bf=$(basename $f)
	echo "metafying $f..."
	dkmeta -m meta/meta.dk -I theory/ -I poly/ $f > middle/$bf
	echo "metafied to middle/$bf"
	echo "polymorphising middle/$bf"
	OCAMLRUNPARAM='b' dune exec -- \
		./main.exe -I poly/ -w eq -w eq_ind -w sym_eq -m meta/meta.dk middle/$bf > poly/$bf
	echo "polymorphisised to poly/$bf"
	echo "checking poly/$bf"
	dkcheck -e poly/$bf -I poly/ -I theory/
	echo "checked"
	echo "Done  $f"
done
