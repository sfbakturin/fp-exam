#!/bin/bash

cd sources || exit 1

for INPUT in *.md; do
	OUTPUT="${INPUT%.*}.pdf"
	pandoc "${INPUT}" \
		--pdf-engine=xelatex \
		-V "mainfont:DejaVu Sans" \
		-V "monofont:DejaVu Sans Mono" \
		-V geometry:margin=1in \
		-o "${OUTPUT}" || exit 2
	echo "${OUTPUT}"
done
