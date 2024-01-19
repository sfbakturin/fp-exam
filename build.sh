#!/bin/bash

cd lectures || exit

for INPUT in *.md; do
	OUTPUT="${INPUT%.*}.pdf"
	pandoc "${INPUT}" \
		--pdf-engine=xelatex \
		-V "mainfont:DejaVu Sans" \
		-V "monofont:DejaVu Sans Mono" \
		-V geometry:margin=1in \
		-o "${OUTPUT}"
	echo "${OUTPUT}"
done
