#!/bin/bash
set -euo pipefail

tmp_index=$(mktemp)
tmp_quarto=$(mktemp)

cp index.qmd "$tmp_index"
cp _quarto.yml "$tmp_quarto"

cleanup() {
  cp "$tmp_index" index.qmd
  cp "$tmp_quarto" _quarto.yml
  rm -f "$tmp_index" "$tmp_quarto"
}

trap cleanup EXIT

echo "Rendering manuscript..."
cp _quarto-manuscript.yml _quarto.yml
cp index-manuscript.qmd index.qmd
quarto render

echo "Rendering appendix..."
cp _quarto-appendix.yml _quarto.yml
cp index-appendix.qmd index.qmd
quarto render

echo "Merging PDFs..."
python3 -c "
from pypdf import PdfWriter, PdfReader

manuscript = PdfReader('_book/manuscript/docs.pdf')
appendix = PdfReader('_book/appendix/docs.pdf')

writer = PdfWriter()

for page in manuscript.pages:
    writer.add_page(page)
for page in appendix.pages:
    writer.add_page(page)

with open('_book/thesis-complete.pdf', 'wb') as f:
    writer.write(f)

print('Merged PDF saved to _book/thesis-complete.pdf')
"

echo 'All done! Den færdige afhandling ligger i _book/thesis-complete.pdf'
