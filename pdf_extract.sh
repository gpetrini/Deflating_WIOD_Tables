#!/usr/bin/env bash

set -euo pipefail

input_dir="reports"
tmp_dir=$(mktemp -d)

timestamp=$(date +"%Y%m%d")
out="${timestamp}_MergedReport.pdf"

count=1

for country in CHN DEU USA JPN NLD MEX; do
    for pdf in "${input_dir}/${country}"*.pdf; do
        # proteção caso não exista match
        [[ -e "$pdf" ]] || continue
        pdftk "$pdf" cat 29 output "${tmp_dir}/$(printf "%03d" $count)_${country}_p29.pdf"
        count=$((count + 1))
    done
done

for country in CHN DEU USA JPN NLD MEX; do
    for pdf in "${input_dir}/${country}"*.pdf; do
        # proteção caso não exista match
        [[ -e "$pdf" ]] || continue

        pdftk "$pdf" cat 27 output "${tmp_dir}/$(printf "%03d" $count)_${country}_p30.pdf"
        count=$((count + 1))
    done
done

# PDF adicional no final
cp "figs/Point_Differences_Database_All.pdf" "${tmp_dir}/$(printf "%03d" $count)_Points.pdf"
count=$((count + 1))

for country in CHN DEU USA JPN NLD MEX; do
    for pdf in "${input_dir}/${country}"*.pdf; do
        # proteção caso não exista match
        [[ -e "$pdf" ]] || continue

        pdftk "$pdf" cat 32 output "${tmp_dir}/$(printf "%03d" $count)_${country}_p30.pdf"
        count=$((count + 1))
    done
done

for country in CHN DEU USA JPN NLD MEX; do
    for pdf in "${input_dir}/${country}"*.pdf; do
        # proteção caso não exista match
        [[ -e "$pdf" ]] || continue

        pdftk "$pdf" cat 1 output "${tmp_dir}/$(printf "%03d" $count)_${country}_p30.pdf"
        count=$((count + 1))
    done
done

# concatenação final
pdfunite "${tmp_dir}"/*.pdf "$out"

rm -rf "$tmp_dir"
