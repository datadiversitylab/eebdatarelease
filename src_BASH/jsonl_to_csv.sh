#!/bin/bash

INPUT_FILE=$1
OUTPUT_FILE=$2

# Extract headers from the first JSON object
headers=$(head -n1 "$INPUT_FILE" | jq -r 'keys_unsorted | @csv')
echo "$headers" > "$OUTPUT_FILE"

# Count lines for progress
total_lines=$(wc -l < "$INPUT_FILE")

# Process file line-by-line with progress bar
pv -l -s "$total_lines" "$INPUT_FILE" | \
  jq -rc '[.[] | if type=="array" or type=="object" then tojson else . end] | @csv' >> "$OUTPUT_FILE"
