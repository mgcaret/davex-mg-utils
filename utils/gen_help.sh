#!/bin/bash
if [ -z "$2" ]; then
  echo "Usage: $0 infile outfile"
  exit 1
fi
sed -n '/%help/,/%hend/p' "$1" | awk -F '; ' '{print $2}' | egrep -v '^%.+' | tr '\n' '\r' > "$2"
if [ -s "$2" ]; then
  echo "Help file created."
else
  echo "Help file missing or size zero."
  rm -f "$2"
fi
exit 0

