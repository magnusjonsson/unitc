#!/bin/bash
set -e
exitcode=0
unitc="stack exec unitc"

for f in doc/examples/good*.c; do
  echo -n $f:
  if OUTPUT=$($unitc $f 2>&1); then
    echo OK
  else
    echo FAIL
    echo "$OUTPUT"
    exitcode=1
  fi
done

for f in doc/examples/bad*.c; do
  echo -n $f:
  if $unitc $f 1>/dev/null 2>/dev/null; then
    echo FAIL
    exitcode=1
  else
    echo OK
  fi 
done
exit $exitcode
