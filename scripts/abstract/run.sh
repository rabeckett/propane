#!/bin/bash

DATAFILE=data
TEMPFILE=temp.csv

PROPANE_CMD="mono ../../src/bin/Release/propane.exe"

if [ -f $TEMPFILE ] ; then
    rm $TEMPFILE
fi

genstats () {
  shopt -s nullglob
  for f in benchmarks/*$1.xml
  do
      file=${f%_abs.xml}
      policy=${file}$1.pro
      topo=${file}$1.xml

      size=${file#benchmarks/fat}
      echo "$size" >> $TEMPFILE
      $PROPANE_CMD --policy $policy --topo $topo --csv --failures=0 >> $TEMPFILE
  done

  awk 'NR % 3 != 2' $TEMPFILE > ${DATAFILE}${1}.csv
  rm $TEMPFILE
}

genstats "_abs"
