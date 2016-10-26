#!/bin/bash

DATANAME=data
TEMPFILE=temp.csv

PROPANE_CMD="mono ../../src/bin/Release/propane.exe"

if [ -f $TEMPFILE ] ; then
    rm $TEMPFILE
fi

$PROPANE_CMD --bench

genstats () {
  shopt -s nullglob
  for f in benchmarks/${1}*$2.xml
  do
    for i in {1..3} 
    do 
      file=${f%${2}.xml}
      policy=${file}$2.pro
      topo=${file}$2.xml
      size=${file#benchmarks/"$1"}
      echo "$size" >> $TEMPFILE
      echo "compiling: $policy"
      $PROPANE_CMD --policy $policy --topo $topo --csv --failures=0 >> $TEMPFILE
    done
  done

  awk 'NR % 3 != 2' $TEMPFILE > ${DATANAME}_${1}${2}.csv
  rm $TEMPFILE
}

# genstats "backbone" "_abs"
# genstats "backbone" "_con"
# genstats "fat" "_abs"
genstats "fat" "_con"

if [ -d output/ ] ; then
  rm -r output/
fi

if [ ! -d graphs/ ] ; then
  mkdir graphs/
fi

echo "Generating graphs..."
python plot.py 