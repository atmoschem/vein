#!/usr/bash

nuf=$(ls regions | wc -l)

echo $nuf

counter=1
while [ ${counter} -le ${nuf} ]
do
  uf=$(ls regions | head -${counter} | tail -1)
  echo "UF = $uf"
  cd regions/$uf  
  
 nohup R CMD BATCH --no-restore main.R &

echo
echo
cd ../../.
let counter++
done
