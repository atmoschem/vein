#!/usr/bash

cd estimation

for yyyy in {1991..2020}
do
  echo "*****"
  echo "YEAR = $yyyy" 
  
  cd ${yyyy}
  
  for nuf in {1..27}
  do
     echo "$nuf / 27"
     uf=$(ls | head -${nuf} | tail -1)
     cd ${uf}
     
     echo "Rodando main.R para $uf"
     Rscript main.R >> ../../../log.txt

     cd ..

  done
  echo
  echo
  cd ..
done
exit
