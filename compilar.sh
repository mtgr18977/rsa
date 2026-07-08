#!/bin/sh
# Compila todos os exercicios de uma vez.
# Uso:  sh compilar.sh        (compila os 22)
#       gfortran exe-N.f -o exe-N   (compila um so)
for i in $(seq 1 22); do
  gfortran -O2 "exe-$i.f" -o "exe-$i" && echo "exe-$i  OK" \
    || echo "exe-$i  FALHOU"
done
