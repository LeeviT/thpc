#!/bin/bash

./a.out 50 23 >> plotdata
./a.out 100 23 >> plotdata

for i in {200..2000..200}
do
  ./a.out $i 23 >> plotdata
done
