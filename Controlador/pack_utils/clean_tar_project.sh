#!/bin/bash

cd ..
rm -fv decision *.ppu *.o *.compiled *.lrs
rm -Rfv backup

rm MDecOmni3.tar.gz
tar -czvf ../MDecOmni3.tar.gz ../MDecOmni3/
mv ../MDecOmni3.tar.gz .
cd pack_utils

