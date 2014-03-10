#!/bin/bash

cd ..
/usr/lib/lazarus/tools/install/smart_strip.sh decision

rm -fv *.ppu *.o *.compiled *.lrs
rm -Rfv backup
cd pack_utils

