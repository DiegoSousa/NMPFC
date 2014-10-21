#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/diego/Development/workspaces/workspace_mestrado/nmpfc_branch/nmpfc/RoC/bin/RoC
OFS=$IFS
IFS="
"
/usr/bin/ld.bfd -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2    -L. -o /home/diego/Development/workspaces/workspace_mestrado/nmpfc_branch/nmpfc/RoC/bin/RoC /home/diego/Development/workspaces/workspace_mestrado/nmpfc_branch/nmpfc/RoC/bin/link.res
if [ $? != 0 ]; then DoExitLink /home/diego/Development/workspaces/workspace_mestrado/nmpfc_branch/nmpfc/RoC/bin/RoC; fi
IFS=$OFS
