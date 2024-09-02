PROBLEM: this dir 

https://cproof.uvic.ca/gliderdata/deployments/dfo-eva035/dfo-eva035-20231019/realtime_raw/

has only 3 gli files, and a *lot* of pld1 files.  This causes a problem
downloading.  I don't know how reading will go, but I think I'll look
for another directory.


To refresh the data, type

    make clean

to remove the existing files, and then type

    make download

to download the files.

