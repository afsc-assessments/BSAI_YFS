if NOT EXIST build mkdir build
if NOT EXIST build\debug mkdir build\debug
if NOT EXIST build\release mkdir build\release
call admb -g fm_bnds.tpl 
copy fm_bnds.exe build\debug
call admb -f fm_bnds.tpl 
copy fm_bnds.exe build\release

