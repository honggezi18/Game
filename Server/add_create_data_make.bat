@echo off

if exist .\data_create.tar.gz 7z e data_create.tar.gz -y
if exist .\data_create.tar 7z e data_create.tar -oaaa

if exist .\aaa goto aaa
goto exit_make

:aaa
echo "data"

rd /q /s .\src\data\create
xcopy .\aaa .\src\data\create\ /e /y

del data_create.tar.gz
del data_create.tar
del .\ebin\data_*.beam
rd /q /s .\aaa
goto exit_make

:exit_make
echo "exit_make"
erl -noshell -s make all -s c q
pause
exit 1





