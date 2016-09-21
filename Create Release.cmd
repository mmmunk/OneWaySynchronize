@echo off
set zip7="C:\Program Files\7-Zip\7z.exe"
set rzfb=..\release\OneWaySynchronize
set rzfs=%rzfb%_Source

cd ..
if exist source if not exist release mkdir release
if exist _temp del /Q _temp\*
if not exist _temp mkdir _temp
cd _temp

copy ..\source\Win32\Release\OneWaySynchronize.exe OneWaySynchronize32.exe
copy ..\source\Win64\Release\OneWaySynchronize.exe OneWaySynchronize64.exe
copy ..\source\Install.cmd .
copy ..\source\License.txt .

%zip7% a -tzip %rzfb% OneWaySynchronize*.exe Install.cmd License.txt

cd ..
rmdir /S /Q _temp

cd source

%zip7% a -tzip %rzfs% *.dproj *.dpr *.res *.cmd *.txt

echo .
echo Please rename release files to include version number.
pause