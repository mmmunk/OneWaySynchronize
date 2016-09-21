@echo off
set dst=%WinDir%\OneWaySynchronize.exe

echo This will copy OneWaySynchronize into the Windows directory
echo to make it work like a built-in command.
echo If you have an existing version you must answer yes to overwrite.
echo Press Ctrl+C to cancel.
pause

if defined ProgramFiles(x86) (
	copy OneWaySynchronize64.exe %dst%
) else (
	copy OneWaySynchronize32.exe %dst%
)

if errorlevel 1 (
	cls
	echo Error: Can't install file!
	echo You need to run this script as administrator.
	echo Right-click on Install.cmd and select Run as administrator.
) else (
	cls
	echo You can now use the command: OneWaySynchronize
)
pause