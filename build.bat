@echo off

set commentForCleanup=/wd4100 /wd4189
set warnings=%commentForCleanup% /wd4820 /wd4201 /wd4710 /wd4204
REM
REM
if exist out.exe del out.exe

set windows_kits_lib_path=%PROGRAMFILES(x86)%\Windows Kits\8.0\Lib\win8\um\x64
REM clang-cl /Wall /WX /Zi -D_CRT_SECURE_NO_WARNINGS %warnings% /MTd /Od compiler.c -Wno-missing-field-initializers && compiler && nasm out.asm -f win64 &&  lld-link.exe out.obj "%windows_kits_lib_path%\Kernel32.Lib" /entry:_start
REM cl /Wall /WX /Zi -D_CRT_SECURE_NO_WARNINGS %warnings% /MTd /Od compiler.c user32.lib && compiler && nasm out.asm -f win64 && link.exe out.obj Kernel32.lib /subsystem:console /entry:_start
cl /Wall /WX /Zi -D_CRT_SECURE_NO_WARNINGS %warnings% /MTd /Od compiler.c user32.lib

if "%1"=="full" (
compiler && nasm out.asm -f win64 && link.exe out.obj Kernel32.lib /subsystem:console /entry:_start
		)


