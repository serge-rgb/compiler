@echo off

set commentForCleanup=/wd4100 /wd4189
set warnings=%commentForCleanup% /wd4820 /wd4201 /wd4710 /wd4204
cl /Wall /WX /Zi -D_CRT_SECURE_NO_WARNINGS %warnings% /MTd /Od compiler.c legacy_stdio_definitions.lib
REM clang-cl /Wall /WX /Zi -D_CRT_SECURE_NO_WARNINGS %warnings% /MTd /Od compiler.c -Wno-missing-field-initializers && compiler
