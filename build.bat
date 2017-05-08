@echo off

set commentForCleanup=/wd4100 /wd4189
set warnings=/wd4820 /wd4201 %commentForCleanup%
cl /Wall /WX /Zi %warnings% /MT /Od compiler.c
