@echo off

set commentForCleanup=/wd4100 /wd4189
set warnings=/wd4820 %commentForCleanup%
cl /Wall /WX /Zi %warnings% /MT /Od compiler.c
