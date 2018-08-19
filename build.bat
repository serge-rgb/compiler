@echo off

set comment_for_cleanup=/wd4100 /wd4456 /wd4457 /wd4710 /wd4706 /wd4221
cl.exe ^
   /Zi ^
            /Wall  %comment_for_cleanup% /wd4214 /wd4244 /wd4242 /wd4388^
            /wd4204 /wd4996 /wd4255 /wd4201 /wd4820 /wd4305 /wd5045^
   compiler.c User32.lib