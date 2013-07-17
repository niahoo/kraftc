@echo off
for /f "delims=" %%a in ('pwd') do @set PWD=%%a
erl -args_file %PWD%/priv/vm-args  -s kraft -s init stop
