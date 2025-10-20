import sys
import time
import os

argc = len(sys.argv)
if argc != 1:

    prog = {sys.argv[0]} if argc == 1 else "w.py"
    print(f"Usage: prog", file=sys.stderr)

while True:
    os.system("dune exec ./gitst.exe w.sexp w.out > NUL 2>w.err")
    time.sleep(1)
