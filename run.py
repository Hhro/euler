#!/usr/bin/env python3
import subprocess
import sys
import re

pr_num = sys.argv[1]
main_ml = open("bin/main.ml").read()
subbed = re.sub(r"(let answer = Prob)\d*", r"\g<1>"+pr_num, main_ml)
open("bin/main.ml", "w").write(subbed)
subprocess.run("make", shell=True)
subprocess.run("./euler")
