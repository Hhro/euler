#!/usr/bin/env python3
import subprocess
import sys
import re

pr_num = sys.argv[1]
main_ml = open("bin/main.ml")
subbed = re.sub(r"(let \(\) = chall)_\d*", r"\1_"+pr_num, main_ml.read())
open("bin/main.ml", "w").write(subbed)
subprocess.run("make", shell=True)
subprocess.run("./euler")
