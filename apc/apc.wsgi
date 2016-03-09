#!/usr/bin/python
import sys

with open('log.txt', 'w') as f:
    f.write(sys.argv)
    
#sys.path.insert(0, "/analysistools/public_html/apps/apc")
sys.path.insert(0, sys.argv[1])
from apc import app as application
