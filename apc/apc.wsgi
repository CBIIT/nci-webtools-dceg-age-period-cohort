#!/usr/bin/python
import sys

with open('apc.conf', 'r') as f:
   filepath = f.read().split('\n')[0].split('=')[1]

sys.path.insert(0, filepath)
from apc import app as application
