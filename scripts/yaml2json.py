#!/usr/bin/env python3

import sys, yaml, json
from yaml import load, dump
try:
    from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
    from yaml import Loader, Dumper

json.dump(yaml.load(sys.argv[1], Loader=Loader), sys.stdout, indent=2)
