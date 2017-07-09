#! /usr/bin/env python
from __future__ import print_function
import sys
import re
import os

if __name__ == '__main__':
    in_fname = sys.argv[1]
    out_fname = in_fname + '.temp'
    last_function = None

    with open(in_fname) as in_f, open(out_fname, 'w') as out_f:
        for line in in_f:
            if not line.startswith('{-# INLINE'):
                out_f.write(line)
    os.rename(out_fname, in_fname)
