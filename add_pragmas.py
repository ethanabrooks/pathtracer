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
            matches = re.search('\A([^\s]+)\s::', line)
            if last_function is not None and re.search('\A([^=]+)\s=', line):
                out_f.write('{' + '-# INLINE {} #-'.format(last_function) + '}\n')
                last_function = None
            if matches:
                last_function = matches.group(1)
            out_f.write(line)
    os.rename(out_fname, in_fname)
