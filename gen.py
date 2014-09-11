#!/usr/bin/env python

import sys
import re

ORIGINAL_FILE='UTF-8'
OUTPUT_FILE='UTF-8-EAW-FULLWIDTH'

def main():
    width_list = []
    line_re = re.compile('([a-fA-F\d]+);(\w)\s+#\s+(.*)')
    f = open('EastAsianWidth.txt')
    for line in f:
        if line.startswith('#'):
            continue
        match = line_re.match(line)
        if not match:
            continue
        (code, amb, comment) = match.group(1), match.group(2), match.group(3)
        if amb != 'A':
            continue

        # Skip VARIATION SELECTOR
        if int('E0100', 16) <= int(code, 16) and \
           int(code, 16) <= int('E01EF', 16):
            continue
#        c = unichr(int(code, 16))
#        print code, comment, c
        width_line = '<U%s> 2 %% %s' % (code, comment)
        width_list.append(width_line)
    f.close()
    out = open(OUTPUT_FILE, 'w')
    f = open(ORIGINAL_FILE)
    for line in f:
        if line.startswith('END WIDTH'):
            out.write('% Added East Asian Width\n')
            out.write('\n'.join(width_list))
            out.write('\n')
        out.write(line)

if __name__ == '__main__':
    main()


