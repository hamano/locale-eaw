#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re

EAW_FILE='EastAsianWidth.txt'
ORIGINAL_FILE='UTF-8'
OUTPUT_FILE='UTF-8-EAW-FULLWIDTH'
TEST_FILE='EastAsianAmbiguous.txt'

def main():
    width_list = []
    line_re = re.compile('([a-fA-F\d]+);(\w)\s+#\s+(.*)')
    test = open(TEST_FILE, 'w', encoding='UTF-8')
    f = open(EAW_FILE)
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
        width_line = '<U%s> 2 %% %s' % (code, comment)
        width_list.append(width_line)

        if sys.version_info.major >= 3:
            c = chr(int(code, 16))
        else:
            c = unichr(int(code, 16))
        test.write('[%c] U+%s %s\n' % (c, code, comment))

    test.close()
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


