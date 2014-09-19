#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re

EAW_FILE='EastAsianWidth.txt'
ORIGINAL_FILE='UTF-8'
OUTPUT_FILE='UTF-8-EAW-FULLWIDTH'
TEST_FILE='EastAsianAmbiguous.txt'
ELISP_FILE='eaw-fullwidth.el'
ELISP_TEMPL='eaw-tmpl.el'

def main():
    width_list = []
    line_re = re.compile('([a-fA-F\d]+);(\w)\s+#\s+(.*)')

    f = open(EAW_FILE)
    for line in f:
        if line.startswith('#'):
            continue
        match = line_re.match(line)
        if not match:
            continue
        (code, amb, comment) = match.groups()
        if amb != 'A':
            continue

        # Exclude COMBINING CHARACTER
        if int('0300', 16) <= int(code, 16) <= int('036F', 16):
            continue

        # Exclude VARIATION SELECTOR
        if int('FE00', 16) <= int(code, 16) <= int('FE0F', 16):
            continue

        if int('E0100', 16) <= int(code, 16) <= int('E01EF', 16):
            continue

        width_list.append((code, comment))

    f.close()

    generate_test(width_list)
    generate_locale(width_list)
    generate_elisp(width_list)
    print('Generation complete.')

def generate_test(width_list):
    print('Generating %s ... ' % (TEST_FILE), end='')
    out = open(TEST_FILE, 'w')

    for (code, comment) in width_list:
        if sys.version_info.major >= 3:
            c = chr(int(code, 16))
        else:
            c = unichr(int(code, 16))
        print('[%c] U+%s %s' % (c, code, comment), file=out)
    print('done')

def generate_locale(width_list):
    print('Generating %s ... ' % (OUTPUT_FILE), end='')
    out = open(OUTPUT_FILE, 'w')
    f = open(ORIGINAL_FILE)
    for line in f:
        if line.startswith('END WIDTH'):
            out.write('% Added East Asian Width\n')
            for (code, comment) in width_list:
                print('<U%s> 2 %% %s' % (code, comment), file=out)
        print(line, end='', file=out)
    print('done')

def generate_elisp(width_list):
    print('Generating %s ... ' % (ELISP_FILE), end='')
    out = open(ELISP_FILE, 'w')
    print('(setq east-asian-ambiguous \'(', file=out)
    for (code, comment) in width_list:
        print('  #x%s ; %s' % (code, comment), file=out)
    print('))', file=out)
    f = open(ELISP_TEMPL)
    out.write(f.read())
    print('done')

if __name__ == '__main__':
    main()


