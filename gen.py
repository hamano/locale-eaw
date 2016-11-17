#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re

EAW_FILE='EastAsianWidth.txt'
ORIGINAL_FILE='UTF-8'
OUTPUT_FILE='UTF-8-EAW-FULLWIDTH'
TEST_FILE='EastAsianAmbiguous.txt'
ELISP_FILE='eaw.el'
ELISP_TEMPL='eaw-template.el'

def main():
    code_list = read_amb_code(EAW_FILE)
    generate_test(code_list)
    generate_locale(code_list)
    generate_elisp(code_list)
    print('Generation complete.')

def read_amb_code(fn):
    ret = []
    line_re = re.compile('([a-fA-F\d\.]+);(\w)\s+#\s+(.*)')
    f = open(fn)
    for line in f:
        if line.startswith('#'):
            continue
        match = line_re.match(line)
        if not match:
            continue
        (code, amb, comment) = match.groups()
        if amb != 'A':
            continue

        if '.' in code:
            # range code
            (start, end) = tuple(code.split('..'))
            # Exclude COMBINING CHARACTER
            if '0300' == start:
                continue
            # Exclude <private-use-E000>..<private-use-F8FF>
            if 'E000' == start:
                continue
            # Exclude VARIATION SELECTOR-1..VARIATION SELECTOR-16
            if 'FE00' == start:
                continue
            # Exclude VARIATION SELECTOR-17..VARIATION SELECTOR-256
            if 'E0100' == start:
                continue
            # Exclude <private-use-F0000>..<private-use-FFFFD>
            if 'F0000' == start:
                continue
            # Exclude private-use-100000
            if '100000' == start:
                continue
            # Exclude SQUARED THREE D..SQUARED VOD
            if '1F19B' == start:
                continue
            ret.append(((int(start, 16), int(end, 16)), comment))
        else:
            # single code
            n = int(code, 16)
            # Exclude COMBINING CHARACTER
            if int('0300', 16) <= n <= int('036F', 16):
                continue
            # Exclude VARIATION SELECTOR
            if int('FE00', 16) <= n <= int('FE0F', 16):
                continue
            if int('E0100', 16) <= n <= int('E01EF', 16):
                continue
            ret.append((n, comment))
    f.close()
    return ret

def generate_test(code_list):
    print('Generating %s ... ' % (TEST_FILE), end='')
    out = open(TEST_FILE, 'w', encoding='UTF-8')

    for (code, comment) in code_list:
        if type(code) == tuple:
            for n in range(code[0], code[1] + 1):
                c = chr(n)
                print('[%c] U+%04X %s' % (c, n, comment), file=out)
        else:
            c = chr(code)
            print('[%c] U+%04X %s' % (c, code, comment), file=out)
    print('done')

def print_locale(out, n, comment):
    if n <= 0xffff:
        print('<U%04X> 2 %% %s' % (n, comment), file=out)
    else:
        print('<U%08X> 2 %% %s' % (n, comment), file=out)

def generate_locale(code_list):
    print('Generating %s ... ' % (OUTPUT_FILE), end='')
    out = open(OUTPUT_FILE, 'w')
    f = open(ORIGINAL_FILE)
    for line in f:
        if line.startswith('END WIDTH'):
            out.write('% Added East Asian Width\n')
            for (code, comment) in code_list:
                if type(code) == tuple:
                    for n in range(code[0], code[1] + 1):
                        print_locale(out, n, comment)
                else:
                    print_locale(out, code, comment)
        print(line, end='', file=out)
    print('done')

def generate_elisp(code_list):
    print('Generating %s ... ' % (ELISP_FILE), end='')
    out = open(ELISP_FILE, 'w')
    print('(setq east-asian-ambiguous \'(', file=out)
    for (code, comment) in code_list:
        if type(code) == tuple:
            for n in range(code[0], code[1] + 1):
                print('  #x%04X ; %s' % (n, comment), file=out)
        else:
            print('  #x%04X ; %s' % (code, comment), file=out)
    print('))', file=out)
    f = open(ELISP_TEMPL)
    out.write(f.read())
    print('done')

if __name__ == '__main__':
    main()
