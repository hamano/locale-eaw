#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re

EAW_FILE='ucd/EastAsianWidth.txt'
EMOJI_FILE='ucd/emoji/emoji-data.txt'
ORIGINAL_FILE='UTF-8'
OUTPUT_FILE='UTF-8-EAW-FULLWIDTH'
TEST_FILE='test.txt'
ELISP_FILE='eaw.el'
ELISP_HEADER='eaw-header.el'
ELISP_FOOTER='eaw-footer.el'

def main():
    code_list = read_amb_code(EAW_FILE)
    generate_test(code_list)
    generate_locale(code_list)
    generate_elisp(code_list)
    print('Generation complete.')

def load_emoji(fn):
    emoji = {}
    line_re = re.compile('([0-9A-Fa-f\.]+)\s+;\s+(\w+)\s+(.*)')
    with open(fn) as f:
        for line in f:
            if line.startswith('#'):
                continue
            match = line_re.match(line)
            if not match:
                continue
            (code_or_range, prop, comment) = match.groups()
            if prop != 'Emoji':
                continue
            if '.' in code_or_range:
                 (start, end) = tuple(code_or_range.split('..'))
                 for code in range(int(start, 16), int(end, 16) + 1):
                     emoji[code] = True
            else:
                emoji[int(code_or_range, 16)] = True
    return emoji

def read_amb_code(fn):
    #emoji = load_emoji(EMOJI_FILE)
    ret = []
    line_re = re.compile('([0-9A-Fa-f\.]+);(\w)\s+#\s+(.*)')
    f = open(fn)
    for line in f:
        if line.startswith('#'):
            continue
        match = line_re.match(line)
        if not match:
            continue
        (code_or_range, amb, comment) = match.groups()
        if '.' in code_or_range:
            # range code
            (start, end) = tuple(code_or_range.split('..'))
            code = int(start, 16)
        else:
            # single code
            code = int(code_or_range, 16)

        # Exclude COMBINING CHARACTER
        if 0x0300 <= code <= 0x036F:
            continue
        # Exclude <private-use-E000>..<private-use-F8FF>
        if 0xE000 <= code <= 0xF8FF:
            continue
        # Exclude <private-use-E000>..<private-use-F8FF>
        if 0xFE00 <= code <= 0xFE0F:
            continue
        # Exclude VARIATION SELECTOR-17..VARIATION SELECTOR-256
        if 0xE0100 <= code <= 0xE01EF:
            continue
        # Exclude <private-use-F0000>..<private-use-FFFFD>
        if 0xF0000 <= code <= 0xFFFFD:
            continue
        # Exclude <private-use-100000>..<private-use-10FFFD>
        if 0x100000 <= code <= 0x10FFFD:
            continue

        # Emoji
        #is_emoji = emoji.get(code, False)
        #print('{:x}'.format(code), is_emoji)
        #if is_emoji and (0x2600 <= code):
            # 絵文字を全角にする
            #amb = 'A'
        if (0x2600 <= code <= 0x27FF) or (0x1F000 <= code <= 0x1FFFF):
            amb = 'A'

        if amb != 'A':
            continue

        if '.' in code_or_range:
            ret.append(((int(start, 16), int(end, 16)), comment))
        else:
            ret.append((code, comment))
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
    out.write(open(ELISP_HEADER).read())
    print('(setq east-asian-ambiguous \'(', file=out)
    for (code, comment) in code_list:
        if type(code) == tuple:
            for n in range(code[0], code[1] + 1):
                print('  #x%04X ; %s' % (n, comment), file=out)
        else:
            print('  #x%04X ; %s' % (code, comment), file=out)
    print('))', file=out)
    out.write(open(ELISP_FOOTER).read())
    print('done')

if __name__ == '__main__':
    main()
