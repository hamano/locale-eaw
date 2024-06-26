#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import configparser
import gzip
import shutil

EAW_FILE = 'ucd/EastAsianWidth.txt'
EMOJI_FILE = 'ucd/emoji/emoji-data.txt'
ORIGINAL_FILE = 'UTF-8'

def main():
    amb_list, comment_map = load_amb_list(EAW_FILE)
    # COMBINING CHARACTERを除外
    amb_list = list(filter(filter_combining, amb_list))
    generate_list('test/eaw.txt', amb_list, comment_map)
    nerdfont_list = load_nerdfont_list()
    generate_list('test/nerdfont.txt', nerdfont_list, comment_map)
    config = configparser.ConfigParser()
    config.read('config.ini')
    for name in config:
        if name == 'DEFAULT':
            continue
        generate_flavor(config[name], amb_list, comment_map)
    print('Generation complete.')

def generate_all():
    for section in config.sections():
        generate_flavor(config[section])

def filter_combining(code):
    # COMBINING CHARACTER
    if 0x0300 <= code <= 0x036F:
        return False
    return True

def load_private_list():
    ret = []
    # <private-use-E000>..<private-use-F8FF>
    ret.extend(range(0xE000, 0xF8FF + 1))
    # <private-use-FE00>..<private-use-F8FF>
    ret.extend(range(0xFE00, 0xFE0F + 1))
    # VARIATION SELECTOR-17..VARIATION SELECTOR-256
    ret.extend(range(0xE0100, 0xE01EF + 1))
    # <private-use-F0000>..<private-use-FFFFD>
    ret.extend(range(0xF0000, 0xFFFFD + 1))
    # <private-use-100000>..<private-use-10FFFD>
    ret.extend(range(0x100000, 0x10FFFD + 1))
    return ret

def load_nerdfont_list():
    ret = []
    with open('nerdfont/list.txt') as f:
        for line in f:
            if line.startswith('#'):
                continue
            ret.append(int(line, 16))
    return ret

def filter_box_drawing(code_comment):
    code = code_comment[0]
    comment = code_comment[1]
    # BOX DRAWINGS
    if 0x2500 <= code <= 0x257F:
        return False
    return True

def filter_block_elements(code_comment):
    code = code_comment[0]
    comment = code_comment[1]
    # BLOCK ELEMENTS
    if 0x2580 <= code <= 0x259F:
        return False
    return True

def filter_geometric_shapes(code_comment):
    code = code_comment[0]
    comment = code_comment[1]
    # GEOMETRIC SHAPES
    if 0x25A0 <= code <= 0x25FF:
        return False
    return True

def set_width(width_map, width_list, width):
    for code in width_list:
        width_map[code] = width
    return

# 連続したコードポイントをレンジ形式にする
def range_compress(width_map):
    ret = []
    code_list = sorted(width_map.keys())
    start = code_list[0]
    end = code_list[0]
    end_width = width_map[end]
    for code in code_list[1:]:
        width = width_map[code]
        if code == end + 1 and width == end_width:
            end = code
            end_width = width
        else:
            ret.append((start, end, end_width))
            start = code
            end = code
            end_width = width
    ret.append((start, end, end_width))
    return ret

def generate_flavor(config, amb_list, comment_map):
    flavor = config.name
    print(f'# {flavor} Flavor')
    width_map = {}
    wide_list = amb_list.copy()

    amb = config.getint('amb')
    if amb:
        set_width(width_map, amb_list, amb)

    private = config.getint('private')
    if private:
        private_list = load_private_list()
        set_width(width_map, private_list, private)

    nerdfont = config.getint('nerdfont')
    if nerdfont:
        nerdfont_list = load_nerdfont_list()
        set_width(width_map, nerdfont_list, nerdfont)

    width_list = range_compress(width_map)
    generate_locale(config, width_list)
    generate_elisp(config, width_map, comment_map)
    generate_vimrc(config, width_list)
    return

    box_drawing = config.getint('box_drawing', amb)
    if box_drawing == 1:
        wide_list = list(filter(filter_box_drawing, wide_list))

    block_elements = config.getint('block_elements', amb)
    if block_elements == 1:
        wide_list = list(filter(filter_block_elements, wide_list))

    geometric_shapes = config.getint('geometric_shapes', amb)
    if geometric_shapes == 1:
        wide_list = list(filter(filter_geometric_shapes, wide_list))


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

def load_amb_list(fn):
    #emoji = load_emoji(EMOJI_FILE)
    ret = []
    comment_map = {}
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

        # Emoji
        #is_emoji = emoji.get(code, False)
        #print('{:x}'.format(code), is_emoji)
        #if is_emoji and (0x2600 <= code):
            # 絵文字を全角にする
            #amb = 'A'
#        if (0x2600 <= code <= 0x27FF) or (0x1F000 <= code <= 0x1FFFF):
#            amb = 'A'

        if amb != 'A':
            continue

        if '.' in code_or_range:
            for i in range(int(start, 16), int(end, 16) + 1):
                #ret.append((i, comment))
                ret.append(i)
                comment_map[i] = comment
        else:
            #ret.append((code, comment))
            ret.append(code)
            comment_map[code] = comment
    f.close()
    return ret, comment_map

def generate_list(path, code_list, comment_map):
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w', encoding='UTF-8')

    for code in code_list:
        comment = comment_map.get(code, 'none')
        c = chr(code)
        print('[%c] U+%04X %s' % (c, code, comment), file=out)
    print('done')

def print_locale(out, start, end):
    if end <= 0xffff:
        octet = 4
    else:
        octet = 8
    if start == end:
        print(f'<U{start:0{octet}X}> 2', file=out)
    else:
        print(f'<U{start:0{octet}X}>...<U{end:0{octet}X}> 2', file=out)

def generate_locale(config, width_list):
    flavor = config.name
    path = f'dist/UTF-8-{flavor}'
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w')
    orig = open(ORIGINAL_FILE)
    for line in orig:
        if line.startswith('END WIDTH'):
            break
        print(line, end='', file=out)
    orig.close()
    out.write('% Added By locale-eaw\n')
    for start, end, width in width_list:
        #comment = comment_map.get(code, "none")
        if width == 2:
            print_locale(out, start, end)
    out.write('END WIDTH\n')
    out.close()
    print('done')
    print(f'Generating {path}.gz ... ', end='')
    with open(path, 'rb') as locale_file:
        with gzip.GzipFile(f'{path}.gz', 'wb', mtime=0) as locale_file_gz:
            shutil.copyfileobj(locale_file, locale_file_gz)
    print('done')

def generate_elisp(config, width_map, comment_map):
    flavor = config.name.lower()
    path = f'dist/{flavor}.el'
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w')
    header = open('eaw-header.el').read()
    out.write(header.format(flavor))
    print('(setq code-half \'(', file=out)
    for code, width in sorted(width_map.items()):
        if width == 1:
            print('  #x%04X' % (code), file=out)
    print('))', file=out)
    print('(setq code-wide \'(', file=out)
    for code, width in sorted(width_map.items()):
        if width == 2:
            print('  #x%04X' % (code), file=out)
    print('))', file=out)
    out.write(open('eaw-footer.el').read())
    print('done')

def generate_vimrc(config, width_list):
    flavor = config.name.lower()
    path = f'dist/{flavor}.vim'
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w')
    out.write('" Generated by locale-eaw\n')
    out.write('call setcellwidths([\n')

    for start, end, width in width_list:
        print(f'\[0x{start:x},0x{end:x},{width}],', file=out)
    out.write('\])')
    out.close()
    print('done')

if __name__ == '__main__':
    main()
