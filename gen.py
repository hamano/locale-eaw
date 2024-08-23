#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import configparser
import gzip
import shutil

UCD_DIR='./ucd'
EAW_FILE = f'{UCD_DIR}/EastAsianWidth.txt'
EMOJI_FILE = f'{UCD_DIR}/emoji/emoji-data.txt'
ORIGINAL_FILE = f'{UCD_DIR}/UTF-8'

class UCD:
    def __init__(self, ucd_dir):
        self.ucd_dir = ucd_dir
        self.ucd, self.ucd_range = self.load_unicode_data()
        self.eaw = self.load_eaw()
        self.group = {}
        self.group['amb'] = self.load_amb()
        self.group['private'] = self.load_private()
        self.group['nerdfont'] = self.load_nerdfont()
        self.jis = self.load_jis()
        self.group['jpdoc'] = self.load_jpdoc()
        self.group['reference_mark'] = [0x203B]
        # 0x24EA と 0x1F10C は neutral
        self.group['circled_digit'] = \
            list(range(0x2460, 0x2473 + 1)) + [0x24EA] + \
            list(range(0x2776, 0x277F + 1)) + list(range(0x24EB, 0x24F4 + 1)) + [0x24FF] + \
            list(range(0x278A, 0x2793 + 1)) + [0x1F10C] + \
            list(range(0x24F5, 0x24FE + 1)) + \
            list(range(0x3248, 0x324F + 1))
        self.group['parenthesized_digit'] = range(0x2474, 0x2487 + 1)
        self.group['digit_full_stop'] = range(0x2488, 0x249B + 1)
        self.group['parenthesized_latin'] = list(range(0x249C, 0x24B5 + 1)) + list(range(0x1F110, 0x1F129 + 1))
        self.group['circled_latin'] = range(0x24B6, 0x24E9 + 1)

    def get_block(self, name):
        return self.blocks.get(name)

    def get_name(self, code):
        data = self.ucd.get(code)
        if data:
            return data['name']
        for data_range in self.ucd_range:
            if data_range['first'] <= code <= data_range['last']:
                return data_range['name']
        return None

    def load_unicode_data(self):
        ucd = {}
        ucd_range = []
        path = f'{self.ucd_dir}/UnicodeData.txt'
        with open(path, mode='r') as f:
            for line in f:
                #print(line)
                row = line.split(';')
                if row[1].endswith(', First>'):
                    first = row
                    continue
                if row[1].endswith(', Last>'):
                    ucd_range.append({
                        'first': int(first[0], 16),
                        'last': int(row[0], 16),
                        'name': first[1].removeprefix('<').removesuffix(', First>'),
                        'category': first[2],
                        'combining': first[3],
                        'comment': first[11],
                    })
                    continue
                ucd[int(row[0], 16)] = {
                    'name': row[1],
                    'category': row[2],
                    'combining': row[3],
                    'comment': row[11],
                }
        return ucd, ucd_range

    # East Asian Widthマップを生成
    def load_eaw(self):
        path = f'{self.ucd_dir}/EastAsianWidth.txt'
        ret = {}
        line_re = re.compile('([0-9A-Fa-f\.]+)\s*;\s*(\w+)\s+#\s+(.*)')
        f = open(path)
        for line in f:
            if line.strip() == '' or line.startswith('#'):
                continue
            match = line_re.match(line)
            if not match:
                print(f'unexpected format: {line}', file=sys.stderr)
                continue
            (code_or_range, eaw, comment) = match.groups()
            if '.' in code_or_range:
                # range code
                (first, last) = tuple(code_or_range.split('..'))
                code = int(first, 16)
            else:
                # single code
                code = int(code_or_range, 16)

            if '.' in code_or_range:
                for i in range(int(first, 16), int(last, 16) + 1):
                    ret[i] = eaw
            else:
                ret[code] = eaw
        f.close()
        return ret

    def load_amb(self):
        ret = []
        for c, w in self.eaw.items():
            if w != 'A':
                continue

            # exclude Combining Diacritical Marks
            if 0x0300 <= c <= 0x036F:
                continue

            # exclude Variation Selectors
            if 0xFE00 <= c <= 0xFE0F:
                continue

            # exclude Variation Selectors Supplement
            if 0xE0100 <= c <= 0xE01EF:
                continue

            ret.append(c)
        return ret

    def load_private(self):
        ret = []
        # Private Use Area
        ret.extend(range(0xE000, 0xF8FF + 1))
        # Supplementary Private Use Area-A
        ret.extend(range(0xF0000, 0xFFFFF + 1))
        # Supplementary Private Use Area-B
        ret.extend(range(0x100000, 0x10FFFD + 1))
        return ret

    def load_nerdfont(self):
        ret = []
        with open('nerdfont/list.txt') as f:
            for line in f:
                if line.startswith('#'):
                    continue
                ret.append(int(line, 16))
        return ret

    def load_jis(self):
        ret = []
        with open(f'{self.ucd_dir}/JIS0208.TXT') as f:
            for line in f:
                if line.startswith('#'):
                    continue

                row = line.split('\t')
                jis = int(row[1], 16)
                uni = int(row[2], 16)

                # 非漢字は8区 0x2070 まで
                if jis > 0x2870:
                    break
                eaw = self.eaw.get(uni)
                if eaw != 'A':
                    continue
                ret.append(uni)
        return sorted(ret)

    def load_jpdoc(self):
        ret = []
        for c in self.jis:
            if 0x203B <= c <= 0x2312:
                ret.append(c)
            if 0x25A0 <= c <= 0x266F:
                ret.append(c)
        return ret

def main():
    ucd = UCD(UCD_DIR)
    generate_list('test/amb.txt', ucd.group['amb'], ucd)
    generate_list('test/nerdfont.txt', ucd.group['nerdfont'], ucd)
    generate_list('test/jis.txt', ucd.jis, ucd)
    generate_list('test/jpdoc.txt', ucd.group['jpdoc'], ucd)
    config = configparser.ConfigParser()
    config.read('config.ini')
    for name in config:
        if name == 'DEFAULT':
            continue
        generate_flavor(config[name], ucd)
    print('Generation complete.')

def generate_all():
    for section in config.sections():
        generate_flavor(config[section])

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

def set_width(width_map, ucd, name, config):
    name = name.lower()
    width = config.getint(name)

    if name.startswith('u+'):
        name = name.removeprefix('u+')
        code_list = [int(name, 16)]
    elif name in ucd.group:
        code_list = ucd.group[name]
    else:
        print(f'warning: unknown group name {name}', file=sys.stderr)
        return
    for code in code_list:
        width_map[code] = width
    return

def generate_flavor(config, ucd):
    flavor = config.name
    print(f'# {flavor} Flavor')
    width_map = {}

    for name in config:
        set_width(width_map, ucd, name, config)

    width_list = range_compress(width_map)
    generate_locale(config, width_list)
    generate_elisp(config, width_list)
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

def generate_list(path, code_list, ucd):
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w', encoding='UTF-8')

    for code in code_list:
        name = ucd.get_name(code)
        c = chr(code)
        print(f'[{c}] U+{code:04X} {name}', file=out)
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

def generate_elisp(config, width_list):
    flavor = config.name.lower()
    path = f'dist/{flavor}.el'
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w')
    header = open('eaw-header.el').read()
    out.write(header.format(flavor))
    print('(setq code-half \'(', file=out)
    for start, end, width in width_list:
        if width == 1:
            if start == end:
                print(f'  #x{start:x}', file=out)
            else:
                print(f'  (#x{start:x}.#x{end:x})', file=out)
    print('))', file=out)
    print('(setq code-wide \'(', file=out)
    for start, end, width in width_list:
        if width == 2:
            if start == end:
                print(f'  #x{start:x}', file=out)
            else:
                print(f'  (#x{start:x}.#x{end:x})', file=out)
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
