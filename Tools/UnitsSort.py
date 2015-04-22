#!/usr/bin/python
# -*- coding: utf-8 -*-


import os
import re
import logging


def init_log(log_file, level=logging.NOTSET):
    logging.basicConfig(level=level,
                        format='%(asctime)s.%(msecs).03d %(levelname)-8s %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S',
                        filename=log_file,
                        filemode='w')
    console = logging.StreamHandler()
    console.setLevel(level)
    formatter = logging.Formatter('%(levelname)-8s %(message)s')
    console.setFormatter(formatter)
    logging.getLogger('').addHandler(console)

    
def check_path(path):
    if path:
        path = os.path.abspath(path)
        if path and path[-1:] != os.path.sep:
            path += os.path.sep
    return path


def sort_dpr(proj_file, by_unit_name=True):
    logging.info('Sorting uses in dpr: ' + proj_file)

    def sort_func_by_unit_path(x, y):
        x1 = x.split(' in ')
        y1 = y.split(' in ')
        return cmp(x1[1], y1[1])

    def uses_to_text(uses_list, text):
        for unit in uses_list:
            if unit:
                if text:
                    text += ','
                text += unit
        return text

    with open(proj_file, 'rb') as f:
        data = f.read()

    uses_sys = []
    uses_sorted = []

    uses_text = ''

    match = re.findall(r'uses\r\n(.*?);', data, re.DOTALL | re.IGNORECASE)
    if match:
        uses = match[0].split(',')
        for pas in uses:
            if ' in ' in pas.lower() and not ('sasplanet.modules' in pas.lower()):
                uses_sorted.append(pas)
            else:
                uses_sys.append(pas)

        if uses_sorted:
            if by_unit_name:
                uses_sorted.sort()
            else:
                uses_sorted.sort(cmp=sort_func_by_unit_path)

            uses_text = uses_to_text(uses_sys, uses_text)
            uses_text = uses_to_text(uses_sorted, uses_text)

            if uses_text:
                if match[0] != uses_text:
                    data = data.replace(match[0], uses_text)
                    with open(proj_file, 'wb') as f:
                        f.write(data)
                    logging.info('Sorting update')
                else:
                    logging.info('Sorting OK')


def patch_proj_file(proj_file, proj_info):
    
    logging.info('Patching file: ' + proj_file)
    is_patched = False
    
    # read proj
    with open(proj_file, 'rb') as f:
        data = f.read()

    # apply patch
    unit_expr = r"[\"'](.*?)[\"']"
    for match in re.finditer(unit_expr, data, re.DOTALL | re.IGNORECASE):
        if match.group(1).endswith('.pas'):
            unit_path, unit_name = os.path.split(match.group(1))
            for pas, unit in proj_info:
                if unit_name in pas:
                    if unit != match.group(1):
                        is_patched = True
                        data = data.replace(match.group(1), unit)
                        logging.debug('Replaced: {} --> {}'.format(match.group(1), unit))
                    else:
                        logging.debug('OK: {} == {}'.format(match.group(1), unit))

    if not is_patched:
        logging.info('Proj file ' + proj_file + ' is OK')
    else:
        # save proj    
        with open(proj_file, 'wb') as f:
            f.write(data)
        logging.info('Proj file ' + proj_file + ' is updated')

    
def main(src_path):

    flst = []

    logging.info('Scan file system: ' + src_path)
    
    for root, dirs, files in os.walk(src_path):

        if '.bin' in dirs:
            dirs.remove('.bin')
        if '.dcu' in dirs:
            dirs.remove('.dcu')
        if '.hg' in dirs:
            dirs.remove('.hg')
        if '__history' in dirs:
            dirs.remove('__history')

        for pasfile in files:
            if pasfile.endswith('.pas'):
                unit = root.replace(src_path, '')
                if unit:
                    unit += '\\'
                unit += pasfile
                flst.append((pasfile, unit))
                logging.debug('Found ' + pasfile + ' in ' + unit)

    if flst:
        patch_proj_file(src_path + 'SASPlanet.dpr', flst)
        patch_proj_file(src_path + 'SASPlanet.dproj', flst)
        patch_proj_file(src_path + 'SASPlanet.XE2.dproj', flst)
        sort_dpr(src_path + 'SASPlanet.dpr', False)


if __name__ == '__main__':
    init_log('UnitsSort.log', logging.INFO)
    src = '..\\'
    main(check_path(src))