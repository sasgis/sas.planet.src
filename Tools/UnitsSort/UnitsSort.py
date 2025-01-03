#!/usr/bin/python
# -*- coding: utf-8 -*-


import os
import re
import sys
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


def read_content(file_name):
    with open(file_name, 'rb') as f:
        content = f.read()
        if sys.version_info[0] >= 3:
            content = content.decode('utf-8')
        return content


def write_content(file_name, content):
    if sys.version_info[0] >= 3:
        content = content.encode('utf-8')
    with open(file_name, 'wb') as f:
        f.write(content)
        

def sort_dpr(proj_file, by_unit_name=True):
    logging.info('Sorting uses in dpr: ' + proj_file)

    def cmp_by_unit_path(item):
        unit_path = item.split(' in ')
        return unit_path[1]

    def uses_to_text(uses_list, text):
        for unit in uses_list:
            if unit:
                if text:
                    text += ','
                text += unit
        return text

    data = read_content(proj_file)

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
                uses_sorted.sort(key=cmp_by_unit_path)

            uses_text = uses_to_text(uses_sys, uses_text)
            uses_text = uses_to_text(uses_sorted, uses_text)

            if uses_text:
                if match[0] != uses_text:
                    data = data.replace(match[0], uses_text)
                    write_content(proj_file, data)
                    logging.info('Sorting update')
                else:
                    logging.info('Sorting OK')


def patch_proj_file(proj_file, proj_info):
    
    logging.info('Patching file: ' + proj_file)
    is_patched = False
    
    # read proj
    data = read_content(proj_file)

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
        write_content(proj_file, data)
        logging.info('Proj file ' + proj_file + ' is updated')


def process_project(root_path, dpr_name):

    flst = []
    dproj = []
    
    logging.info('Scan file system: ' + root_path)
    
    for root, dirs, files in os.walk(root_path):

        for dir in dirs:
            if dir.startswith('.') or dir.startswith('__'):
                dirs.remove(dir)

        for f in files:
            if f.endswith('.dproj') and root == root_path:
                dproj.append(f)
            elif f.endswith('.pas'):
                unit = root.replace(root_path, '')
                if unit:
                    unit += '\\'
                unit += f
                flst.append((f, unit))
                logging.debug('Found ' + f + ' in ' + unit)

    if flst:
        patch_proj_file(root_path + dpr_name, flst)
        sort_dpr(root_path + dpr_name, False)
        
        for proj in dproj:
            patch_proj_file(root_path + proj, flst)


if __name__ == '__main__':
    init_log('UnitsSort.log', logging.INFO)
    
    root_path = '..\\..\\'
    
    projects = (
        (root_path, 'SASPlanet.dpr'), 
        (root_path + 'Test', 'SASPlanetTests.dpr'),
        (root_path + 'Benchmark', 'BenchmarkCmd.dpr'),
    )
    
    for path, dpr in projects:
        process_project(check_path(path), dpr)
