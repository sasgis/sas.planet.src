#!/usr/bin/env python3

import argparse
import os
import re
import shlex
import shutil
import subprocess

DLL_REGEX = re.compile(r"DLL Name:\s*(\S+)")

SYSTEM_DLL = ["msvcrt.dll", "kernel32.dll", "user32.dll", "oleaut32.dll",
              "ntdll.dll", "advapi32.dll", "ws2_32.dll", "bcrypt.dll",
              "userenv.dll", "shell32.dll", "crypt32.dll", "wldap32.dll"]


def check_file(filename):
    print("checking dependencies for {}".format(filename))

    command = "objdump -p {}".format(filename)
    result = subprocess.check_output(shlex.split(command)).decode()

    hitnames = set()
    for line in result.splitlines():
        match = DLL_REGEX.search(line)
        if match:
            hitname = match.group(1)
            if hitname.lower() not in SYSTEM_DLL:
                hitnames.add(hitname)

    return hitnames


class Dependency:
    _filename = ""
    _filepath = ""
    _checked = False
    _searched = False
    _found = False

    def __init__(self, filename):
        self._filename = filename.lower()
        self._regex = re.compile(re.escape(self._filename), re.IGNORECASE)

    def __eq__(self, other):
        return self._filename == other.filename

    def __hash__(self):
        return hash(self._filename)

    @property
    def checked(self):
        return self._checked

    @property
    def found(self):
        return self._found

    @property
    def filename(self):
        return self._filename

    @property
    def filepath(self):
        return self._filepath

    def find(self, dirs):
        if self._searched:
            return
        self._searched = True

        hits = {}
        for directory in dirs:
            for dirpath, dirnames, filenames in os.walk(directory):
                for filename in filenames:
                    if self._regex.fullmatch(filename):
                        # Filenames are unique, directories are not
                        # (e.g. a.dll and A.DLL in the same directory)
                        hits[filename] = dirpath
        if len(hits.items()) == 0:
            print("-", self._filename, "missing")
            self._found = False
            self._checked = True
        elif len(hits.items()) == 1:
            filename, dirpath = list(hits.items())[0]
            self._filepath = os.path.join(dirpath, filename)
            self._found = True
        else:
            print("\n" + self._filename, "found in multiple directories")
            print("please choose one:")
            choices = list(hits.items())
            for filename, dirpath in choices:
                print(" [{}] {}".format(choices.index((filename, dirpath)),
                                        os.path.join(dirpath, filename)))
            while True:
                answer = input('> ')
                idx = -1
                try:
                    idx = int(answer)
                except ValueError:
                    pass
                if idx < len(hits):
                    break
                else:
                    print("invalid answer")
            filename, dirpath = list(hits.items())[idx]
            self._filepath = os.path.join(dirpath, filename)
            self._found = True

    def check(self):
        if self._checked:
            return []
        if not self._searched:
            return []
        if not self._found:
            return []
        self._checked = True

        return check_file(self._filepath)


class DependencyChecker:
    _filename = ""
    _dirs = []
    _deps = set()
    _ignore_files = []

    def __init__(self, filename, dirs, ignore_files):
        self._filename = filename
        if dirs:
            self._dirs = dirs
        else:
            self._dirs = ['/']
        if ignore_files:
            self._ignore_files = ignore_files

    def run(self):
        if not os.path.isfile(self._filename):
            print("File does not exist: {}".format(self._filename))
            return [], []

        hitnames = check_file(self._filename)
        for filename in hitnames:
            self.add_dependency(filename)

        while len(self.not_checked) > 0:
            for dep in self.not_checked:
                dep.find(self._dirs)
                hitnames = dep.check()
                for filename in hitnames:
                    self.add_dependency(filename)

        return self.found_paths, self.not_found_names

    def add_dependency(self, filename):
        if filename in self._ignore_files:
            print("skip dependencies check for {}".format(filename))
        else:
            self._deps.add(Dependency(filename))

    @property
    def checked(self):
        return [i for i in self._deps if i.checked]

    @property
    def not_checked(self):
        return [i for i in self._deps if not i.checked]

    @property
    def found(self):
        return [i for i in self._deps if i.found]

    @property
    def not_found(self):
        return [i for i in self._deps if not i.found]

    @property
    def found_paths(self):
        return [i.filepath for i in self.found]

    @property
    def not_found_names(self):
        return [i.filename for i in self.not_found]


def main():
    parser = argparse.ArgumentParser(description='check DLL dependencies')
    parser.add_argument('FILE', help='check the dependencies of this file')
    parser.add_argument('-d', '--dir', nargs='+',
                        help='check for dependencies in these directories')
    parser.add_argument('-t', '--target-dir',
                        help='copy dependencies into this directory (optional)')
    parser.add_argument('-i', '--ignore', nargs='+',
                        help='exclude dependencies from check (optional)')

    args = parser.parse_args()

    checker = DependencyChecker(args.FILE, args.dir, args.ignore)
    found, notfound = checker.run()

    print("\n*** Found dependencies: ***")
    if len(found) > 0:
        found.sort()
        for filepath in found:
            print('+', filepath)
    else:
        print("None")

    print("\n*** Missing dependencies: ***")
    if len(notfound) > 0:
        notfound.sort()
        for filename in notfound:
            print('-', filename)
    else:
        print("None")

    if args.target_dir and len(found) > 0:
        print('\nCopying files to', args.target_dir)
        for source_path in found:
            source_dir, source_name = os.path.split(source_path)
            print('>', source_name)
            shutil.copy2(source_path, args.target_dir)

    print('\nDone!')


if __name__ == '__main__':
    main()
