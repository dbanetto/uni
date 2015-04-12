#!/usr/bin/env python3

from urllib.parse import urlparse
from urllib.request import urlopen
from urllib.error import URLError
from os import path
import os

def pywget(url=None):
    if type(url) is not str:
        raise TypeError('Expected type str for url but got ' + type(url))

    try:
        domain = urlparse(url)
        print('Downloading', url)
        req = urlopen(url)
        data = req.read()
        to_save = resolve_name(path.basename(domain.path))
        with open(to_save, 'wb') as f:
            f.write(data)
            f.flush()
        print('Saved', url, 'to', to_save)

    except URLError as e:
        print('Url error', e)
    except ValueError as e:
        print('Value Error', e)


def resolve_name(name):
    """
    Check if the given name exists

    name:
        A local path to a file that does not exist

    returns:
        If it does not just return the name
        If the name already exists it will increment a counter using
        a <file name>.<numb>.<file extension> format
    """
    if type(name) is not str:
        raise TypeError('Expected type str for name but got ' + type(name))

    if not path.exists(name):
        return name

    (file, ext) = path.splitext(name)
    n = 1
    tmp_name = file + '.' + str(n) + ext
    while path.exists(tmp_name):
        n += 1
        tmp_name = file + '.' + str(n) + ext

    return tmp_name


if __name__ == '__main__':
    pywget('http://homepages.ecs.vuw.ac.nz/~ian/nwen241/index.html')
