#!/usr/bin/env python3

from urllib.parse import urlparse, ParseResult
from urllib.request import urlopen
from urllib.error import URLError
from os import path
import os, re

def pywget(url=None, depth=1, got=set()):
    if type(url) is not str:
        raise TypeError('Expected type str for url but got ' + type(url))
    if type(depth) is not int:
        raise TypeError('Expected type int for depth but got ' + type(depth))
    if type(got) is not set:
        raise TypeError('Expected type set for got but got ' + type(got))

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
        got.add(url)
        parse_file(url, depth, data, to_save, got)
        return to_save

    except URLError as e:
        print('Url error', e)
    except ValueError as e:
        print('Value Error', e)

def parse_file(url, depth, data, to_save, got):
    """
    Parse the file to correct html links to local
    version
    """
    if depth <= 0:
        return

    domain = urlparse(url)
    next = []
    if domain.path.endswith('.html') or to_save.endswith('.html'):
        editted_doc = data.decode()
        for link in re.finditer('[href|src]=\"(\S+?)\".*?>', data.decode()):
            url_path, = link.groups()
            item_url = urlparse(url_path)
            if item_url.netloc == '': # relative url
                next_url = make_abs(domain, item_url)
            elif item_url.netloc == domain.netloc: # absolute url
                next_url = url_path
            else: # not of this domain (wikipedia etc.)
                continue

            if next_url not in got:
                got.add(next_url)
                next_path = pywget(url=next_url, depth=depth-1, got=got)
                editted_doc = re.sub(url_path, make_relative_local(to_save ,next_path), editted_doc)

        with open(to_save, 'wb') as f:
            f.write(editted_doc.encode())
            f.flush()


def make_path(url):
    """
    url: A parsed url from urllib.parse.urlparse()

    Creates a directory structure of
        <domain>/path/to/file/...

    Returns:
        A str of the local path created
    """
    if type(url) is not ParseResult:
        raise TypeError('Expected type ParseResult for base but got ' + type(url))

    mkpath = url.netloc
    if not os.path.exists(mkpath):
        os.mkdir(mkpath)

    for dir in path.dirname(url.path).split('/'):
        mkpath = path.join(mkpath, dir) # build a platform independent path
        if not os.path.exists(mkpath):
            os.mkdir(mkpath)
    return mkpath

def make_abs(domain, relative):
    """
    domain:
        A ParseResult of urllib.parse.urlparse()
        the base url

    relative:
        A ParseResult of urllib.parse.urlparse()
        The relative url to be solved using the
        base url as the 'working directory'

    Returns:
        if a relative path
        returns absoulte url of relative in terms of domain
        otherwise return url of relative
    """
    if type(domain) is not ParseResult:
        raise TypeError('Expected type ParseResult for domain but got ' + type(domain))
    if type(relative) is not ParseResult:
        raise TypeError('Expected type ParseResult for relative but got ' + type(relative))

    if relative.netloc != '':
        return relative.geturl()

    rel_path = domain.path if domain.path[-1] == '/' else path.dirname(domain.path)
    for step in relative.path.split('/'):
        if step == '..':
            rel_path = path.dirname(rel_path)
        elif step == '.':
            pass
        else:
            rel_path += '/' + step

    return domain.scheme + '://' + domain.netloc +  rel_path

def make_relative_local(base, to_make):
    """
    Make to_make relative to base

    base:
        base path to be made relative to

    to_make:
        a path to be made relative to base

    return:
        a valid relative path to `to_make` from `base`
    """
    if type(base) is not str:
        raise TypeError('Expected type str for base but got ' + type(base))
    if type(to_make) is not str:
        raise TypeError('Expected type str for to_make but got ' + type(to_make))

    relative = ""

    base_path = path.dirname(base).split('/')
    rel_path = to_make.split('/')

    while len(base_path) != 0:
        if rel_path[0] == base_path[0]:
            # pop the shared path
            base_path = base_path[1:]
            rel_path = rel_path[1:]
        else:
            # reached difference
            # cannot go on further
            break

    # build necessary number of relative 'directory-ups'
    out_path = ['..' for i in base_path if i != '']
    # slap the rest of the to_make path to end
    out_path.extend(rel_path)

    return os.path.join(*out_path)


def resolve_name(name):
    """
    Check if the given name exists, if so return an adjusted file name

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
