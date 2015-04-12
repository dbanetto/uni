#!/usr/bin/python3

from lxml import html
from urllib.parse import urlparse
from urllib.request import urlopen
from urllib.error import URLError
from os import path
import os

def pywget(url=None, depth=2, got=set()):
    if type(url) is not str:
        raise TypeError('Expected type str for url but got', type(url))
    try:
        req = urlopen(url)
        domain = urlparse(url)
        print(domain)
        data = req.read()
        to_save = resolve_name(path.join(make_path(domain), path.basename(domain.path)))
        with open(to_save, 'wb') as f:
            f.write(data)
            f.flush()
        got.add(url)
        parse_file(url, depth, data, to_save , got)
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
        doc = html.fromstring(data.decode())
        for elem, attr, url_path, n in doc.iterlinks():
            item_url = urlparse(url_path)
            if item_url.netloc == '':
                next_url = make_abs(domain, item_url)
            elif item_url.netloc == domain.netloc:
                next_url = url_path
            else:
                continue
            if next_url not in got:
                got.add(next_url)
                next_path = pywget(url=next_url, depth=depth-1, got=got)
                elem.attrib[attr] = make_relative_local(to_save ,next_path)

        data = html.tostring(doc, 'utf-8')
        with open(to_save, 'wb') as f:
            f.write(data)
            f.flush()


def make_path(url):
    """
    url: A parsed url from urllib.parse.urlparse()

    Creates a directory structure of
        <domain>/path/to/file/...

    Returns:
        A str of the local path created
    """
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
    base:
        base path to be made relative to

    to_make:
        a path to be made relative to base

    return:
        a valid relative path to `to_make` from `base`
    """
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

def make_relative_url(domain, abs):
    """
    domain:
        A ParseResult of urllib.parse.urlparse()
        The base url

    abs:
        A ParseResult of urllib.parse.urlparse()
        An absoulte url to be made relative to `domain`
        Has to be of the same domain as `domain`

    errors:
        If abs and domain's .netloc differ a ValueError
        is raised

    returns:
        if abs has no domain returns abs's path

        otherwise returns a relative url of abs
        in terms of domain

    """
    if abs.netloc == '':
        return abs.path

    if abs.netloc != domain.netloc:
        raise ValueError('Urls are not of the same domain')

    relative = ""

    base_path = path.dirname(domain.path).split('/')[1:]
    rel_path = abs.path.split('/')[1:]

    while len(base_path) != 0:
        if rel_path[0] == base_path[0]:
            base_path = base_path[1:]
            rel_path = rel_path[1:]
        else:
            print('break')
            break

    print(rel_path)
    out_path = ['..' for i in base_path if i != '']
    out_path.extend(rel_path)

    return "/".join(out_path)


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
    print(make_relative_local('hello/words.html', 'woods/words.img'))
    pywget('http://homepages.ecs.vuw.ac.nz/~ian/nwen241/index.html')

