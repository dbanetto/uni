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
                elem.attrib[attr] = make_relative(domain, item_url)
            else:
                continue
            next.append(next_url)

        data = html.tostring(doc, 'utf-8')
        with open(to_save, 'wb') as f:
            f.write(data)
            f.flush()

    for u in next:
        if u not in got:
            got.add(u)
            pywget(url=u, depth=depth-1, got=got)

def make_path(url):
    mkpath = url.netloc
    if not os.path.exists(mkpath):
        os.mkdir(mkpath)

    for dir in path.dirname(url.path).split('/'):
        mkpath = path.join(mkpath, dir)
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

def make_relative(domain, abs):
    """
    domain:

    abs:

    returns:


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

