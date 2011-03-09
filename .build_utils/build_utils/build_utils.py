#!/usr/bin/env python
# Setup and utilities for building the project in buildfile
#
# TODO:
#  * Directory creation when needed

import os, sys, glob, re
from fabricate import *

# Pop this stuff off of argv
DEBUG = True
for command in sys.argv:
    if command == '-production':
        DEBUG = False
        sys.argv.remove('-production')

def flat(l, ltypes=(list, tuple)):
    ltype = type(l)
    l = list(l)
    i = 0
    while i < len(l):
        while isinstance(l[i], ltypes):
            if not l[i]:
                l.pop(i)
                i -= 1
                break
            else:
                l[i:i + 1] = l[i]
        i += 1
    return ltype(l)

sources = ['./', 'src']
dirs = [sources]
if os.path.isdir('ebin'): dirs.append('ebin')
if os.path.isdir('priv'): dirs.append('priv')
if os.path.isdir('include'): dirs.append('include')
if os.path.isdir('grammar'): dirs.append('grammar')
setup(dirs=flat(dirs), runner='smart_runner')

def to_camelcase(s):
    return ''.join([w.title() for w in s.split('_')]);

def p(*args):
    '''Shorthand for joining path segments together into one path'''
    return os.path.normpath(os.path.join(*args))

BUILD_UTIL_DIR = os.path.abspath(os.path.dirname(__file__) + '/')
ROOT_DIR = p(BUILD_UTIL_DIR, '..', '..')

def mkdir(*path):
    '''Shorthand for mkdir -p and then any number of path segments.'''
    run('mkdir', '-p', p(*path))

def rmdir(*path):
    run('rm', '-rf', p(*path))

def neotoma(grammar_definition):
    gdef = grammar_definition + '.peg'
    gcomp = grammar_definition + '.erl'
    if os.path.isfile(p('grammar',gdef)): gdir = 'grammar'
    else: gdir = 'priv'
    run('erl', '-noinput', '-eval', 'neotoma:file("'+p(gdir,gdef)+'").', '-run', 'init', 'stop')
    erlc(grammar_definition,gdir)

def erlc(module_name,src_dir='src'):
    if not os.path.isdir('ebin'):
        mkdir('ebin')
    if os.path.isdir('include'):
        run('erlc','-o','ebin/','-I','include',p(src_dir,module_name+'.erl'))
    else:
        run('erlc','-o','ebin/',p(src_dir,module_name+'.erl'))


#------------------------------------------------------
# Make sure we're in the right working directory to begin with (possibly not
# needed and possibly even interferes w/ fabricate later, we'll see)
#os.chdir(CLIENT_ROOT_DIR)


