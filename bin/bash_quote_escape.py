import sys
from StringIO import StringIO

escmap = {
    0x07: '\\a',
    0x08: '\\b',
    0x09: '\\t',
    0x0a: '\\n',
    0x0b: '\\v',
    0x0c: '\\f',
    0x0d: '\\r',
    0x1b: '\\e',
    ord('\\'): '\\\\',
    ord("'"): "\\'",
    ord("!"): "\\x21",
}

def escape(s):
    r = StringIO()
    for c in s:
        if ord(c) in escmap:
            r.write(escmap[ord(c)])
        else:
            r.write(c)
    return r.getvalue()

def p(*kargs):
    return " ".join(kargs)

def e(*kargs):
    return "$'"+escape(p(*kargs))+"'"

def P(*kargs):
    print " ".join(kargs)

print '''
Usage:
    python -i -m bash_quote_escape

Input <P-e-command> while prompting

    `P-e-command' is the comamnd line that expressed using P() and e() premitives.

    e(a,b,...) escape the simple command which consist of "a b ..." using $''

    P(a,b,...) prints the simple command which consist of "a b ..."

Example:
    >>> P('ssh 10.1.2.31', e('sudo -u admin sh -c', e("ifconfig | grep flags | awk '{print $2}'")))

You will get:
    ''' + p('ssh 10.1.2.31', e('sudo -u admin sh -c', e("ifconfig | grep flags | awk '{print $2}'")))
