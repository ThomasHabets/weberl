#!/usr/bin/python

import re

def indent(s):
    return len(re.match('^( *).*',s).group(1))

def getFuncId():
    ret = getFuncId.idCounter
    getFuncId.idCounter += 1
    return ret
getFuncId.idCounter = 1

class Construct(object):
    def __init__(self, children):
        self.children = children

class ForConstruct(Construct):
    def __init__(self, children, array, other):
        self.children = children
        self.id = getFuncId()
        self.funcname = "for_%d" % (self.id)
        self.array = array
        self.other = other
    def describe(self, ind = 0):
        ret = " " * ind + "for (%s)" %(self.funcname)
        for c in self.children:
            ret += "\n" + c.describe(ind+2)
        return ret
    def call(self):
        return '%s(%s,"",%s)' % (self.funcname, self.array, self.other)
    def implementation(self):
        ret = '''%(funcName)s([],S,%(other)s) -> S;
%(funcName)s(L,S,%(other)s) ->
  [H|T] = L,
  S2 = %(children)s,
  %(funcName)s(T,S2,%(other)s).

''' % ({'funcName': self.funcname,
        'children': '++'.join([x.call()
                               for x in self.children
                               if len(x.call())]),
        'other': self.other
        })
        for c in self.children:
            ret += c.implementation()
        return ret

    def __repr__(self):
        return "for:"+str(self.children)

class TextConstruct(Construct):
    def describe(self, ind=0):
        return (" " * ind + "text len %d" % (len(self.children)))
    def call(self):
        s = self.children
        s = s.replace('\n', '\\n')
        return '"%s"' % (s)
    def implementation(self):
        return ''
    def __repr__(self):
        return self.children

class Parser:
    def __init__(self):
        self.idCounter = 1

    def runFile(self, f):
        first = f.readline()
        m = re.match(r'^\$def with \((.*)\)$', first)
        if m:
            self.parms = ','.join([x.capitalize()
                                   for x in re.split(r'[, ]',m.group(1))
                                   if len(x)])
        return self.runLines(f.readlines())
        
    def runLines(self, lines):
        children = []
        ind = indent(lines[0])
        lineNo = -1
        ret = []
        const = ''
        while True:
            lineNo += 1
            if lineNo >= len(lines):
                break
            line = lines[lineNo]
            ind = len(re.match(r'^( *).*', line).group(1))
            mfor = re.match(r' *\$for \(?(.*?)\)? in (.*):$', line)
            if mfor:
                parms = [x.strip()
                         for x in re.split('[ ,]', mfor.group(1))]
                array = mfor.group(2).capitalize()
                if len(const):
                    ret.append(TextConstruct(const))
                    const = ''
                sub = []
                lineNo += 1
                while indent(lines[lineNo]) > ind:
                    sub.append(lines[lineNo])
                    lineNo += 1
                    if lineNo >= len(lines):
                        break
                
                lineNo -= 1
                ret.append(ForConstruct(self.runLines(sub), array, self.parms))
            else:
                const += lines[lineNo]
        ret.append(TextConstruct(const))
        self.parsed = ret
        return ret

class Create:
    def __init__(self, name, parsed, parms):
        self.name = name
        self.parsed = parsed
        self.parms = parms
    def run(self):
        #for entry in self.parsed:
        #    print entry.describe()
        #print "----"
        #for entry in self.parsed:
        #    print entry.call()
        #print "----"
        #for entry in self.parsed:
        #    print entry.implementation()
        #print "========"

        s = '\n'.join([x.implementation() for x in self.parsed])
        s += "%s(%s) ->\n  " % (self.name, self.parms)
        s += '\n  ++'.join([x.call()
                            for x in self.parsed
                            if len(x.call())])
        s += "."
        return s

def main():
    t = Parser()
    p = t.runFile(open("test.tmpl"))
    c = Create('index', p, t.parms)
    print c.run()

main()
