# -*- coding: utf8 -*-
import os
from collections import namedtuple
import re
import json

#QUESTIONS:
# bb1, fd0
VERBOSE = True

def debug(x):
    if VERBOSE:
        print unicode(x).encode("utf8")

CHAPTER = u".章首" # header of a chapter, the following line contains a syllable in zhuyin
ENTRY = u".本文" # start and end of an entry ( one character + words)
#CHAR = u"~fm7t168"#bb1;" # char entry followed by char, zhuyin and 反切
#CHAR2 = u"~fkt168"#bb1;" # char entry followed by char, zhuyin and 反切
CHAR_PRE_RECODE_re = re.compile("^~(fm7|fk|fm3)t168")
CHAR_re = re.compile("^<CHAR/>")
READING = u"~fm7;" # various readings of a char (mandarin, various hokkien accents, wen/bai)

WORD = u"~t96;" # start of a Word entry (untill next empty line or ENTRY)
KIND = u"~fk;" # start of a (台)類語 line

Chapter = namedtuple('Chapter',"zhuyin pinyin content chars words")
Char = namedtuple('Char', "hanji details readings content words")
Word = namedtuple('Word', "content")

private_to_unicode = json.load(open(os.path.dirname(__file__) + "/../Data/mapping.json")) # non-zhuyin in k font
m3_mapping = json.load(open(os.path.dirname(__file__) + "/../Data/m3.json")) # m3 font (all zhuyin)
k_mapping = json.load(open(os.path.dirname(__file__) + "/../Data/k.json")) # zhuyin in k font
missing_mapping = json.load(open(os.path.dirname(__file__) + "/../Data/missings.json")) #still missing after crowdsourcing
missing_mapping = {unichr(int("f"+k,16)):v for k,v in   missing_mapping.items()}
#m3_mapping.update(missing_mapping)
#private_to_unicode.update(missing_mapping)

re_fk = re.compile(ur"<k>.*?</k>", re.U)
re_change_font = re.compile(ur"~fk[a-z0-9]*;(.*?)~fm3[a-z0-9]*;",re.U)

def get_one_line(buffer):
    try:
        line = buffer.pop(0)
        #debug(line)
        return line
    except:
        return None


def match_apply(re, f, s):
    """
    apply f on every match of re in s
    """
    m = re.search(s)
    output = []
    while m is not None:
        #debug("all = " + s)
        (begin, end) = m.span()
        output.append(s[:begin])
        output.append(f(s[begin:end]))
        #debug("sub = " + output[-1])
        s = s[end:]
        m = re.search(s)
    output.append(s)
    return "".join(output)

k_re = re.compile(ur"<k>.*?<\/k>",re.U)
k1_re = re.compile(ur"[\U000fc6a1-\U000fc6a9]", re.U) # chiffres encerclés, enlever <k/>
k2_re = re.compile(ur"[\U000F8000-\U000Fafff\U000FF000-\U000FFFFF]", re.U) #zhuyin si mapping dispo ou private ou mark (rendu direct)
k3_re = re.compile(ur"[\U000F0000-\U000F7fff\U000Fb000-\U000FeFFF]", re.U) #idem mais image dans img/k si fail

# si pas de markage (k2 k2 k3 ont rien fait?)
k4_re = re.compile(ur"[\U000fc6a1-\U000fc6a9](?!</mark>)(?!</rt>)", re.U) #idem que k1 mais sans enlever le <k/>
k5_re = re.compile(ur"[\U000Fc000-\U000Fcfff](?!</mark>)(?!</rt>)", re.U) 
k6_re = re.compile(ur"[\U000F0000-\U000Fffff](?!</mark>)(?!</rt>)", re.U)
def recode(s):
    if CHAR_PRE_RECODE_re.match(s):
        s = "<CHAR/>" + s
    s = re_change_font.sub("<k>\\1</k>",s)
    def k1(s):
        return match_apply(k1_re, lambda x: unichr(0x245f + ord(x) - 0xfc6a0), s[3:-4]) # remove </k>
    def k2(s):
        def f(sub):
            code = "%04x" %(ord(sub) - 0xF0000,)
            if code in k_mapping:
                return "<rt>%s</rt>" % (k_mapping[code],)
            if sub in private_to_unicode:
                return private_to_unicode[sub]
            return "<mark>&#xf%s;</mark>" % (code,)
        return match_apply(k2_re, f, s)
    def k3(s):
        def f(sub):
            code = "%04x" % (ord(sub) - 0xF0000,)
            if code in k_mapping:
                return "<rt>%s</rt>" % (k_mapping[code],)
            if sub in private_to_unicode:
                return private_to_unicode[sub]
            return "<img src=\"img/k/%s.png\" />" % (sub,)
        return match_apply(k3_re, f, s)

    def k5(s):
        def f(sub):
            code = "%04x" %(ord(sub) - 0xF0000,)
            if code in m3_mapping:
                return m3_mapping[code]
            if sub in missing_mapping:
                return missing_mapping[sub]
            return "<img src=\"img/m3/%s.png\" />" % (code,)
        return match_apply(k5_re, f, s)
    def k6(s):
        def f(sub):
            code = "%04x" %(ord(sub) - 0xF0000,)
            if code in m3_mapping:
                return "<rt>%s</rt>" % (m3_mapping[code],)
            if sub in missing_mapping:
                return missing_mapping[sub]
            return "<img src=\"img/m3/%s.png\" />" % (code,)
        return match_apply(k6_re, f, s)

    s = match_apply(k_re, lambda x: k3(k2(k1(x))), s)
    s = match_apply(k4_re, lambda x: unichr(0x245f + ord(x) - 0xfc6a0), s)
    return k6(k5(s)).replace("</rt>/<rt>","/")


if __name__ == "__main__":
    import sys
    import codecs
    import pickle
    data = [recode(l.decode("utf8")) for l in sys.stdin.readlines()]
    print "\n".join(data).encode("utf8")
    sys.exit(0)
