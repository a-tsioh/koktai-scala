# -*- coding:utf8 -*-
import mwclient
import argparse
import urllib



def main():
    parser = argparse.ArgumentParser(description='Upload content to Wikisource.')
    parser.add_argument("files", type=str, nargs='+')
    parser.add_argument('-u', '--username', type=str, nargs=1)
    parser.add_argument('-p', '--password', type=str, nargs=1)
    parser.add_argument('-r', '--root',type=str, action="store",default=u"國臺對照活用辭典/".encode("utf8"))
    args = parser.parse_args()
    site = mwclient.Site('ids-testing.wmflabs.org')
    site.login(args.username, args.password)
    for f in args.files:
        text = open(f,"r").read()
        url = (args.root + f.split("/")[-1]).decode("utf8")
        print url
        page = site.pages[url]
        print page
        page.save(text)
        


        
if __name__ == "__main__":
    main()
