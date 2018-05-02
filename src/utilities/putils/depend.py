#!/usr/bin/python

import os.path,re,os
from optparse import OptionParser

parse=OptionParser()
parse.add_option("--flags",dest="cmp",help="Add a string of flags")
parse.add_option("-d","--dir",dest="srcdir",help="give source directory",default=".")
parse.add_option("-x","--xdir",dest="xdir",help="source directory to exclude from search path",default=[],action="append")
(opts,objlist)=parse.parse_args()

for ii,obj in enumerate(objlist):
	objlist[ii]=obj.replace('.o','')

fmod=re.compile(r"^\s*module\s+(\w*)",re.IGNORECASE)
fuse=re.compile(r"^\s*use\s+(\w*)",re.IGNORECASE)
finclude=re.compile(r"^\s*include\s+[\'\"](\w*)[\'\"]",re.IGNORECASE)
fext=re.compile("\.[fF](?:9[05])?$")

src={}
for dr in os.walk(opts.srcdir):
	if opts.xdir.count(dr[0])>0:
		dr[1][:]=[]
		continue
	if len(objlist)==0:break
	for fil in dr[2]:
		srcf,ext=os.path.splitext(fil)
		if fext.search(ext) and objlist.count(srcf)>0:
			entry={'file':os.path.join(dr[0],fil),'modules':[],'uses':[],'depends':[]}
			objlist.remove(srcf)
			fh=open(entry['file'],'r')
			for line in fh.readlines():
				if not (fmod.search(line) or finclude.search(line) or fuse.search(line)): continue
				m=fmod.search(line)
				if m and m.group(1)!="procedure" :
					entry['modules'].append(m.group(1).lower())
					continue
				m=fuse.search(line)
				if m:
					entry['uses'].append(m.group(1).lower())
					continue
				m=finclude.search(line)
				if m:
					entry['include'].append(m.group(1).lower())
					continue
			fh.close()
			src[srcf]=entry

if len(objlist)!=0:
	print "Warning: The following objects were not found: "+" ".join(objlist)+"\n"

for srco in src.iterkeys():
	for modules in src[srco]['modules']:
		for srci in src.iterkeys():
			if srci==srco: continue
			if src[srci]['uses'].count(modules)>=1 and src[srci]['depends'].count(srco)==0:
				src[srci]['depends'].append(srco)

fh=open('makefile.dep','w')
line=""
llen=0
for srco in src.iterkeys():
	line+=srco+".o: "
	llen+=len(srco+".o: ")
	for depend in src[srco]['depends']:
		if llen > 65:
			line+="\\\n\t"
			llen=0
		line+=depend+".o "
		llen+=len(depend+".o ")
	line+="\n"
	llen=0

fh.write(line)
fh.close()
		
