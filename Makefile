SHELL = /bin/bash

jarfile = $(jar)

all:
	echo ok

build:
	rebar compile

tjar:
	jar tf $(jarfile)

xjar:
	rm -rf /tmp/$(jarfile)
	mkdir -p /tmp/$(jarfile)
	cp -f $(jarfile) /tmp/$(jarfile)/
	pushd /tmp/$(jarfile); jar xf *.jar; popd

tjar-tmp:
	jar tf $(jarfile) | awk '$$NF ~ /[.]class$$/ { printf "/tmp/$(jarfile)/%s\n", $$NF }' | sed 's/[.]class//'

cls2asm: xjar
	jar tf $(jarfile) | awk '$$NF ~ /[.]class$$/ { print }' | sed 's/[.]class//' | sed 's/\$$/\\$$/g' | awk '{printf "mkdir -p src/main/java/$$(dirname %s)\njavap -c -verbose /tmp/$(jarfile)/%s.class > src/main/java/%s.javasm\ntouch src/main/java/%s.java\n", $$1, $$1, $$1, $$1 }' > /tmp/$(jarfile).sh
	sh /tmp/$(jarfile).sh

asm2code: cls2asm
	jar tf $(jarfile) | awk '$$NF ~ /[.]class$$/ { print }' | sed 's/[.]class//' | sed 's/\$$/\\$$/g' | awk -v P="$$(pwd)" 'BEGIN{printf "erl -pa ebin -eval \"jczdzm_app:start(normal, ["} NR>1{printf ", " } { printf "\\\"%s/src/main/java/%s.javasm\\\"", P, $$1 } END{print "])\" -noshell"}' > /tmp/$(jarfile).sh
	sh /tmp/$(jarfile).sh
