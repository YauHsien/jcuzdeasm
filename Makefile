#!IF EXIST("C:\Windows")
#REBAR = escript.exe "C:/Windows/System32/rebar"
#TMP = D:/tmp
#RM = RD /s/q
#MKDIR = mkdir
#CP = copy /y
#!ELSE
SHELL = /bin/bash
REBAR = rebar
TMP = /tmp
RM = rm -rf
MKDIR = mkdir -p
CP = cp -f
#!ENDIF

priv = priv
jarfile = $(jar)

all:
	echo ok

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

tjar:
	jar tf "$(priv)/$(jarfile)"

xjar:
	$(RM) "$(TMP)/$(jarfile)"
	$(MKDIR) "$(TMP)/$(jarfile)"
	$(CP) "$(priv)/$(jarfile)" "$(TMP)/$(jarfile)/"
	pushd "$(TMP)/$(jarfile)"; jar xf $(jarfile); popd

tjar-tmp:
	jar tf "$(priv)/$(jarfile)" | awk '$$NF ~ /[.]class$$/ { printf "/tmp/$(jarfile)/%s\n", $$NF }' | sed 's/[.]class//'

cls2asm: xjar
	jar tf "$(priv)/$(jarfile)" | awk '$$NF ~ /[.]class$$/ { print }' | sed 's/[.]class//' | sed 's/\$$/\\$$/g' | awk '{printf "mkdir -p src/main/java/$$(dirname %s)\njavap -c -verbose /tmp/$(jarfile)/%s.class > src/main/java/%s.javasm\ntouch src/main/java/%s.java\n", $$1, $$1, $$1, $$1 }' > /tmp/$(jarfile).sh
	sh /tmp/$(jarfile).sh

asm2code: cls2asm
	jar tf "$(priv)/$(jarfile)" | awk '$$NF ~ /[.]class$$/ { print }' | sed 's/[.]class//' | sed 's/\$$/\\$$/g' | awk 'BEGIN{printf "erl -pa ebin -eval \"jczdzm_app:start(normal, ["} NR>1{printf ", " } { printf "\\\"src/main/java/%s.javasm\\\"", $$1 } END{print "])\" -noshell"}' > /tmp/$(jarfile).sh
	sh /tmp/$(jarfile).sh
