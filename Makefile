SOURCES = `find src -name *.scala`
TESTSOURCES = `find test -name *.scala`
ZIPFILE = bcc.zip
ZIPCONTENT = $(SOURCES) $(TESTSOURCES) cfg/grammar.lr1 joosc Makefile
BINDIR = bin

all:
	mkdir -p $(BINDIR)
	scalac -d $(BINDIR) $(SOURCES)
zip:
	rm -f $(ZIPFILE)
	zip $(ZIPFILE) $(ZIPCONTENT)
clean:
	rm -f $(ZIPFILE)
