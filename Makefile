SOURCES = `find src -name *.scala`
ZIPFILE = bcc.zip
ZIPCONTENT = $(SOURCES) cfg/Joos1W.lr1 joosc Makefile
BINDIR = bin

all:
	mkdir -p $(BINDIR)
	scalac -d $(BINDIR) $(SOURCES)
zip:
	zip $(ZIPFILE) $(ZIPCONTENT)
clean:
	rm -f $(ZIPFILE)
