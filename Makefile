SOURCES = `find src -name *.scala`
ZIPFILE = bcc.zip
ZIPCONTENT = $(SOURCES) cfg/Joos1W.lr1

all:
	scalac -d bin $(SOURCES)
zip:
	zip $(ZIPFILE) $(ZIPCONTENT)
clean:
	rm -f $(ZIPFILE)
