SOURCES=$(wildcard *.R)
OBJECTS=$(SOURCES:.R=.out)
all: $(OBJECTS)
%.out: %.R
	Rscript $< > $@
publish: force
	cp *.csv ../inst/extdata/dictionaries
clean: force
	-rm *.out *~
force:
