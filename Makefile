SOURCES=$(shell find . -name *.Rmd)
SOURCES := $(wildcard *.Rmd)
TARGETS=$(SOURCES:%.Rmd=%.pdf)

%.pdf: %.Rmd
	@echo "$< -> $@"
	@Rscript -e "rmarkdown::render('$<')"
	@pdf2ps $@ temp.ps
	@ps2pdf temp.ps SmartMetres2018.pdf
	@rm temp.ps

default: $(TARGETS)

deploy:
	rsync -zrvce 'ssh -p 18765' $(TARGETS) robjhynd@m80.siteground.biz:public_html/talks

clean:
	rm -rf $(TARGETS)