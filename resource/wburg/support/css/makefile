includes = $(wildcard _*.scss)

css: deck.css page.css handout.css

watch: css
	ls *.scss | entr make css

deck.css: $(includes)
page.css: $(includes)
handout.css: $(includes)

%.css : %.scss
	sassc --style expanded $< $@
