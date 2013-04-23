preview: site
	./site preview

site: site.hs
	ghc --make site.hs
	./site clean

clean: site
	./site clean


build: site
	./site build


