#! /bin/sh

ghc dtf.hs && ./dtf rebuild
rsync -av ./_site/ dflicker@ericmart.in:/srv/www/dflicker_personal/
