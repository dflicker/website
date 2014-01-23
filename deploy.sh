#! /bin/sh

cp ~/Dropbox/CurrentResume/CurrentResume1.pdf ./static/resume.pdf
ghc dtf.hs && ./dtf rebuild
rsync --checksum -av ./_site/ dflicker@ericmart.in:/srv/www/dflicker_personal/
