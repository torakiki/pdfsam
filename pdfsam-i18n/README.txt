TO GENERATE AN UPDATED pdfsam.pot

xgettext -ki18n -L Java -o po/pdfsam.pot --copyright-holder='Copyright 2024 by Sober Lemur Srl (info@soberlemur.com)' --msgid-bugs-address=info@pdfsam.org --no-location $(find ../ -name "*.java") --from-code=UTF-8


To rename when downloaded from Launchpad
rename 's/^PDFsam-//' *


