TO GENERATE AN UPDATED pdfsam.pot

xgettext -ki18n -L Java -o po/pdfsam.pot --copyright-holder='Copyright 2022 by Sober Lemur S.a.s. (andrea.vacondio@gmail.com)' --msgid-bugs-address=info@pdfsam.org --no-location $(find ../ -name "*.java") --from-code=UTF-8


To rename when downloaded from Launchpad
rename 's/^PDFsam-//' *


