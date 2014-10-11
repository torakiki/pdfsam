TO GENERATE AN UPDATED PDFsam.pot

xgettext -ki18n -L java -o po/pdfsam.pot --copyright-holder='Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com)' --msgid-bugs-address=info@pdfsam.org --no-location $(find ../ -name "*.java")



