TO GENERATE AN UPDATED PDFsam.pot

xgettext -ki18n -L java -o po/PDFsam.pot --copyright-holder='Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com)' --package-name=PDFsam --msgid-bugs-address=info@pdfsam.org --no-location $(find ../ -name "*.java")



