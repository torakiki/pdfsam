TO GENERATE AN UPDATED keys.pot

xgettext -ktrc -ktr -kmarktr -ktrn:1,2 -o po/keys.pot $(find ../ -name "*.java")


