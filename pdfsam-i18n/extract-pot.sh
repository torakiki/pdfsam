#!/bin/sh
xgettext -ktr -L Java -o po/pdfsam.pot --copyright-holder='Copyright 2026 by Sober Lemur S.r.l. (info@soberlemur.com)' --msgid-bugs-address=info@soberlemur.com --no-location $(find ../ -name "*.java" -not -path "*/.idea/*" -not -name "*Test.java") --from-code=UTF-8
