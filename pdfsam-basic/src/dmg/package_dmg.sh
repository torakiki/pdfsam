#!/bin/sh
# This file is part of the PDF Split And Merge Basic source code
# Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
mv "${project.build.directory}/assembled/runtime/" "${project.build.directory}/runtime/" || exit 1
echo "Moved assembled runtime"
rm "${project.build.directory}/runtime/bin/keytool" || exit 1
echo "Removed unused (I think) keytool"
rm "${project.build.directory}/runtime/bin/jrunscript" || exit 1
echo "Removed unused (I think) jrunscript"
rm -R "${project.build.directory}/assembled/bin"
echo "Removed bin scripts"

$JAVA_HOME_14/bin/jpackage --runtime-image "${project.build.directory}/runtime/"  --type app-image \
--input "${project.build.directory}/assembled/" --main-class org.pdfsam.basic.App --icon "${project.build.directory}/dmg/Icon.icns" --name "PDFsam Basic" \
--description "A free open source application to split, merge, extract pages and mix PDF files" --main-jar "${project.build.finalName}.jar" \
--app-version "${project.version}" --vendor "Sober Lemur S.a.s. di Vacondio Andrea" \
--dest "${project.build.directory}" --copyright "2020 Sober Lemur S.a.s. di Vacondio Andrea" \
--mac-package-identifier org.pdfsam.basic  --resource-dir "${project.build.directory}/dmg/resources" --verbose || exit 1
echo "App image created"

codesign --force --deep --timestamp --entitlements "${project.build.directory}/dmg/entitlements.mac.plist" \
--options runtime -vvv --sign "Developer ID Application: Andrea Vacondio" "${project.build.directory}/PDFsam Basic.app/Contents/runtime/Contents/Home/lib/jspawnhelper" || exit 1
codesign --verify -vvv "${project.build.directory}/PDFsam Basic.app/Contents/runtime/Contents/Home/lib/jspawnhelper" || exit 1
echo "Signed and verified jspawnhelper"

codesign --force --deep --timestamp --entitlements "${project.build.directory}/dmg/entitlements.mac.plist" \
--options runtime -vvv --sign "Developer ID Application: Andrea Vacondio" "${project.build.directory}/PDFsam Basic.app/Contents/runtime/Contents/Home/bin/java" || exit 1
codesign --verify -vvv "${project.build.directory}/PDFsam Basic.app/Contents/runtime/Contents/Home/bin/java" || exit 1
echo "Signed and verified java"

find "${project.build.directory}/PDFsam Basic.app/Contents/" -type f \( -name "*.jar" -or -name "*.dylib" -or -name "*.jnilib" \) \
-exec codesign --force --timestamp --entitlements "${project.build.directory}/dmg/entitlements.mac.plist" --options \
runtime -vvv --sign "Developer ID Application: Andrea Vacondio" {} \; || exit 1

find "${project.build.directory}/PDFsam Basic.app/Contents/" -type f \( -name "*.jar" -or -name "*.dylib" -or -name "*.jnilib" \) \
-exec codesign --verify -vvv {} \; || exit 1
echo "Signed and verified jars and dylibs"

codesign --force --deep --timestamp --entitlements "${project.build.directory}/dmg/entitlements.mac.plist" \
--options runtime -vvv --sign "Developer ID Application: Andrea Vacondio" "${project.build.directory}/PDFsam Basic.app" || exit 1
codesign --verify -vvv "${project.build.directory}/PDFsam Basic.app" || exit 1
echo "Signed and verified App"

$JAVA_HOME_14/bin/jpackage --name "pdfsam" --mac-package-identifier org.pdfsam.basic  --mac-sign --mac-signing-key-user-name "Andrea Vacondio" \
--app-version "${project.version}" --mac-package-name "PDFsam Basic" \
--app-image "${project.build.directory}/PDFsam Basic.app" --license-file "${project.build.directory}/assembled/LICENSE.txt" --dest "${project.build.directory}" \
--resource-dir "${project.build.directory}/dmg/resources" --name "PDFsam Basic" --verbose || exit 1
echo "dmg created"

mv "${project.build.directory}/PDFsam Basic-${project.version}.dmg" "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
echo "dmg renamed"

codesign --force --deep --timestamp --entitlements "${project.build.directory}/dmg/entitlements.mac.plist" --options runtime \
-vvv --sign "Developer ID Application: Andrea Vacondio" "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
echo "dmg signed"

xcrun altool --notarize-app --primary-bundle-id org.pdfsam.basic --username $APPLEID --password $APPLEIDPASS --file "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
echo "dmg notarized"

#stapling takes some time and it fails when run right after the notarization, we should probably wait few minutes
#xcrun stapler staple "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
#xcrun stapler validate "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
echo "dmg stapled"


