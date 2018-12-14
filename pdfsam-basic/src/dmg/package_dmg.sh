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
rm -R "${project.build.directory}/assembled/runtime" || exit 1
echo "Deleted assembled runtime"

jpackager create-installer dmg --name "PDFsam Basic" --description "A free open source application to split, merge, extract pages and mix PDF files" \
--version ${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion} --vendor "Sober Lemur S.a.s. di Vacondio Andrea" \
--add-modules java.base,java.datatransfer,java.logging,java.naming,java.sql,java.desktop,java.xml,java.scripting,jdk.unsupported,java.prefs \
-o "${project.build.directory}" -i "${project.build.directory}/assembled" -c org.pdfsam.basic.App --license-file LICENSE.txt -j "${project.build.finalName}.jar" \
--icon "${project.build.directory}/dmg/Icon.icns" --identifier org.pdfsam.basic --user-jvm-args -Xmx=512m -a -Dorg.pdfsam.disable.ui.restore=true \
--category Business --mac-bundle-identifier org.pdfsam.basic --mac-bundle-name "PDFsam Basic" --verbose || exit 1
echo "dmg created"

mv ${project.build.directory}/PDFsam Basic-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}.dmg ${project.build.directory}/pdfsam-${project.version}.dmg || exit 1
echo "dmg renamed to pdfsam-${project.version}.dmg"

codesign --force --sign "Sober" "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
echo "dmg signed"