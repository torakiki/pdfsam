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
mv "${project.build.directory}/PDFsam Basic-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}.dmg" "${project.build.directory}/pdfsam-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}.dmg" || exit 1
echo "dmg renamed"

xcrun altool --notarize-app --primary-bundle-id org.pdfsam.basic --username $APPLEID --password $APPLEIDPASS --file "${project.build.directory}/pdfsam-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}.dmg" || exit 1
echo "dmg notarized"

#stapling takes some time and it fails when run right after the notarization, we should probably wait few minutes
#xcrun stapler staple "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
#xcrun stapler validate "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
#echo "dmg stapled"


