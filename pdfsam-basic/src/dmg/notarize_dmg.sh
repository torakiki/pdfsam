#!/bin/sh
# This file is part of the PDF Split And Merge Basic source code
# Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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

# Define the source and target file names
SOURCE_FILE="${project.build.directory}/PDFsam Basic-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}.dmg"
TARGET_FILE="${project.build.directory}/pdfsam-basic-${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}-${os.detected.classifier}.dmg"

# Rename the file with the appropriate suffix
mv "$SOURCE_FILE" "$TARGET_FILE" || exit 1
echo "dmg renamed"

xcrun notarytool submit --apple-id $APPLEID --password $APPLEIDPASS --team-id $TEAMID "$TARGET_FILE" || exit 1
echo "dmg notarized"

#stapling takes some time and it fails when run right after the notarization, we should probably wait few minutes
#xcrun stapler staple "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
#xcrun stapler validate "${project.build.directory}/pdfsam-${project.version}.dmg" || exit 1
#echo "dmg stapled"


