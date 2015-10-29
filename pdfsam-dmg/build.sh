#!/bin/bash

set -e

echo 'Creating bundle'
ant create-bundle

echo 'Signing'

sudo codesign --deep -s "ANDREA VACONDIO" release/PDFsam.app

echo 'Making .dmg'
make
