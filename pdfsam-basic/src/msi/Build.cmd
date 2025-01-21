ECHO OFF
PUSHD "${project.build.directory}/msi"

SET PDFSAM_VERSION=${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}

REM Prevent compiling with outdated pdfsam.wixobj file if there is a error in candle.
del /Q pdfsam.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj
del /Q harvestedFiles.wxs
del /Q harvestedFiles.wixobj

REM harvest the files
"%WIX%bin\heat.exe" dir "${project.build.directory}/image/pdfsam" -ag -cg "AllFiles" -ke -sfrag -srd -sreg -dr APPLICATIONFOLDER -out harvestedFiles.wxs
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Files harvested"

REM Build the MSI
"%WIX%bin\candle.exe" pdfsam.wxs featuresTree.wxs verifyWithLanguageDlg.wxs exitDlg.wxs harvestedFiles.wxs -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -arch x64
if %ERRORLEVEL% NEQ 0 goto error
ECHO "candle run ok"

REM English
IF EXIST pdfsam.wixobj "%WIX%bin\light.exe" pdfsam.wixobj verifyWithLanguageDlg.wixobj featuresTree.wixobj exitDlg.wixobj harvestedFiles.wixobj -b "${project.build.directory}/image/pdfsam" -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -spdb -out "${project.build.directory}/pdfsam-basic-%PDFSAM_VERSION%-${os.detected.classifier}.msi" -loc "culture.wxl" -cultures:en-us
if %ERRORLEVEL% NEQ 0 goto error
ECHO "MSI created"

REM Cleanup
del /Q pdfsam.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj
del /Q harvestedFiles.wixobj

IF EXIST "${project.build.directory}/pdfsam-basic-%PDFSAM_VERSION%-${os.detected.classifier}.msi" "signtool.exe" sign /fd sha256 /tr http://timestamp.sectigo.com /td sha256 /sha1 "%SIGN_CERT_FINGERPRINT%" /d "PDFsam Basic" "${project.build.directory}/pdfsam-basic-%PDFSAM_VERSION%-${os.detected.classifier}.msi"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "MSI signed"
POPD

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%

