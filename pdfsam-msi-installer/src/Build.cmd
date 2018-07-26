@ECHO OFF

SET PDFSAM_VERSION=3.3.5

REM Prevent compiling with outdated pdfsam.wixobj file if there is a error in candle.
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj

REM Build the MSI
"%WIX%bin\candle.exe" pdfsam.wxs requirementsDlg.wxs featuresTree.wxs verifyWithLanguageDlg.wxs exitDlg.wxs -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension

REM English
IF EXIST pdfsam.wixobj "%WIX%bin\light.exe" pdfsam.wixobj requirementsDlg.wixobj verifyWithLanguageDlg.wixobj featuresTree.wixobj exitDlg.wixobj -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -spdb -out "output\pdfsam-%PDFSAM_VERSION%.msi" -loc "culture.wxl" -cultures:en-us

REM Cleanup
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj
SET Culture=
SET MsiName=
SET WinSDK=

IF EXIST "output\pdfsam-%PDFSAM_VERSION%.msi" "signtool.exe" sign /t http://timestamp.verisign.com/scripts/timstamp.dll /a /d "PDFsam Basic" "output\pdfsam-%PDFSAM_VERSION%.msi"
