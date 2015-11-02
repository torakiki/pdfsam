@ECHO OFF

SET PDFSAM_VERSION=3.0.0.RELEASE

REM Prevent compiling with outdated pdfsam.wixobj file if there is a error in candle.
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj

REM Build the MSI
"%WIX%bin\candle.exe" pdfsam.wxs requirementsDlg.wxs featuresTree.wxs verifyWithLanguageDlg.wxs exitDlg.wxs -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -ext WixNetFxExtension

REM English
IF EXIST pdfsam.wixobj "%WIX%bin\light.exe" pdfsam.wixobj requirementsDlg.wixobj verifyWithLanguageDlg.wixobj featuresTree.wixobj exitDlg.wixobj -ext WixUIExtension -ext WixUtilExtension -ext WixNetFxExtension -ext WixNetFxExtension -spdb -out "output\pdfsam-v%PDFSAM_VERSION%.msi" -loc "culture.wxl" -cultures:en-us

REM Cleanup
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj
SET Culture=
SET MsiName=
SET WinSDK=
