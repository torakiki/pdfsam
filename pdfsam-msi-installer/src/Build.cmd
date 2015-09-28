@ECHO OFF

SET MsiName=pdfsam-v3.0.0.M2

REM Prevent compiling with outdated pdfsam.wixobj file if there is a error in candle.
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj

REM Build the MSI
"%WIX%bin\candle.exe" pdfsam.wxs requirementsDlg.wxs featuresTree.wxs verifyWithLanguageDlg.wxs exitDlg.wxs -ext WixUIExtension -ext WixUtilExtension

REM English
IF EXIST pdfsam.wixobj "%WIX%bin\light.exe" pdfsam.wixobj requirementsDlg.wixobj verifyWithLanguageDlg.wixobj featuresTree.wixobj exitDlg.wixobj -ext WixUIExtension -ext WixUtilExtension -spdb -out "output\%MsiName%.msi" -loc "culture.wxl" -cultures:en-us

REM Cleanup
del /Q pdfsam.wixobj
del /Q requirementsDlg.wixobj
del /Q featuresTree.wixobj
del /Q verifyWithLanguageDlg.wixobj
del /Q exitDlg.wixobj
SET Culture=
SET MsiName=
SET WinSDK=
