ECHO OFF

IF NOT EXIST "AddLine.CA.dll" GOTO error
attrib -r "AddLine.CA.dll"
"signtool.exe" sign /v /fd sha256 /tr http://timestamp.acs.microsoft.com /td sha256 /dlib "%AZURE_CODESIGNING_DLIB%" /dmdf "%AZURE_SIGNING_METADATA%" "AddLine.CA.dll"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Dll signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%