ECHO OFF

IF NOT EXIST "AddLine.CA.dll" GOTO error
attrib -r "AddLine.CA.dll"
"signtool.exe" sign /v /debug /fd sha256 /td sha256 /tr http://timestamp.sectigo.com /sha1 "%SIGN_CERT_FINGERPRINT%" "AddLine.CA.dll"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Dll signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%