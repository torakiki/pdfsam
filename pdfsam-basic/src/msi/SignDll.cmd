ECHO OFF

IF NOT EXIST "AddLine.CA.dll" GOTO error
attrib -r "AddLine.CA.dll"
"signtool.exe" sign /v /debug /fd sha256 /td sha256 /tr http://sha256timestamp.ws.symantec.com/sha256/timestamp /sha1 "%SIGN_CERT_FINGERPRINT%" "AddLine.CA.dll"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Dll signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%