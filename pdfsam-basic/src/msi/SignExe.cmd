ECHO OFF

IF NOT EXIST "${project.build.directory}\image\pdfsam\pdfsam.exe" GOTO error
attrib -r "${project.build.directory}\image\pdfsam\pdfsam.exe"
"signtool.exe" sign /fd sha256 /tr http://sha256timestamp.ws.symantec.com/sha256/timestamp /sha1 "%SIGN_CERT_FINGERPRINT%" /d "PDFsam Basic Launcher" "${project.build.directory}\image\pdfsam\pdfsam.exe"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Launcher signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%