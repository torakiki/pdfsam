ECHO OFF

IF NOT EXIST "${project.build.directory}\image\pdfsam\pdfsam.exe" GOTO error
attrib -r "${project.build.directory}\image\pdfsam\pdfsam.exe"
"signtool.exe" sign /fd sha256 /tr http://timestamp.sectigo.com /td sha256 /sha1 "%SIGN_CERT_FINGERPRINT%" /d "PDFsam Basic Launcher" "${project.build.directory}\image\pdfsam\pdfsam.exe"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Launcher signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%