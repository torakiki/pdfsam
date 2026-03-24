ECHO OFF

SET IMAGE_DIR=${project.build.directory}\image\pdfsam

IF NOT EXIST "%IMAGE_DIR%\pdfsam.exe" GOTO error

REM ============================================================
REM  Sign all PE binaries (.exe and .dll) in the jpackage image
REM  using Azure Trusted Signing (built-in support in signtool)
REM ============================================================

REM Sign all .exe files
FOR /R "%IMAGE_DIR%" %%F IN (*.exe) DO (
    attrib -r "%%F"
    "signtool.exe" sign /v /fd sha256 /tr http://timestamp.acs.microsoft.com /td sha256 /dlib "%AZURE_CODESIGNING_DLIB%" /dmdf "%AZURE_SIGNING_METADATA%" /d "PDFsam Basic" "%%F"
    if %ERRORLEVEL% NEQ 0 goto error
    ECHO "Signed: %%F"
)

REM Sign all .dll files
FOR /R "%IMAGE_DIR%" %%F IN (*.dll) DO (
    attrib -r "%%F"
    "signtool.exe" sign /v /fd sha256 /tr http://timestamp.acs.microsoft.com /td sha256 /dlib "%AZURE_CODESIGNING_DLIB%" /dmdf "%AZURE_SIGNING_METADATA%" /d "PDFsam Basic" "%%F"
    if %ERRORLEVEL% NEQ 0 goto error
    ECHO "Signed: %%F"
)

ECHO "All binaries signed"

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%