ECHO OFF
PUSHD "${project.build.directory}/msix"

SET PDFSAM_VERSION=${parsedVersion.majorVersion}.${parsedVersion.minorVersion}.${parsedVersion.incrementalVersion}

REM Copy manifest and assets into the jpackage image
copy /Y AppxManifest.xml "${project.build.directory}\image\pdfsam\"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Manifest copied"

xcopy /E /I /Y Assets "${project.build.directory}\image\pdfsam\Assets"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "Assets copied"

echo -Dorg.pdfsam.disable.updates=true >> "${project.build.directory}\image\pdfsam\app\pdfsam.cfg"
if %ERRORLEVEL% NEQ 0 goto error

REM Build the MSIX
"MakeAppx.exe" pack /d "${project.build.directory}\image\pdfsam" /p "${project.build.directory}\pdfsam-basic-%PDFSAM_VERSION%-${os.detected.classifier}.msix"
if %ERRORLEVEL% NEQ 0 goto error
ECHO "MSIX created"

POPD

:error
set ERROR_CODE=%ERRORLEVEL%
exit /B %ERROR_CODE%
