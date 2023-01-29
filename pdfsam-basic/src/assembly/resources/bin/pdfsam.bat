@REM This file is part of the PDF Split And Merge Basic source code
@REM Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
@REM
@REM This program is free software: you can redistribute it and/or modify
@REM it under the terms of the GNU Affero General Public License as
@REM published by the Free Software Foundation, either version 3 of the
@REM License, or (at your option) any later version.
@REM
@REM This program is distributed in the hope that it will be useful,
@REM but WITHOUT ANY WARRANTY; without even the implied warranty of
@REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@REM GNU General Public License for more details.
@REM
@REM You should have received a copy of the GNU Affero General Public License
@REM along with this program.  If not, see <http://www.gnu.org/licenses/>.

@echo off

set ERROR_CODE=0

:init
@REM Decide how to startup depending on the version of windows

@REM -- Win98ME
if NOT "%OS%"=="Windows_NT" goto Win9xArg

@REM set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" @setlocal

@REM -- 4NT shell
if "%eval[2+2]" == "4" goto 4NTArgs

@REM -- Regular WinNT shell
set CMD_LINE_ARGS=%*
goto WinNTGetScriptDir

@REM The 4NT Shell from jp software
:4NTArgs
set CMD_LINE_ARGS=%$
goto WinNTGetScriptDir

:Win9xArg
@REM Slurp the command line arguments.  This loop allows for an unlimited number
@REM of arguments (up to the command line limit, anyway).
set CMD_LINE_ARGS=
:Win9xApp
if %1a==a goto Win9xGetScriptDir
set CMD_LINE_ARGS=%CMD_LINE_ARGS% %1
shift
goto Win9xApp

:Win9xGetScriptDir
set SAVEDIR=%CD%
%0\
cd %0\..\..
set BASEDIR=%CD%
cd %SAVEDIR%
set SAVE_DIR=
goto repoSetup

:WinNTGetScriptDir
for %%i in ("%~dp0..") do set "BASEDIR=%%~fi"

:repoSetup
set "RUNTIME=%BASEDIR%\pdfsam\runtime"
set "MODULEPATH=%BASEDIR%\pdfsam\app\mods"
set "PATH=%RUNTIME%;%BASEDIR%"

if exist "%PDFSAM_JAVA_PATH%" (
	set "JAVACMD=%PDFSAM_JAVA_PATH%\bin\java"
) else (
	if exist "%RUNTIME%" (
        set "JAVACMD=%RUNTIME%\bin\java"
    )
)

if "%JAVACMD%"=="" set JAVACMD=java

@REM Reaching here means variables are defined and arguments have been captured
:endInit

"%JAVACMD%" --module-path "%MODULEPATH%" --module org.pdfsam.basic/org.pdfsam.basic.App %JAVA_OPTS% -Xmx512M -splash:%BASEDIR%\splash.png -Dapp.name="pdfsam-basic" -Dprism.lcdtext=false -Dapp.home="%BASEDIR%" -Dbasedir="%BASEDIR%" %CMD_LINE_ARGS%
if %ERRORLEVEL% NEQ 0 goto error
goto end

:error
if "%OS%"=="Windows_NT" @endlocal
set ERROR_CODE=%ERRORLEVEL%

:end
@REM set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" goto endNT

@REM For old DOS remove the set variables from ENV - we assume they were not set
@REM before we started - at least we don't leave any baggage around
set CMD_LINE_ARGS=
goto postExec

:endNT
@REM If error code is set to 1 then the endlocal was done already in :error.
if %ERROR_CODE% EQU 0 @endlocal


:postExec

if "%FORCE_EXIT_ON_ERROR%" == "on" (
  if %ERROR_CODE% NEQ 0 exit %ERROR_CODE%
)

pause
exit /B %ERROR_CODE%
