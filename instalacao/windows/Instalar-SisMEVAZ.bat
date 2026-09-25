@echo off
setlocal
title Instalacao do Sis-MEVAZ
cd /d "%~dp0..\.."

set "RSCRIPT_EXE="
where Rscript >nul 2>&1
if %errorlevel% equ 0 set "RSCRIPT_EXE=Rscript"
if defined RSCRIPT_EXE goto rscript_found
for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT_EXE=%%B\bin\Rscript.exe"
if defined RSCRIPT_EXE if exist "%RSCRIPT_EXE%" goto rscript_found
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RSCRIPT_EXE=%%D\bin\Rscript.exe"
if not defined RSCRIPT_EXE goto r_missing

:rscript_found
"%RSCRIPT_EXE%" -e "q(status=if(getRversion() >= '4.5.0') 0 else 1)"
if %errorlevel% neq 0 (
    echo [ERRO] O Sis-MEVAZ requer R 4.5.0 ou posterior.
    pause
    exit /b 1
)

echo R compativel detectado.
echo Instalando o Sis-MEVAZ e todos os componentes...
"%RSCRIPT_EXE%" "%~dp0..\instalar_sismevaz.R"
if %errorlevel% neq 0 (
    echo [ERRO] A instalacao nao foi concluida.
    pause
    exit /b 1
)
echo.
powershell -NoProfile -ExecutionPolicy Bypass -Command "$s=(New-Object -COM WScript.Shell).CreateShortcut([IO.Path]::Combine([Environment]::GetFolderPath('Desktop'),'Sis-MEVAZ.lnk'));$s.TargetPath=[IO.Path]::GetFullPath('%~dp0..\..\abrir_interface\windows\Abrir-SisMEVAZ.bat');$s.WorkingDirectory=[IO.Path]::GetFullPath('%~dp0..\..');$s.Save()" >nul 2>&1
echo Instalacao concluida. Use o atalho Sis-MEVAZ ou abrir_interface\windows\Abrir-SisMEVAZ.bat.
pause
exit /b 0

:r_missing
start "" "https://cran.r-project.org/bin/windows/base/" >nul 2>&1
echo [ERRO] O R 4.5.0 ou posterior nao foi encontrado.
echo Instale uma versao atual do R antes de continuar.
pause
exit /b 1
