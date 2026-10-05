@echo off
setlocal
title Sis-MEVAZ

cd /d "%~dp0..\.."

set "RSCRIPT_EXE="

where Rscript >nul 2>&1
if %errorlevel% equ 0 set "RSCRIPT_EXE=Rscript"
if defined RSCRIPT_EXE goto rscript_found

for /f "tokens=2,*" %%A in ('reg query "HKLM\SOFTWARE\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do set "RSCRIPT_EXE=%%B\bin\Rscript.exe"
if defined RSCRIPT_EXE if exist "%RSCRIPT_EXE%" goto rscript_found

for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" set "RSCRIPT_EXE=%%D\bin\Rscript.exe"
)

if not defined RSCRIPT_EXE goto r_missing

:rscript_found

echo Rscript detectado:
"%RSCRIPT_EXE%" --version
echo.

"%RSCRIPT_EXE%" -e "q(status=if(requireNamespace('SisMEVAZ', quietly=TRUE)) 0 else 1)"

if %errorlevel% neq 0 (
    echo [ERRO] O pacote SisMEVAZ ainda nao esta instalado.
    echo Execute primeiro instalacao\windows\Instalar-SisMEVAZ.bat.
    pause
    exit /b 1
)

set "SISMEVAZ_BASE_DIR=%~dp0..\..\"

if exist "%SISMEVAZ_BASE_DIR%dados\" goto dados_found

goto dados_missing


:dados_found

for /f "tokens=5" %%P in ('netstat -ano ^| findstr :8082 ^| findstr LISTENING') do (
    taskkill /F /PID %%P >nul 2>&1
)


echo ============================================================
echo                     SIS-MEVAZ
echo ============================================================
echo
echo Iniciando a interface do Sis-MEVAZ...
echo.
echo Aguarde alguns segundos.
echo O navegador sera aberto automaticamente.
echo.
echo Para encerrar o Sis-MEVAZ, feche esta janela.
echo
echo ============================================================
echo

"%RSCRIPT_EXE%" --vanilla -e "SisMEVAZ::SisMEVAZ_interativo(diretorio_de_dados=Sys.getenv('SISMEVAZ_BASE_DIR'), navegador=TRUE, porta=8082, endereco='0.0.0.0')"

echo.
echo O servidor foi encerrado.
pause

exit /b 0

echo.
echo O servidor foi encerrado.
pause

exit /b 0


:dados_missing

echo [ERRO] A pasta 'dados' nao foi encontrada.
echo Coloque a pasta dados na raiz do projeto.
pause

exit /b 1


:r_missing

echo [ERRO] O R nao foi encontrado.
echo Instale o R antes de abrir o Sis-MEVAZ.
pause

exit /b 1
