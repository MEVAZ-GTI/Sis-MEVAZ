@echo off
setlocal EnableExtensions

title Instalacao do Sis-MEVAZ

REM ============================================================
REM CONFIGURACAO
REM ============================================================

set "RSCRIPT_EXE="

cd /d "%~dp0...."

echo.
echo ============================================================
echo                   Instalacao do Sis-MEVAZ
echo ============================================================
echo.

REM ============================================================
REM 1. VALIDAR ESTRUTURA DO PACOTE
REM ============================================================

if not exist "%~dp0..\instalar_sismevaz.R" (
echo [ERRO] O arquivo instalar_sismevaz.R nao foi encontrado.
echo.
echo Caminho esperado:
echo %~dp0..\instalar_sismevaz.R
echo.
pause
exit /b 1
)

echo Estrutura do pacote: OK
echo.

REM ============================================================
REM 2. LOCALIZAR Rscript
REM ============================================================

echo Procurando o R instalado...
echo.

REM ------------------------------------------------------------
REM Primeiro: Rscript disponivel no PATH
REM ------------------------------------------------------------

for /f "delims=" %%R in ('where Rscript 2^>nul') do (
if not defined RSCRIPT_EXE set "RSCRIPT_EXE=%%R"
)

REM ------------------------------------------------------------
REM Segundo: instalacao padrao do R para o usuario
REM ------------------------------------------------------------

if not defined RSCRIPT_EXE if exist "%LOCALAPPDATA%\Programs\R" (
for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do (
if exist "%%D\bin\x64\Rscript.exe" (
set "RSCRIPT_EXE=%%D\bin\x64\Rscript.exe"
)
)
)

REM ------------------------------------------------------------
REM Terceiro: Program Files
REM ------------------------------------------------------------

if not defined RSCRIPT_EXE if exist "%ProgramFiles%\R" (
for /d %%D in ("%ProgramFiles%\R\R-*") do (
if exist "%%D\bin\x64\Rscript.exe" (
set "RSCRIPT_EXE=%%D\bin\x64\Rscript.exe"
)
)
)

REM ------------------------------------------------------------
REM Verificar se encontramos R
REM ------------------------------------------------------------

if not defined RSCRIPT_EXE goto R_MISSING

echo R encontrado:
echo %RSCRIPT_EXE%
echo.

REM ============================================================
REM 3. INSTALAR
REM ============================================================

echo ============================================================
echo              Instalando o Sis-MEVAZ
echo ============================================================
echo.

"%RSCRIPT_EXE%" --vanilla "%~dp0..\instalar_sismevaz.R"

if errorlevel 1 (
echo.
echo ============================================================
echo [ERRO] A instalacao nao foi concluida.
echo ============================================================
echo.
echo R utilizado:
echo %RSCRIPT_EXE%
echo.
pause
exit /b 1
)

REM ============================================================
REM 4. CONCLUSAO
REM ============================================================

echo.
echo ============================================================
echo              INSTALACAO CONCLUIDA
echo ============================================================
echo.
echo R utilizado:
echo %RSCRIPT_EXE%
echo.
echo O Sis-MEVAZ foi instalado.
echo.
echo Para executar a interface, utilize:
echo abrir_interface\windows\Abrir-SisMEVAZ.bat
echo.

pause
exit /b 0

REM ============================================================
REM R NAO ENCONTRADO
REM ============================================================

:R_MISSING

echo.
echo ============================================================
echo [ERRO] O R nao foi encontrado neste computador.
echo ============================================================
echo.
echo O R deve estar instalado antes de executar este instalador.
echo.
echo Locais pesquisados:
echo.
echo   - Rscript disponivel no PATH
echo   - %LOCALAPPDATA%\Programs\R
echo   - %ProgramFiles%\R
echo.
echo Verifique a instalacao do R e tente novamente.
echo.

pause
exit /b 1
