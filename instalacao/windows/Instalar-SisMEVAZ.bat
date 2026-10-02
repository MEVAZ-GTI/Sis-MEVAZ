@echo off
setlocal

echo.
echo ============================================================
echo              INSTALACAO DO SIS-MEVAZ
echo ============================================================
echo.

REM ------------------------------------------------------------
REM 1. Tentar localizar Rscript no PATH
REM ------------------------------------------------------------

where Rscript >nul 2>&1

if %ERRORLEVEL% EQU 0 (
    set "RSCRIPT_EXE=Rscript"
    goto R_FOUND
)

REM ------------------------------------------------------------
REM 2. Procurar instalacoes do R no perfil do usuario
REM ------------------------------------------------------------

for /f "delims=" %%R in ('dir /b /ad "%LOCALAPPDATA%\Programs\R\R-*" 2^>nul') do (
    if exist "%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe" (
        set "RSCRIPT_EXE=%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe"
        goto R_FOUND
    )
)

REM ------------------------------------------------------------
REM 3. Procurar instalacoes do R em Program Files
REM ------------------------------------------------------------

for /f "delims=" %%R in ('dir /b /ad "%ProgramFiles%\R\R-*" 2^>nul') do (
    if exist "%ProgramFiles%\R\%%R\bin\Rscript.exe" (
        set "RSCRIPT_EXE=%ProgramFiles%\R\%%R\bin\Rscript.exe"
        goto R_FOUND
    )
)

echo [ERRO] Rscript nao foi encontrado.
echo.
echo Instale o R antes de continuar.
echo.
pause
exit /b 1


:R_FOUND

echo Rscript detectado:
echo %RSCRIPT_EXE%
echo.

REM ------------------------------------------------------------
REM Verificar instalador
REM ------------------------------------------------------------

if not exist "%~dp0..\instalar_sismevaz.R" (
    echo [ERRO] O arquivo instalar_sismevaz.R nao foi encontrado.
    echo.
    echo Local esperado:
    echo %~dp0..\instalar_sismevaz.R
    echo.
    pause
    exit /b 1
)

REM ------------------------------------------------------------
REM Executar instalador
REM ------------------------------------------------------------

echo Iniciando instalacao...
echo.

"%RSCRIPT_EXE%" --vanilla "%~dp0..\instalar_sismevaz.R"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ============================================================
    echo [ERRO] A instalacao do Sis-MEVAZ falhou.
    echo ============================================================
    echo.
    pause
    exit /b %ERRORLEVEL%
)

echo.
echo ============================================================
echo       Instalacao do Sis-MEVAZ concluida com sucesso!
echo ============================================================
echo.
echo O pacote foi instalado na biblioteca do usuario.
echo.

pause
exit /b 0
