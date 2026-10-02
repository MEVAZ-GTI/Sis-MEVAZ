#!/usr/bin/env bash

# Instala o Sis-MEVAZ completo.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_DIR" || exit 1

echo
echo "============================================================"
echo "             INSTALACAO DO SIS-MEVAZ"
echo "============================================================"
echo

# ------------------------------------------------------------
# Localizar Rscript
# ------------------------------------------------------------

if ! command -v Rscript >/dev/null 2>&1; then
    echo "[ERRO] Rscript nao foi encontrado no PATH."
    echo
    echo "Instale o R antes de continuar."
    exit 1
fi

RSCRIPT="$(command -v Rscript)"

echo "Rscript detectado:"
echo "  $RSCRIPT"
echo

# ------------------------------------------------------------
# Localizar instalador R
# ------------------------------------------------------------

INSTALLER="$SCRIPT_DIR/../instalar_sismevaz.R"

if [ ! -f "$INSTALLER" ]; then
    echo "[ERRO] O arquivo instalar_sismevaz.R nao foi encontrado:"
    echo "       $INSTALLER"
    exit 1
fi

# ------------------------------------------------------------
# Executar instalador
# ------------------------------------------------------------

echo "Iniciando instalacao..."
echo

"$RSCRIPT" --vanilla "$INSTALLER"

STATUS=$?

if [ "$STATUS" -ne 0 ]; then
    echo
    echo "============================================================"
    echo "[ERRO] A instalacao do Sis-MEVAZ falhou."
    echo "Codigo de saida: $STATUS"
    echo "============================================================"
    exit "$STATUS"
fi

echo
echo "============================================================"
echo "      Instalacao do Sis-MEVAZ concluida com sucesso!"
echo "============================================================"
echo

exit 0
