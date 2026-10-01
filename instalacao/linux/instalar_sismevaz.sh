#!/usr/bin/env bash

# ============================================================

# INSTALADOR DO SIS-MEVAZ

# ============================================================

#

# Instala o pacote Sis-MEVAZ, suas dependências,

# a interface interativa e o WhiteboxTools.

#

# O instalador usa o Rscript encontrado no sistema e

# não exige uma versão específica do R.

#

# --vanilla é usado para impedir que configurações de

# projetos, .Rprofile ou renv interfiram na instalação.

# ============================================================

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_DIR" || {
echo "[ERRO] Não foi possível acessar a pasta do projeto:"
echo "       $PROJECT_DIR"
exit 1
}

echo
echo "============================================================"
echo "                 INSTALAÇÃO DO SIS-MEVAZ"
echo "============================================================"
echo

# ============================================================

# 1. LOCALIZAR Rscript

# ============================================================

if ! command -v Rscript >/dev/null 2>&1; then
echo "[ERRO] Rscript não foi encontrado no PATH."
echo
echo "Instale o R antes de executar o instalador."
exit 1
fi

RSCRIPT="$(command -v Rscript)"

echo "Rscript encontrado:"
echo "  $RSCRIPT"
echo

# ============================================================

# 2. VERIFICAR INSTALADOR R

# ============================================================

INSTALLER="$SCRIPT_DIR/../instalar_sismevaz.R"

if [ ! -f "$INSTALLER" ]; then
echo "[ERRO] O arquivo instalar_sismevaz.R não foi encontrado."
echo
echo "Caminho esperado:"
echo "  $INSTALLER"
echo
exit 1
fi

echo "Instalador R encontrado:"
echo "  $INSTALLER"
echo

# ============================================================

# 3. EXECUTAR INSTALADOR

# ============================================================

echo "============================================================"
echo "              INSTALANDO O SIS-MEVAZ"
echo "============================================================"
echo

# --vanilla evita que .Rprofile, renv ou configurações

# particulares do projeto alterem a biblioteca usada

# durante a instalação.

"$RSCRIPT" --vanilla "$INSTALLER"

STATUS=$?

# ============================================================

# 4. VERIFICAR RESULTADO

# ============================================================

if [ "$STATUS" -ne 0 ]; then
echo
echo "============================================================"
echo "[ERRO] A instalação do Sis-MEVAZ falhou."
echo "============================================================"
echo
echo "Código de saída: $STATUS"
echo "R utilizado: $RSCRIPT"
echo
exit "$STATUS"
fi

echo
echo "============================================================"
echo "           INSTALAÇÃO CONCLUÍDA COM SUCESSO"
echo "============================================================"
echo
echo "R utilizado:"
echo "  $RSCRIPT"
echo
echo "O Sis-MEVAZ foi instalado."
echo

exit 0
