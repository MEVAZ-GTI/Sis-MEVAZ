#!/usr/bin/env bash

# ============================================================
# ABRE A INTERFACE DO SIS-MEVAZ
# ============================================================
#
# O script:
#   1. Localiza o Rscript disponível no sistema;
#   2. Não exige uma versão específica do R;
#   3. Verifica se o pacote SisMEVAZ está instalado;
#   4. Verifica a pasta de dados;
#   5. Libera a porta 8082, se necessário;
#   6. Inicia a interface Shiny.
#
# ============================================================


# ============================================================
# 1. Localiza os diretórios do projeto
# ============================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_DIR" || exit 1


# ============================================================
# 2. Localiza o Rscript
# ============================================================

if ! command -v Rscript >/dev/null 2>&1; then
    echo "[ERRO] O Rscript não foi encontrado no PATH."
    echo "Instale o R antes de abrir o Sis-MEVAZ."
    exit 1
fi

RSCRIPT="$(command -v Rscript)"

echo "Rscript detectado:"
echo "  $RSCRIPT"

R_VERSION="$("$RSCRIPT" --version 2>&1)"

echo "  $R_VERSION"
echo


# ============================================================
# 3. Verifica se o pacote SisMEVAZ está instalado
# ============================================================

if ! "$RSCRIPT" --vanilla -e \
    "q(status=if(requireNamespace('SisMEVAZ', quietly=TRUE)) 0 else 1)"
then
    echo "[ERRO] O pacote SisMEVAZ não está disponível para este R."
    echo
    echo "Execute primeiro:"
    echo "  ./instalacao/linux/instalar_sismevaz.sh"
    echo
    exit 1
fi

echo "[OK] Pacote SisMEVAZ encontrado."


# ============================================================
# 4. Verifica a pasta de dados
# ============================================================

BASE_DIR="$PROJECT_DIR"

if [ ! -d "$BASE_DIR/dados" ]; then
    echo "[ERRO] A pasta 'dados' não foi encontrada na raiz do projeto."
    echo
    echo "Diretório esperado:"
    echo "  $BASE_DIR/dados"
    echo
    exit 1
fi

echo "[OK] Pasta de dados encontrada."


# ============================================================
# 5. Libera a porta 8082, se estiver sendo utilizada
# ============================================================

if command -v lsof >/dev/null 2>&1; then

    OLD_PID="$(lsof -ti tcp:8082 2>/dev/null || true)"

    if [ -n "$OLD_PID" ]; then
        echo "[INFO] Processo anterior encontrado na porta 8082."
        echo "[INFO] Encerrando processo: $OLD_PID"

        kill "$OLD_PID" 2>/dev/null || true

        # Pequena espera para a porta ser liberada.
        sleep 1
    fi

fi


# ============================================================
# 6. Define o diretório de dados
# ============================================================

export SISMEVAZ_BASE_DIR="$BASE_DIR"


# ============================================================
# 7. Inicia a interface
# ============================================================

echo
echo "============================================================"
echo "                 SIS-MEVAZ"
echo "============================================================"
echo
echo "Abrindo o Sis-MEVAZ em:"
echo "  http://localhost:8082"
echo
echo "Para encerrar, feche esta janela ou interrompa o processo."
echo

exec "$RSCRIPT" --vanilla -e \
    "SisMEVAZ::SisMEVAZ_interativo(diretorio_de_dados=Sys.getenv('SISMEVAZ_BASE_DIR'))"
