#!/usr/bin/env bash
# Inicia a interface interativa instalada com o pacote SisMEVAZ.
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_DIR"

if ! command -v Rscript >/dev/null 2>&1; then
    echo "[ERRO] R 4.5.0 ou posterior nao esta instalado ou nao esta no PATH."
    exit 1
fi
if ! Rscript -e 'q(status=if(getRversion() >= "4.5.0") 0 else 1)'; then
    echo "[ERRO] O Sis-MEVAZ requer R 4.5.0 ou posterior."
    exit 1
fi
if ! Rscript -e "q(status=if(requireNamespace('SisMEVAZ', quietly=TRUE)) 0 else 1)"; then
    echo "[ERRO] O Sis-MEVAZ ainda nao esta instalado."
    echo "Execute primeiro: ./instalacao/linux/instalar_sismevaz.sh"
    exit 1
fi

BASE_DIR="$PROJECT_DIR"
if [ ! -d "$BASE_DIR/dados" ]; then
    echo "[ERRO] A pasta 'dados' nao foi encontrada na raiz do projeto."
    exit 1
fi

if command -v lsof >/dev/null 2>&1; then
    OLD_PID="$(lsof -ti tcp:8082 2>/dev/null || true)"
    [ -n "$OLD_PID" ] && kill "$OLD_PID" 2>/dev/null || true
fi

export SISMEVAZ_BASE_DIR="$BASE_DIR"
echo "Abrindo o Sis-MEVAZ em http://localhost:8082"
exec Rscript -e "SisMEVAZ::SisMEVAZ_interativo(diretorio_de_dados=Sys.getenv('SISMEVAZ_BASE_DIR'))"
