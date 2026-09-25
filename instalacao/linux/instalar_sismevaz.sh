#!/usr/bin/env bash
# Instala o Sis-MEVAZ completo (pacote R e interface interativa).
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

echo "R compativel detectado: $(Rscript -e 'cat(as.character(getRversion()))')"
echo "Instalando o Sis-MEVAZ e todos os componentes..."
Rscript "$SCRIPT_DIR/../instalar_sismevaz.R"
