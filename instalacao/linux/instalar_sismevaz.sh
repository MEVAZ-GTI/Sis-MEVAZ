```bash
#!/usr/bin/env bash

# Instala o Sis-MEVAZ completo (pacote R e interface interativa).

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_DIR" || exit 1

# Localiza o Rscript instalado no sistema.
if ! command -v Rscript >/dev/null 2>&1; then
    echo "[ERRO] Rscript não foi encontrado no PATH."
    echo "Instale o R antes de continuar."
    exit 1
fi

RSCRIPT="$(command -v Rscript)"

# Verifica se o instalador R existe.
INSTALLER="$SCRIPT_DIR/../instalar_sismevaz.R"

if [ ! -f "$INSTALLER" ]; then
    echo "[ERRO] O arquivo instalar_sismevaz.R não foi encontrado:"
    echo "       $INSTALLER"
    exit 1
fi

echo "Rscript detectado: $RSCRIPT"
echo "Instalando o Sis-MEVAZ e todos os componentes..."
echo

"$RSCRIPT" "$INSTALLER"

STATUS=$?

if [ $STATUS -ne 0 ]; then
    echo
    echo "[ERRO] A instalação do Sis-MEVAZ falhou."
    echo "Código de saída: $STATUS"
    exit $STATUS
fi

echo
echo "================================================="
echo "  Instalação do Sis-MEVAZ concluída com sucesso!"
echo "================================================="

exit 0
```
