# SisMEVAZ

Pacote R da metodologia MEVAZ, incluindo a interface interativa em Shiny.

## Requisito de versão

O Sis-MEVAZ requer **R 4.5.0 ou posterior**. Os instaladores recusam apenas versões anteriores. O sistema não usa `devtools` para instalação; versões novas devem ser validadas antes de entrarem na matriz de versões homologadas.

A instalação e o carregamento do pacote foram verificados no R 4.5.1. A imagem Docker conserva o R 4.5.3 como ambiente de referência.

## Instalação completa para usuários

A partir da raiz do repositório:

- Windows: dê duplo clique em `instalacao/windows/Instalar-SisMEVAZ.bat`.
- Linux/macOS: execute `./instalacao/linux/instalar_sismevaz.sh`.

No Windows, não é necessário usar o terminal. Uma única etapa instala o pacote, as dependências da interface, `hydrobr`,
`phylin` e o WhiteboxTools. Não é necessário abrir o RStudio.

## Interface interativa

Para o uso cotidiano:

- Windows: use o atalho criado ou dê duplo clique em `abrir_interface/windows/Abrir-SisMEVAZ.bat`.
- Linux/macOS: execute `./abrir_interface/linux/abrir_sismevaz.sh`.

Também é possível iniciar pelo R:

```r
SisMEVAZ::SisMEVAZ_interativo(diretorio_de_dados = "CAMINHO/QUE/CONTEM/DADOS")
```

O caminho informado deve apontar para o diretório que contém a pasta `dados`.

## Uso programático

```r
library(SisMEVAZ)

SisMEVAZ_redeTeste(diretorio_de_dados = "CAMINHO/para/dados")
SisMEVAZ_print_Sintese(
  diretorio_de_dados = "CAMINHO/para/dados",
  versao_print = "redeTeste"
)
```

## Desenvolvimento

A interface está em `inst/shiny/` para ser distribuída dentro do pacote. Após
alterar funções documentadas, regenere a documentação com `roxygen2::roxygenise()` (ou, opcionalmente, `devtools::document()`).
