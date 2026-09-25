A Metodologia para o Cálculo de Vazões (MEVAZ) é a metodologia oficial para a estimativa das séries de vazões afluentes aos reservatórios no Ceará e de sua oferta hídrica de longo prazo.

Este repositório contém o pacote R `SisMEVAZ` e sua interface web em R/Shiny. O código fica concentrado em `SisMEVAZ/`, enquanto os dados operacionais permanecem na pasta `dados/`, fora do pacote.

## Pré-requisitos

- Windows, Linux ou macOS
- [R 4.5.3](https://cran.r-project.org/), versão homologada para o Sis-MEVAZ. Versões posteriores não são suportadas
- A pasta de dados `dados` https://drive.google.com/file/d/1mE1IyT91-oy2ermkb-Q7DFGlyzrajMfh/view?usp=sharing (~5 GB). **Esta pasta não está incluída neste repositório Git** por causa do seu tamanho. 

## Estrutura do repositório

| Pasta | Conteúdo |
| --- | --- |
| `SisMEVAZ/R/` | Funções do pacote R e back-end de cálculo |
| `SisMEVAZ/inst/shiny/` | Interface web em R/Shiny |
| `dados/` | Bases fixas, entradas, saídas e versões históricas |

Os arquivos geoespaciais intermediários são criados no diretório
temporário do sistema durante a execução e removidos quando o aplicativo
é encerrado. As entradas e os resultados da simulação continuam sendo
gravados em `dados/RededeReservatorios_em_teste/`.

## Instalação

Clone este repositório:

```
git clone https://github.com/maetzoo/Sis-MEVAZ-dash-no-shiny.git
```

A instalação inclui, em uma única etapa, o pacote R, a interface interativa, suas dependências e o WhiteboxTools. Não é necessário abrir o RStudio ou instalar separadamente os pacotes do Shiny.

**Windows**

Dê duplo clique em **`instalacao/instalar_sismevaz.bat`**.

**Linux / macOS**

```
chmod +x instalacao/instalar_sismevaz.sh iniciar_sismevaz.sh
./instalacao/instalar_sismevaz.sh
```

## Configuração dos dados

No modo interativo, a pasta `dados` deve permanecer na raiz do projeto, ao lado dos iniciadores. Ela deve conter `Dados_fixos`, `Plu_ETP`, `RededeReservatorios_consolidada` e `RededeReservatorios_em_teste`. Usuários do R podem informar outro local pelo argumento `diretorio_de_dados` de `SisMEVAZ_interativo()`.

## Executar o painel

- **Windows**: dê duplo clique em **`iniciar_sismevaz.bat`**.
- **Linux/macOS**: execute **`./iniciar_sismevaz.sh`**.

O iniciador confere o R 4.5.3 e o pacote instalado, localiza a pasta `dados`, chama `SisMEVAZ::SisMEVAZ_interativo()` e abre o navegador em `http://localhost:8082`. Para parar, feche a janela (Windows) ou pressione `Ctrl+C` (Linux/macOS).

## Solução de problemas

- **"R 4.5.3 não foi encontrado"**: instale ou selecione exatamente a versão homologada e execute novamente.
- **"Instalação incompleta"**: execute novamente `instalar_sismevaz` para instalar o pacote e todos os componentes da interface.
- **"A pasta 'dados' nao foi encontrada"**: ver "Configuração dos dados" acima.
- Os erros de traçado de bacia ou de simulação exigem que o R e o pacote `SisMEVAZ` estejam corretamente instalados (etapa de instalação do R).
