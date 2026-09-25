# Recomendações para atualizar a instalação no tutorial do Sis-MEVAZ

A Parte II do tutorial precisa ser reestruturada porque os antigos passos 4 a 7 foram substituídos por um instalador único, válido tanto para o pacote R quanto para a interface interativa.

## 1. Atualizar os pré-requisitos

Na lista de ferramentas obrigatórias:

- Manter apenas o **R 4.5.3**, exatamente essa versão.
- Informar que versões anteriores ou posteriores não são homologadas e serão recusadas pelo instalador.
- Retirar o **RStudio** da lista obrigatória. Ele continua opcional para quem utilizará o Sis-MEVAZ programaticamente.
- Retirar o **Rtools** da instalação obrigatória.
- Retirar o **GitHub Desktop** da lista obrigatória. Ele pode permanecer como opção para baixar ou atualizar o repositório.
- Acrescentar um navegador atualizado como requisito da interface interativa.
- Informar que é necessária conexão com a internet durante a instalação inicial, pois o instalador baixa pacotes do CRAN, GitHub, o `phylin` arquivado e o WhiteboxTools.

A recomendação antiga de “atualizar o R” deve ser substituída por:

> Instale especificamente o R 4.5.3. Não atualize para uma versão posterior sem que ela esteja homologada para o Sis-MEVAZ.

## 2. Explicar as duas formas de uso

O tutorial deve apresentar claramente:

- **Modo interativo:** interface gráfica aberta no navegador, recomendada para usuários que não programam.
- **Modo programático:** funções do pacote chamadas diretamente em scripts ou no console do R.

É importante deixar claro que não são duas instalações diferentes. O mesmo instalador instala:

- o pacote `SisMEVAZ`;
- a interface Shiny;
- todas as dependências da interface;
- `hydrobr`;
- `phylin`;
- WhiteboxTools.

## 3. Simplificar os passos de instalação

Os passos antigos devem mudar assim:

| Passos antigos | Alteração |
|---|---|
| Instalar GitHub Desktop | Tornar opcional ou substituir por “obter os arquivos do projeto” |
| Baixar os arquivos | Manter, atualizando o endereço e as opções de download |
| Baixar a pasta `dados` | Manter, mas atualizar link, tamanho e posição |
| Instalar `devtools` | Excluir |
| Instalar dependências manualmente | Excluir |
| Usar `Build > Load All` | Excluir |
| Usar `Build > Install Package` | Excluir |
| Executar `library(SisMEVAZ)` | Manter somente na seção de uso programático |

### Novo fluxo para Windows

1. Instalar o R 4.5.3.
2. Baixar ou clonar o projeto.
3. Baixar e descompactar a pasta `dados`.
4. Colocar `dados` na raiz do projeto, ao lado de `iniciar_sismevaz.bat`.
5. Dar duplo clique em `instalacao\instalar_sismevaz.bat`.
6. Aguardar a mensagem de instalação concluída.

Não é preciso abrir o RStudio nem instalar pacotes individualmente.

### Novo fluxo para Linux/macOS

A partir da raiz do projeto:

```bash
chmod +x instalacao/instalar_sismevaz.sh iniciar_sismevaz.sh
./instalacao/instalar_sismevaz.sh
```

## 4. Atualizar a seção da pasta de dados

O tutorial antigo utiliza um link diferente. Conforme o README atual, deve ser usado:

<https://drive.google.com/file/d/1mE1IyT91-oy2ermkb-Q7DFGlyzrajMfh/view?usp=sharing>

A pasta deve ter esta posição:

```text
Sis-MEVAZ/
├── dados/
├── SisMEVAZ/
├── instalacao/
├── iniciar_sismevaz.bat
└── iniciar_sismevaz.sh
```

Também deve ser informado que `dados` precisa conter:

```text
Dados_fixos/
Plu_ETP/
RededeReservatorios_consolidada/
RededeReservatorios_em_teste/
```

A recomendação de apenas 5 GB livres deve ser revista: como o arquivo compactado tem aproximadamente 5 GB, será necessário espaço adicional para download, descompactação, pacotes do R e resultados. É prudente recomendar pelo menos **10–15 GB livres**.

## 5. Acrescentar como abrir a interface

Esta seção não existe no tutorial não interativo.

### Windows

Dar duplo clique em:

```text
iniciar_sismevaz.bat
```

### Linux/macOS

```bash
./iniciar_sismevaz.sh
```

O sistema:

- verifica o R 4.5.3;
- verifica se o pacote foi instalado;
- localiza a pasta `dados`;
- inicia a interface;
- abre `http://localhost:8082` no navegador.

Para encerrar, fecha-se a janela do iniciador no Windows ou usa-se `Ctrl+C` no Linux/macOS.

## 6. Manter uma subseção para programadores

Depois da instalação única, quem preferir usar o pacote diretamente pode executar:

```r
library(SisMEVAZ)

SisMEVAZ_redeTeste(
  diretorio_de_dados = "CAMINHO/para/dados"
)

SisMEVAZ_print_Sintese(
  diretorio_de_dados = "CAMINHO/para/dados",
  versao_print = "redeTeste"
)
```

A interface também pode ser iniciada pelo R:

```r
SisMEVAZ::SisMEVAZ_interativo(
  diretorio_de_dados = "CAMINHO/QUE/CONTEM/DADOS"
)
```

Nesse último comando, o caminho deve apontar para o diretório que **contém** a pasta `dados`, e não para a pasta `dados` diretamente.

## 7. Incluir validação e problemas comuns

Após instalar, o tutorial deve orientar o usuário a verificar:

- presença da mensagem “Instalação concluída com sucesso”;
- abertura da interface no navegador;
- localização correta da pasta `dados`;
- ausência de mensagens sobre componentes faltantes.

Erros a documentar:

- **“R 4.5.3 não foi encontrado”**: instalar ou selecionar exatamente essa versão;
- **“Instalação incompleta”**: executar novamente o instalador;
- **“A pasta ‘dados’ não foi encontrada”**: reposicionar a pasta na raiz do projeto;
- falha ao baixar dependências: conferir conexão com a internet, proxy ou bloqueios institucionais;
- porta 8082 indisponível: encerrar outra execução do sistema.

## Arquivos do projeto consultados

- `README.md`
- `SisMEVAZ/README.md`
- `SisMEVAZ/DESCRIPTION`
- `instalacao/instalar_sismevaz.R`
- `instalacao/instalar_sismevaz.bat`
- `instalacao/instalar_sismevaz.sh`
- `iniciar_sismevaz.bat`
- `iniciar_sismevaz.sh`
- `SisMEVAZ/R/SisMEVAZ_interativo.R`
- `rascunho_Tutorial_SisMEVAZnaoInterativo.pdf`
