A Metodologia para o Cálculo de Vazões (MEVAZ) é a metodologia oficial para a estimativa das séries de vazões afluentes aos reservatórios no Ceará e de sua oferta hídrica de longo prazo.

Este repositório contém o pacote R `SisMEVAZ` e sua interface web em R/Shiny. O código fica concentrado em `SisMEVAZ/`, enquanto os dados operacionais permanecem na pasta `dados/`, fora do pacote.

## Instalação recomendada no Windows (sem terminal)

1. Instale uma versão atual do R (4.5.0 ou posterior).
2. Baixe ou clone este projeto.
3. Baixe e descompacte a pasta `dados` na raiz do projeto.
4. Dê duplo clique em `instalacao/windows/Instalar-SisMEVAZ.bat`.
5. Depois, dê duplo clique em `abrir_interface/windows/Abrir-SisMEVAZ.bat`.

O instalador localiza automaticamente o R, instala o pacote, a interface e as dependências e cria um atalho na Área de Trabalho. Não é necessário instalar RStudio, Rtools ou `devtools`, nem abrir um terminal.

## Requisitos

- R 4.5.0 ou posterior;
- navegador atualizado;
- conexão com a internet na primeira instalação;
- pelo menos 10 a 15 GB livres;
- a pasta externa `dados`, disponível no link abaixo.

Compatibilidade verificada localmente: construção, instalação e carregamento no R 4.5.1 sobre Ubuntu 24.04. Novas versões do R devem passar pelo mesmo teste antes de serem declaradas homologadas.

## Docker no Linux (alternativa reproduzível)

Use Docker quando o R instalado na máquina for antigo ou quando não quiser alterar o ambiente local. O R da máquina não é utilizado: a imagem contém o R 4.5.3, as bibliotecas geoespaciais, os pacotes R e o WhiteboxTools.

Antes de iniciar, confirme que a pasta `dados` está na raiz do projeto e execute os comandos a partir dessa raiz.

### Verificar o Docker

```bash
docker --version
docker compose version
```

Se o serviço não estiver ativo em uma distribuição com systemd:

```bash
sudo systemctl start docker
```

### Erro de permissão em `/var/run/docker.sock`

A mensagem `permission denied while trying to connect to the docker API` significa que o usuário atual não tem acesso ao socket do daemon Docker. Não é um erro da imagem Sis-MEVAZ.

Para executar imediatamente:

```bash
sudo docker compose up --build
```

Para permitir o uso futuro sem `sudo`:

```bash
sudo groupadd -f docker
sudo usermod -aG docker "$USER"
newgrp docker
docker run --rm hello-world
```

Adicionar um usuário ao grupo `docker` concede privilégios equivalentes aos de administrador. Em computadores institucionais ou compartilhados, consulte a equipe responsável antes de fazer essa alteração. Também é possível continuar usando `sudo` sem alterar os grupos do usuário.

Depois da configuração:

```bash
docker compose up --build
```

Quando aparecer a mensagem de que o servidor está ativo, abra <http://localhost:8082>. Para encerrar, pressione `Ctrl+C`. Nas próximas execuções, se o código não mudou, basta:

```bash
docker compose up
```

Para executar em segundo plano ou parar o sistema:

```bash
docker compose up -d
docker compose down
```

A pasta local `dados` é montada no contêiner; entradas e resultados permanecem no computador.

## Instalação nativa em Linux/macOS

- Windows, Linux ou macOS
- [R](https://cran.r-project.org/), versão 4.5.0 ou posterior
- A pasta de dados `dados` https://drive.google.com/file/d/1mE1IyT91-oy2ermkb-Q7DFGlyzrajMfh/view?usp=sharing (~5 GB). **Esta pasta não está incluída neste repositório Git** por causa do seu tamanho.

## Estrutura do repositório

| Pasta | Conteúdo |
| --- | --- |
| `SisMEVAZ/R/` | Funções do pacote R e back-end de cálculo |
| `SisMEVAZ/inst/shiny/` | Interface web em R/Shiny |
| `instalacao/instalar_sismevaz.R` | Lógica comum usada pelos dois instaladores e pelo Docker |
| `instalacao/windows/` | Instalador para Windows |
| `instalacao/linux/` | Instalador para Linux/macOS |
| `abrir_interface/windows/` | Iniciador da interface no Windows |
| `abrir_interface/linux/` | Iniciador da interface no Linux/macOS |
| `dados/` | Bases fixas, entradas, saídas e versões históricas |

Os arquivos geoespaciais intermediários são criados no diretório
temporário do sistema durante a execução e removidos quando o aplicativo
é encerrado. As entradas e os resultados da simulação continuam sendo
gravados em `dados/RededeReservatorios_em_teste/`.

## Instalação nativa

Clone este repositório:

```
git clone https://github.com/maetzoo/Sis-MEVAZ-dash-no-shiny.git
```

A instalação inclui, em uma única etapa, o pacote R, a interface interativa, suas dependências e o WhiteboxTools. Não é necessário abrir o RStudio ou instalar separadamente os pacotes do Shiny.

**Windows**

Dê duplo clique em **`instalacao/windows/Instalar-SisMEVAZ.bat`**.

**Linux / macOS**

```
chmod +x instalacao/linux/instalar_sismevaz.sh abrir_interface/linux/abrir_sismevaz.sh
./instalacao/linux/instalar_sismevaz.sh
```

## Configuração dos dados

No modo interativo, a pasta `dados` deve permanecer na raiz do projeto. Ela deve conter `Dados_fixos`, `Plu_ETP`, `RededeReservatorios_consolidada` e `RededeReservatorios_em_teste`. Usuários do R podem informar outro local pelo argumento `diretorio_de_dados` de `SisMEVAZ_interativo()`.

## Executar o painel

- **Windows**: use o atalho criado ou dê duplo clique em **`abrir_interface/windows/Abrir-SisMEVAZ.bat`**.
- **Linux/macOS**: execute **`./abrir_interface/linux/abrir_sismevaz.sh`**.

O iniciador confere se o R é 4.5.0 ou posterior e o pacote instalado, localiza a pasta `dados`, chama `SisMEVAZ::SisMEVAZ_interativo()` e abre o navegador em `http://localhost:8082`. Para parar, feche a janela (Windows) ou pressione `Ctrl+C` (Linux/macOS).

## Solução de problemas

- **"permission denied" em `/var/run/docker.sock`**: execute temporariamente com `sudo` ou configure o grupo `docker` conforme a seção "Docker no Linux".
- **"Cannot connect to the Docker daemon"**: inicie o serviço com `sudo systemctl start docker`.


- **"R compatível não foi encontrado"**: instale ou selecione o R 4.5.0 ou posterior e execute novamente.
- **"Instalação incompleta"**: execute novamente `instalar_sismevaz` para instalar o pacote e todos os componentes da interface.
- **"A pasta 'dados' nao foi encontrada"**: ver "Configuração dos dados" acima.
- Os erros de traçado de bacia ou de simulação exigem que o R e o pacote `SisMEVAZ` estejam corretamente instalados (etapa de instalação do R).
