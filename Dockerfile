FROM rocker/r-ver:4.5.3

ENV DEBIAN_FRONTEND=noninteractive \
    SISMEVAZ_BASE_DIR=/opt/sismevaz \
    R_DEFAULT_INTERNET_TIMEOUT=600

# Dependências nativas de sf/lwgeom, PostgreSQL, HTTP e renderização.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    cmake \
    git \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libpq-dev \
    libproj-dev \
    libssl-dev \
    libtiff-dev \
    libudunits2-dev \
    libxml2-dev \
    make \
    pkg-config \
    unzip \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/sismevaz
COPY SisMEVAZ/ SisMEVAZ/
COPY instalacao/instalar_sismevaz.R instalacao/instalar_sismevaz.R

RUN Rscript instalacao/instalar_sismevaz.R \
    && Rscript -e "stopifnot(getRversion() >= '4.5.0', requireNamespace('SisMEVAZ', quietly = TRUE))"

EXPOSE 8082
CMD ["Rscript", "-e", "SisMEVAZ::SisMEVAZ_interativo(diretorio_de_dados=Sys.getenv('SISMEVAZ_BASE_DIR'), navegador=FALSE, porta=8082, endereco='0.0.0.0')"]
