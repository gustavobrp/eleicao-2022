#### SCRIPT VOTACAO SECAO ELEITORAL MG
#### GUSTAVO BRUNO DE PAULA
#### gustavobrp@gmail.com @gustavobrp

#### pacotes ====
library(tidyverse)
library(sf)
library(geobr)
#library(ggmap)
library(tidygeocoder)
#library(leaflet)
#library(osmdata)
#library(OpenStreetMap)

#### bases ====
##### br ====
#### base de votacoes extraida do site do TSE
bd.br <- read_delim("bases/votacao_secao_2022_BR.csv",
                    delim = ";",
                    locale = locale(encoding = "latin1"))

names(bd.br)
table(bd.br$NM_VOTAVEL)
table(bd.br$NM_MUNICIPIO)

#### filtrando a base para conter apenas BH e criando uma variavel chave de zona e secao para usar depois para unir bases
bd.bh <- bd.br %>% 
  filter(NM_MUNICIPIO %in% c("BELO HORIZONTE"),
         NR_TURNO == 1) %>% 
  mutate(zona_secao = str_c(NR_ZONA,
                            NR_SECAO,
                            sep = "-"))

table(bd.bh$NR_ZONA)

##### perfil eleitorado ====
#### base extraida do site do TSE, nesse caso a de perfil por zona eleitoral, que inclui enderecos
bd.perfil.br <- read_delim("bases/eleitorado_local_votacao_2022.csv",
                        delim = ";",
                        locale = locale(encoding = "latin1"))

bd.perfil.bh <- bd.perfil.br %>% 
  filter(NM_MUNICIPIO %in% c("BELO HORIZONTE"),
         NR_TURNO == 1)

#### criando uma variavel chave com a zona eleitoral e a secao, para unir depois as bases
#### com os enderecos
bd.perfil.bh <- bd.perfil.bh %>% 
  mutate(endereco_local = str_c(DS_ENDERECO,
                                NM_BAIRRO,
                                NM_MUNICIPIO,
                                "MINAS GERAIS",
                                NR_CEP,
                                "BRASIL",
                                sep = " "),
         zona_secao = str_c(NR_ZONA,
                            NR_SECAO,
                            sep = "-"))

names(bd.perfil.bh)

bd.perfil.bh <- bd.perfil.bh %>% 
  select(-c(DT_GERACAO,
            HH_GERACAO,
            AA_ELEICAO,
            DT_ELEICAO,
            DS_ELEICAO,
            NR_TURNO,
            CD_MUNICIPIO,
            DS_SITU_SECAO_ACESSIBILIDADE,
            CD_SITU_SECAO_ACESSIBILIDADE,
            NR_TELEFONE_LOCAL))

#bingmapskey <- "CHAVE BING"

#usethis::edit_r_environ()

#### incluindo a latitude e longitude do local de votacao
#### para isso, usei aqui o pacote tidygeocode e o servico do bing, para encontrar as lat e long dos
#### dos enderecos dos locais de votacao; para usar o do bing, tem que ter uma chave de api (gratuita)
bd.perfil.bh <- bd.perfil.bh %>% 
  geocode(endereco_local,
          method = "bing")

bd.nr.local.lat.long <- bd.perfil.bh %>% 
  select(zona_secao,
         lat,
         long)


#### unindo as bases de voto com os resultados das lat e long extraidas dos enderecos, usando a chave criada
bd.bh.voto <- left_join(bd.bh,
                   bd.nr.local.lat.long,
                   by = "zona_secao")

bd.bh.voto <- bd.bh.voto %>% 
  select(zona_secao,
         NR_LOCAL_VOTACAO,
         NR_ZONA,
         NR_SECAO,
         NM_VOTAVEL,
         QT_VOTOS,
         lat,
         long)

##### zonas eleitorais cep ====
#mg.ze <- read_delim("bases/lista_zonas_eleitorais_mg.csv",
#                    delim = ",",
#                    locale = locale(encoding = "latin1"))

#table(mg.ze$nome_municipio)

#mg.ze <- mg.ze %>% 
#  filter(nome_municipio %in% c("BELO HORIZONTE"))

##### mg estadual ====
#bd.mg <- read_delim("bases/votacao_secao_2022_MG.csv",
#                    delim = ";",
#                    locale = locale(encoding = "latin1"))

#names(bd.mg)

#bd.bh.gov <- bd.mg %>% 
#  filter(NM_MUNICIPIO == "BELO HORIZONTE") %>% 
#  select(NR_LOCAL_VOTACAO, QT_VOTOS, NR_ZONA, NR_SECAO, NR_VOTAVEL, NM_VOTAVEL)
 
#### shapes ====
#### usando o pacote geobr (topster)
## shape de BH
shp.bh <- read_municipality(code_muni = 3106200,
                            year = "2020")

plot(shp.bh$geom)

#### uniformizei o crs de todas as bases, para evitar problemas (mas nao encontrei problema nisso sem alterar)
shp.bh <- st_transform(shp.bh,
                     crs = "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0 +no_defs")

plot(shp.bh$geom)

shp.bh.votos <- st_as_sf(bd.bh.voto,
                         coords = c("long",
                                    "lat"),
                         crs = "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0 +no_defs")

st_crs(shp.bh)

st_crs(shp.bh.votos)

plot(shp.bh.votos$geometry)

shp.bh.ap <- read_weighting_area(code_weighting = 3106200,
                                 year = 2010)

plot(shp.bh.ap$geom)

shp.bh.ap <- shp.bh.ap %>% 
  select(code_weighting,
         geom)

shp.bh.ap <- st_transform(shp.bh.ap,
                       crs = "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0 +no_defs")

plot(shp.bh.ap$geom)

##### base de regional de bh =====
#### shape baixado pelo site da prefeitura de bh
shp.bh.reg <- st_read("bases/shapes/REGIONAL.shp")

shp.bh.reg <- st_transform(shp.bh.reg,
                          crs = "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0 +no_defs")

plot(shp.bh.reg)

##### base shape de setor censitario ====
shp.bh.sc <- read_census_tract(code_tract = 3106200)

plot(shp.bh.sc$geom)

shp.bh.sc <- st_transform(shp.bh.sc,
                          crs = "+proj=longlat +datum=WGS84 +ellps=GRS80 +towgs84=0,0,0 +no_defs")

#### analises area de ponderacao ====
##### unindo bases ====
bd.bh.votos.ap <- st_join(shp.bh.ap,
                          shp.bh.votos)

##### modificacoes e novas variaveis na base de area ponderacao ====
bd.bh.votos.ap <- bd.bh.votos.ap %>% 
  group_by(code_weighting,
           NM_VOTAVEL) %>% 
  summarise(total_votos = sum(QT_VOTOS)) %>% 
  ungroup()

bd.bh.vencedores <- bd.bh.votos.ap %>% 
  group_by(code_weighting) %>% 
  filter(total_votos == max(total_votos)) %>% 
  ungroup() %>% 
  mutate(NM_VOTAVEL = case_when(NM_VOTAVEL == "LUIZ INÁCIO LULA DA SILVA" ~ "Lula",
                                NM_VOTAVEL == "JAIR MESSIAS BOLSONARO" ~ "Bolsonaro"))

##### plots ====
bd.bh.vencedores %>% 
  ggplot() + 
  geom_sf(aes(fill = NM_VOTAVEL),
          size = 0.5,
          alpha = 0.5) +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8"),
                    breaks = c("Lula",
                               "Bolsonaro")) +
  theme_void() +
  labs(fill = "Candidato") +
  theme(legend.title.align = 0.5)

coord.bh <- c(-44.088959,-20.063901,-43.825287,-19.771238)

mapa.bh <- get_map(location = coord.bh,
                   source = "stamen",
                   color = "bw")

ggmap(mapa.bh)

ggmap(mapa.bh) +
  geom_sf(data = bd.bh.vencedores,
          aes(fill = NM_VOTAVEL),
          size = 0.5,
          alpha = 0.3,
          inherit.aes = FALSE) +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8"),
                    breaks = c("Lula",
                               "Bolsonaro")) +
  theme_void() +
  labs(fill = "Candidato") +
  theme(legend.title.align = 0.5)

p1 <- ggplot() +
  geom_sf(data = bd.bh.vencedores,
          aes(fill = NM_VOTAVEL),
          size = 0.5,
          alpha = 0.6) +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8"),
                    breaks = c("Lula",
                               "Bolsonaro")) +
  theme_void() +
  labs(fill = "Candidato",
       title = "Candidato mais votado em Belo Horizonte, \npor área de ponderação",
       caption = "Fonte: TSE; geobr\nGustavo Paula") +
  theme(legend.title.align = 0.5,
        legend.text = element_text(family = "Verdana"),
        plot.caption = element_text(hjust = 1,
                                    face = "bold",
                                    family = "Verdana"),
        plot.title = element_text(hjust = 0.3,
                                  size = 18,
                                  face = "bold",
                                  family = "Verdana")) +
  guides()

p1 + geom_sf_label(data = shp.bh.reg,
               aes(label = NOME),
               colour = "black",
               alpha = 0.5)

#### analise por setor censitario ====
##### unindo bases ====
bd.bh.votos.sc <- st_join(shp.bh.sc,
                          shp.bh.votos)

#### retirando linhas que nao consta zona eleitoral
bd.bh.votos.sc <- bd.bh.votos.sc %>% 
  filter(!is.na(zona_secao))

##### modificacoes e novas variaveis ====
names(bd.bh.votos.sc)

bd.bh.votos.sc <- bd.bh.votos.sc %>% 
  group_by(code_tract,
           NM_VOTAVEL) %>% 
  summarise(total_votos = sum(QT_VOTOS, na.rm = TRUE)) %>% 
  ungroup()

table(bd.bh.votos.sc$NM_VOTAVEL)

bd.bh.vencedores.sc <- bd.bh.votos.sc %>% 
  group_by(code_tract) %>% 
  filter(total_votos == max(total_votos)) %>% 
  ungroup() %>% 
  mutate(NM_VOTAVEL = case_when(NM_VOTAVEL == "LUIZ INÁCIO LULA DA SILVA" ~ "Lula",
                                NM_VOTAVEL == "JAIR MESSIAS BOLSONARO" ~ "Bolsonaro"))

table(bd.bh.vencedores.sc$NM_VOTAVEL)

##### plots ====
ggplot() +
  geom_sf(data = bd.bh.vencedores.sc,
          aes(fill = NM_VOTAVEL),
          size = 0.1,
          alpha = 0.6) +
  scale_fill_manual(values = c("#e41a1c",
                               "#377eb8"),
                    breaks = c("Lula",
                               "Bolsonaro")) +
  theme_void() +
  labs(fill = "Candidato",
       title = "Candidato mais votado em Belo Horizonte, \npor setor censitário",
       caption = "Fonte: TSE; geobr\nGustavo Paula") +
  theme(legend.title.align = 0.5,
        legend.text = element_text(family = "Verdana"),
        plot.caption = element_text(hjust = 1,
                                    face = "bold",
                                    family = "Verdana"),
        plot.title = element_text(hjust = 0.3,
                                  size = 18,
                                  face = "bold",
                                  family = "Verdana")) +
  guides()

p1 + geom_sf_label(data = shp.bh.reg,
                   aes(label = NOME),
                   colour = "black",
                   alpha = 0.5)
