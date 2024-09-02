rm(list = ls())

library(tidyverse)
library(readxl)
library(reshape2)
library(janitor)
library(dplyr)
library(readxl)
library(xlsx)
library(openxlsx)
library(rio)

#-------------------------------------------------------------------------------
# LECTURA DE LA BDD DEL MINISTERIO DE EDUCACIÓN
#-------------------------------------------------------------------------------

base_colegios <- read_excel("insumos/1MINEDUC_2023-2024-Inicio BBD.xlsx", 
                                             sheet = "Tabla n_01") %>% 
  clean_names() %>% 
  mutate(dom_1 = "nacional",
         dom_2 = tolower(provincia),
         dom_3 = tolower(canton),
         dom_4 = tolower(parroquia))

#-------------------------------------------------------------------------------
# Descriptivos de la base
#-------------------------------------------------------------------------------

dim(base_colegios)
names(base_colegios)

base_colegios %>% group_by(dom_2,dom_3) %>% 
  summarise(n()) %>% View()

# --- Cantidad de colegios por provincia
base_colegios %>%
  rename("dom" = dom_2) %>% 
  group_by(dom) %>% 
  summarise(N = n()) %>% #filter(grepl(dom_m,pattern = "2")) %>% 
  ggplot(aes(x=dom, y=N, fill = dom)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  labs(x="",y="")+
  #theme(legend.position = "") +
  theme(legend.position = "",axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

# --- Cantidad de colegios por provincia sin guayas, manabi y pich
base_colegios %>%
  rename("dom" = dom_2) %>% 
  filter(! dom %in% c("guayas","manabi","pichincha")) %>% 
  group_by(dom) %>% 
  summarise(N = n()) %>% #filter(grepl(dom_m,pattern = "2")) %>% 
  ggplot(aes(x=dom, y=N, fill = dom)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  labs(x="",y="")+
  #theme(legend.position = "") +
  theme(legend.position = "",axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

# --- Cantidad de colegios por canton. Cantidad de colegios menores a 50
base_colegios %>%
  rename("dom" = dom_3) %>%
  filter(regimen_escolar == "Sierra") %>% 
  group_by(dom) %>% 
  summarise(N = n()) %>% 
  filter(N < 30) %>% 
  ggplot(aes(x=dom, y=N, fill = dom)) + 
  geom_bar(stat="identity", position = position_dodge()) +
  labs(x="",y="")+
  #theme(legend.position = "") +
  theme(legend.position = "",axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))



#-------------------------------------------------------------------------------
# Calculo del tamaño de muestra
#-------------------------------------------------------------------------------

p = 0.5
q = 0.5
nc = 0.95
z = qnorm(nc+(1-nc)/2)
er = 0.1

base_colegios %>% 
  rename("dom" = dom_3) %>% 
  group_by(dom) %>% 
  mutate( N = n(),
    num = z^2 * N * p * q,
    denom = er^2 * (N-1) + z^2 * p *q,
    n_muestra = ceiling(num/denom)) %>%
  ungroup() %>% 
  group_by(dom) %>% 
  summarise(n = unique(n_muestra),
            N = unique(N)) %>%
  adorn_totals() %>% 
  View("tamano")










marco_canasta_08 <- directorio %>% 
  filter(codigo_actividad_eco %in% canasta$codigo_actividad_eco,
         tamanou_plazas!=1) %>% 
  filter(situacion == 1) %>%
  #filtro las empresas no ubicadas
  filter(is.na(empresas_noubicadas)) %>%
  mutate(
    dom_1 = codigo_seccion,
    dom_2 = paste0(tamanou_plazas, codigo_seccion),
    id_empresa = as.character(id_empresa)) %>% 
  group_by(dom_2) %>% 
  mutate(n_cod_act_eco = n_distinct(codigo_actividad_eco)) %>% 
  ungroup()
# 
# marco_canasta_08 %>% 
#   select(dom_2,n_cod_act_eco) %>% 
#   distinct(dom_2,n_cod_act_eco) %>% View("UNO")
# 
# marco_canasta_08 %>% filter(dom_2=="4A") %>% 
#   select(dom_2,n_cod_act_eco) %>% 
#   distinct(dom_2,n_cod_act_eco) %>% View()


#-------------------------------------------------------------------------------
# EXPORTANDO MARCO CANASTA 2021 
#-------------------------------------------------------------------------------
export(marco_canasta_08,
       "productos/01_marco/marco_canasta_08.rds")

