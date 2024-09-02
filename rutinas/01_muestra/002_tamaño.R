rm(list=ls())
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(janitor)
library(reshape2)

#-------------------------------------------------------------------------------
# CARGAMOS EL MARCOIPP
marco <- readRDS("productos/01_marco/marco_canasta_08.rds")
tnr <- readRDS("productos/01_marco/tnr_ipp.rds")
#-------------------------------------------------------------------------------

# CARGAMOS EL DIRECTORIO 2020 ---------------------------------------------

# carpeta_old <- last(list.dirs(paste0("insumos/01_tamanio/",anion - 1,"/nogit/"), recursive = F, full.names = F))
# nombre_diee_old <- list.files(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old), pattern = ".rds")
# directorio_2020 <- readRDS(paste0("insumos/01_tamanio/",anion - 1,"/nogit/", carpeta_old, "/", nombre_diee_old)) %>%filter(anio==2020)
# directorio_2020 <- directorio_2020 %>%  mutate(id_empresa=as.character(id_empresa))

#-------------------------------------------------------------------------------
# INCLUSION FORZOSA
#-------------------------------------------------------------------------------
inc_for <- marco %>% 
mutate(inclusion_forzosa=ifelse(tamanou_plazas==5,1,0)) %>% 
filter(inclusion_forzosa==1)

#-------------------------------------------------------------------------------
# NO INCLUSION FORZOSA
#-------------------------------------------------------------------------------
marco_sin_inc_for <- marco %>% 
mutate(inclusion_forzosa=ifelse(tamanou_plazas==5,1,0)) %>% 
filter(inclusion_forzosa==0)

#-------------------------------------------------------------------------------
# PARAMETROS
#-------------------------------------------------------------------------------
nc=0.95
z=qnorm(nc+(1-nc)/2)
er=0.1

# CALCULO -----------------------------------------------------------------

tamanio <- marco_sin_inc_for %>% 
mutate(dominio=dom_2,
       ventas_totales=as.numeric(ventas_totales)) %>% 
group_by(dominio) %>% 
summarise(N=n(),
          desv=sd(ventas_totales,na.rm = T),
          ventas=sum(ventas_totales,na.rm = T)) %>% 
mutate(numerador=(N*desv)^2,
       denominador=((N-1)/N)*((er*ventas/z)^2)+N*(desv^2),
       tam=numerador/denominador) %>% 
left_join(select(tnr,dominio,tnr_pro),by="dominio") %>% 
mutate(tnr_pro=ifelse(is.na(tnr_pro),0,tnr_pro/100),
       n3=ceiling(tam/(1-tnr_pro)),
       n4=ifelse(n3>N,N,n3))

#sum(tamanio$n1)
sum(tamanio$n4,na.rm = T)

tamanio_final <- inc_for %>% 
  mutate(dominio=paste0(tamanou_plazas,codigo_seccion)) %>% 
  group_by(dominio) %>% 
  summarise(N  = n(),
            n4 = n()) %>% 
  rbind(select(tamanio,dominio,N,n4)) %>% 
  group_by(dominio) %>% 
  summarise(Total=sum(N),
            n_pro = sum(n4)) %>% 
  left_join(select(marco[!duplicated(marco$dom_2),],
                   dominio = (dom_2),
                   n_cod_act_eco),by = "dominio")

#-------------------------------------------------------------------------------
# EXPORTANDO MARCO CANASTA 2021 
#-------------------------------------------------------------------------------
export(tamanio_final,
       "productos/02_tamanio_seleccion/tamanio.rds")



