
rm(list = ls())

{
library(srvyr)
library(sampling)
library(survey)
library(sampling)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(reshape2)
library(janitor)
library(rio)
library(TeachingSampling)
}

# ------------------------------------------------------------------------------
# CARGAMOS LOS TAMAÑOS ---------------------------------------------------------
# ------------------------------------------------------------------------------
tamanio <- readRDS("productos/01_muestra/tamanio.rds")

#-------------------------------------------------------------------------------
# Marco 
#-------------------------------------------------------------------------------
marco <- readRDS("productos/01_muestra/base_colegios_marco.rds")

# ------------------------------------------------------------------------------
# SELECCIÓN DE COLEGIOS
# ------------------------------------------------------------------------------

selec_col <- marco %>% 
  rename(dom = "dom_1") %>%
  left_join(tamanio) %>% 
  group_by(dom) %>% 
    mutate(Mi = sum(suma_est),
           pik = inclusionprobabilities(Mi, unique(col_muestra)),
           sel = UPrandomsystematic(pik, eps=1e-6)) %>% 
    filter(sel == 1)

#--- Control ---
selec_col %>% group_by(dom) %>% summarise(selec = n(),
                                          tam = unique(col_muestra),
                                          dif = selec - tam) %>% View()
  

# ------------------------------------------------------------------------------
# SELECCIÓN DE ESTUDIANTES
# ------------------------------------------------------------------------------

selec_col %>% mutate()

aux <- marco_sin_inc_for %>% 
  group_by(dom_2,codigo_actividad_eco) %>% 
  summarise(Nh=n()) %>% 
  left_join(select(tamanio,dom_2=dominio,n_pro),by = "dom_2") %>% 
  left_join(marco_aux,by = "dom_2") 

seleccion <- aux %>% mutate(a = if_else(Nh<5,Nh,5),
                            b = Nh-a)
aux_2 <-  seleccion %>%  
  group_by(dom_2) %>% 
  summarise(Nh_b=sum(b), #cantidad restante para elegir muestra
            n_b = unique(n_pro)-sum(a)) %>% #tamaño muestral faltante por llenar
  right_join(seleccion,by="dom_2") %>% 
  mutate(n_b = if_else(n_b<0,0,n_b),
         PPT_b =if_else(Nh_b==0,0,ceiling((n_b)*(b/Nh_b))),
         PPT_b = PPT_b+a,
         Control = Nh - PPT_b)

seleccion <- aux_2                   








