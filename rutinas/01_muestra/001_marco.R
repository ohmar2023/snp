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
  mutate(dom_1 = cod_canton,
         suma_est = estudiantes_masculino_tercer_ano_bach + 
           estudiantes_femenino_tercer_ano_bach) %>% 
  filter(suma_est > 0) %>% 
  select(c(1:19,61,62),dom_1,suma_est)
  
#-------------------------------------------------------------------------------
# Descriptivos de la base
#-------------------------------------------------------------------------------


# --- Cantidad de colegios por provincia
base_colegios %>%
  rename("dom" = dom_3) %>% 
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
  filter(N < 50) %>% 
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
deff = 2

tam_est <- base_colegios %>% 
  rename("dom" = dom_1) %>% 
  group_by(dom) %>% 
  mutate( N = sum(suma_est),
    num = z^2 * N * p * q * deff,
    denom = er^2 * p^2 * (N-1) + z^2 * p *q *deff, #error absoluto
    n_muestra = ceiling(num/denom)) %>% 
  #select(N,estudiantes_masculino_tercer_ano_bach,estudiantes_femenino_tercer_ano_bach,
  #       num,denom) %>% 
  #View()
ungroup() %>% 
  group_by(dom) %>% 
  summarise(est_muestra = unique(n_muestra),
            est_total = unique(N),
            col_total = n()) 

#-------------------------------------------------------------------------------
# Calculo media de estudiantes
#-------------------------------------------------------------------------------

med_est <- base_colegios %>% 
  rename("dom" = dom_1) %>% 
  group_by(dom) %>% 
  summarise(est_total = sum(suma_est),
            col_total = n(),
            est_prom = mean(suma_est),
            q_1 = ceiling(quantile(suma_est,probs = c(0.2)))) 


#-------------------------------------------------------------------------------
# Juntando resulatdos para el tamaño final
#-------------------------------------------------------------------------------

tam_final <- tam_est %>% left_join(med_est) %>% 
  mutate(col_muestra = ceiling(est_muestra/q_1)) %>% 
  select(dom, est_total, est_muestra, est_prom, q_1, col_total, col_muestra)


ggplot(data = base_colegios, mapping = aes(x=suma_est)) + geom_boxplot(outlier.colour="pink")

#-------------------------------------------------------------------------------
# EXPORTANDO MARCO Y TAMAÑO DE MUESTRA 
#-------------------------------------------------------------------------------
export(base_colegios,
       "productos/01_muestra/base_colegios_marco.rds")

export(tam_final,
       "productos/01_muestra/tamanio.rds")












