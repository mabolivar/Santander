#***********************************************
# Santader 
#***********************************************


#'=============================================
#' Libraries
#'=============================================

library(readr)
library(dplyr)
library(tidyr)
library(sparklyr)
library(stringr)

#'=============================================
#' Read data
#'=============================================



df <- read_csv("./data/train.csv",
               col_types = cols( .default = col_integer(), 
                                 fecha_dato = col_date(format = ""), 
                                 ncodpers = col_character(), 
                                 ind_empleado = col_character(), 
                                 pais_residencia = col_character(), 
                                 sexo = col_character(), 
                                 age = col_integer(), 
                                 fecha_alta = col_date(format = ""), 
                                 ind_nuevo = col_integer(), 
                                 antiguedad = col_integer(), 
                                 indrel = col_integer(), 
                                 ult_fec_cli_1t = col_date(format = ""), 
                                 indrel_1mes = col_character(), 
                                 tiprel_1mes = col_character(), 
                                 indresi = col_character(), 
                                 indext = col_character(), 
                                 conyuemp = col_character(), #****** 
                                 canal_entrada = col_character(), 
                                 indfall = col_character(), 
                                 tipodom = col_integer(), 
                                 cod_prov = col_integer(), 
                                 nomprov = col_character(), 
                                 ind_actividad_cliente = col_integer(), 
                                 renta = col_double(), 
                                 segmento = col_character() )) %>% 
  select(fecha_dato,ncodpers,contains("fin_ult1")) 
gc()


#'=============================================
#' Read data
#'=============================================

df %>% saveRDS("./data/df_small.rds")

df <- readRDS("./data/df_small.rds")

library(purrr)

df %>%
  mutate(end = ncodpers %>% str_sub(-1,-1)) %>% 
  split(.$end) %>%
  map(. %>% gather(prod,rating,-ncodpers,-fecha_dato) %>%
        filter(rating == 1))
  
long_df <- df %>% 
  gather(prod,rating,-ncodpers,-fecha_dato) %>%
  filter(rating == 1)
  

