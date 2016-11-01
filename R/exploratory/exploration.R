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
                                 segmento = col_character() ))
gc()

#=================================================
# Exploration
#=================================================

#num obs per individual
df %>% group_by(ncodpers) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  arrange((cnt)) %>%
  print(n = 100)

df %>% filter(tiprel_1mes == "R")

df %>%
  filter(ncodpers == 672422) %>%
  write.csv("clipboard-1617")

# Target
target <- df %>% 
  group
filter()

#sum
df %>% 
  group_by(ncodpers) %>%
  mutate(ind_ahor_fin_ult1 = sum( ind_ahor_fin_ult1 ), ind_aval_fin_ult1 = sum( ind_aval_fin_ult1 ), ind_cco_fin_ult1 = sum( ind_cco_fin_ult1 ), ind_cder_fin_ult1 = sum( ind_cder_fin_ult1 ), ind_cno_fin_ult1 = sum( ind_cno_fin_ult1 ), ind_ctju_fin_ult1 = sum( ind_ctju_fin_ult1 ), ind_ctma_fin_ult1 = sum( ind_ctma_fin_ult1 ), ind_ctop_fin_ult1 = sum( ind_ctop_fin_ult1 ), ind_ctpp_fin_ult1 = sum( ind_ctpp_fin_ult1 ), ind_deco_fin_ult1 = sum( ind_deco_fin_ult1 ), 
         ind_deme_fin_ult1 = sum( ind_deme_fin_ult1 ), ind_dela_fin_ult1 = sum( ind_dela_fin_ult1 ), ind_ecue_fin_ult1 = sum( ind_ecue_fin_ult1 ), ind_fond_fin_ult1 = sum( ind_fond_fin_ult1 ), ind_hip_fin_ult1 = sum( ind_hip_fin_ult1 ), ind_plan_fin_ult1 = sum( ind_plan_fin_ult1 ), ind_pres_fin_ult1 = sum( ind_pres_fin_ult1 ), ind_reca_fin_ult1 = sum( ind_reca_fin_ult1 ), ind_tjcr_fin_ult1 = sum( ind_tjcr_fin_ult1 ), ind_valo_fin_ult1 = sum( ind_valo_fin_ult1 ), 
         ind_viv_fin_ult1 = sum( ind_viv_fin_ult1 ), ind_nomina_ult1 = sum( ind_nomina_ult1 ), ind_nom_pens_ult1 = sum( ind_nom_pens_ult1 ), 
         ind_recibo_ult1 = sum( ind_recibo_ult1 )) %>% 
  ungroup %>% 
  filter(ind_hip_fin_ult1 == 17) 
arrange(desc(ncodpers), fecha_dato) %>% print(n = 100)

#=============================================
# Aggregate users and cound usage of products
#=============================================

usage_matrix <- df %>% 
  group_by(ncodpers) %>%
  summarise(ind_ahor_fin_ult1 = sum( ind_ahor_fin_ult1,na.rm = T), ind_aval_fin_ult1 = sum( ind_aval_fin_ult1,na.rm = T), ind_cco_fin_ult1 = sum( ind_cco_fin_ult1,na.rm = T), ind_cder_fin_ult1 = sum( ind_cder_fin_ult1,na.rm = T), ind_cno_fin_ult1 = sum( ind_cno_fin_ult1,na.rm = T), ind_ctju_fin_ult1 = sum( ind_ctju_fin_ult1,na.rm = T), ind_ctma_fin_ult1 = sum( ind_ctma_fin_ult1,na.rm = T), ind_ctop_fin_ult1 = sum( ind_ctop_fin_ult1,na.rm = T), ind_ctpp_fin_ult1 = sum( ind_ctpp_fin_ult1,na.rm = T), ind_deco_fin_ult1 = sum( ind_deco_fin_ult1,na.rm = T), 
            ind_deme_fin_ult1 = sum( ind_deme_fin_ult1,na.rm = T), ind_dela_fin_ult1 = sum( ind_dela_fin_ult1,na.rm = T), ind_ecue_fin_ult1 = sum( ind_ecue_fin_ult1,na.rm = T), ind_fond_fin_ult1 = sum( ind_fond_fin_ult1,na.rm = T), ind_hip_fin_ult1 = sum( ind_hip_fin_ult1,na.rm = T), ind_plan_fin_ult1 = sum( ind_plan_fin_ult1,na.rm = T), ind_pres_fin_ult1 = sum( ind_pres_fin_ult1,na.rm = T), ind_reca_fin_ult1 = sum( ind_reca_fin_ult1,na.rm = T), ind_tjcr_fin_ult1 = sum( ind_tjcr_fin_ult1,na.rm = T), ind_valo_fin_ult1 = sum( ind_valo_fin_ult1,na.rm = T), 
            ind_viv_fin_ult1 = sum( ind_viv_fin_ult1,na.rm = T), ind_nomina_ult1 = sum( ind_nomina_ult1,na.rm = T), ind_nom_pens_ult1 = sum( ind_nom_pens_ult1,na.rm = T), 
            ind_recibo_ult1 = sum( ind_recibo_ult1,na.rm = T)) %>% 
  ungroup 
# %>%
#   mutate(ind_ahor_fin_ult1 = ifelse(ind_ahor_fin_ult1>0,1,0),ind_aval_fin_ult1 = ifelse(ind_aval_fin_ult1>0,1,0),ind_cco_fin_ult1 = ifelse(ind_cco_fin_ult1>0,1,0),ind_cder_fin_ult1 = ifelse(ind_cder_fin_ult1>0,1,0),ind_cno_fin_ult1 = ifelse(ind_cno_fin_ult1>0,1,0),ind_ctju_fin_ult1 = ifelse(ind_ctju_fin_ult1>0,1,0),ind_ctma_fin_ult1 = ifelse(ind_ctma_fin_ult1>0,1,0),ind_ctop_fin_ult1 = ifelse(ind_ctop_fin_ult1>0,1,0),
#          ind_ctpp_fin_ult1 = ifelse(ind_ctpp_fin_ult1>0,1,0),ind_deco_fin_ult1 = ifelse(ind_deco_fin_ult1>0,1,0),ind_deme_fin_ult1 = ifelse(ind_deme_fin_ult1>0,1,0),ind_dela_fin_ult1 = ifelse(ind_dela_fin_ult1>0,1,0),ind_ecue_fin_ult1 = ifelse(ind_ecue_fin_ult1>0,1,0),ind_fond_fin_ult1 = ifelse(ind_fond_fin_ult1>0,1,0),ind_hip_fin_ult1 = ifelse(ind_hip_fin_ult1>0,1,0),ind_plan_fin_ult1 = ifelse(ind_plan_fin_ult1>0,1,0),ind_pres_fin_ult1 = ifelse(ind_pres_fin_ult1>0,1,0),ind_reca_fin_ult1 = ifelse(ind_reca_fin_ult1>0,1,0),ind_tjcr_fin_ult1 = ifelse(ind_tjcr_fin_ult1>0,1,0),
#          ind_valo_fin_ult1 = ifelse(ind_valo_fin_ult1>0,1,0),ind_viv_fin_ult1 = ifelse(ind_viv_fin_ult1>0,1,0),ind_nomina_ult1 = ifelse(ind_nomina_ult1>0,1,0),ind_nom_pens_ult1 = ifelse(ind_nom_pens_ult1>0,1,0),ind_recibo_ult1 = ifelse(ind_recibo_ult1>0,1,0))

#build function
#names(df)[25:48] %>% paste0(.," = ifelse(",.,">0,1,0)", collapse = ",") %>% write.csv("clipboard")


usage_matrix %>% select(-ncodpers) %>%
  colSums() %>% sort


interestm <- usage_matrix %>% 
  gather(prod,rating,-ncodpers)  %>%
  mutate(rating = ifelse(rating >= 1,1,0)) %>%
  group_by(ncodpers) %>%
  mutate(prod = sum(rating)) %>%
  ungroup() %>% 
  filter(prod >= 3)
#%>% 
 # filter(rating != 0) 

user_map <-interestm %>% distinct(ncodpers) %>%
  mutate(user = 1:nrow(.))

item_map <- interestm %>% distinct(prod) %>%
  mutate(item = 1:nrow(.))

item_matrix <- interestm %>%
  left_join(user_map) %>%
  left_join(item_map)%>%
  select(user,item,rating)
  
  
#expand.grid(1:max(item_matrix))

item_matrix %>% saveRDS("./output/item_matrix.rds")


#======================================================
# Recommendation system
#======================================================

sc <- spark_connect(master = "local")

item_matrix_tbl <- copy_to(sc, item_matrix,
                           overwrite = T)

ml_als <- ml_als_factorization(item_matrix_tbl, 
                               rating.column = "rating", 
                               user.column = "user",
                               item.column = "item", 
                               rank = 5L, 
                               regularization.parameter = 0.5, 
                               iter.max = 10L)


ml_als %>% saveRDS("./output/ml_als.rds")

str(ml_als)



#======================================================
# model analysis
#======================================================


ml_als$item.factors %>% str()
ml_als$user.factors %>% str()

#recom_matrix <- 
  
  ml_als$user.factors %>% select(-id) %>%
  as.matrix() %>% .[1:100,] %*%
  (ml_als$item.factors %>% select(-id) %>%
     as.matrix() %>% .[1:13,] %>% t) %>%
    round(2)


pred_tbl <- sdf_(ml_als, 
                        newdata =  item_matrix_tbl) %>% 
  collect()


select(ind_ahor_fin_ult1:ind_recibo_ult1)  %>%
  arrange(desc(ind_tjcr_fin_ult1))






#Iris

iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)
iris_tbl
kmeans_model <- iris_tbl %>%
  select(Petal_Width, Petal_Length) %>%
  ml_kmeans(centers = 3)

# print our model fit
print(kmeans_model)

predicted <- sdf_predict(kmeans_model, iris_tbl) %>%
  collect
table(predicted$Species, predicted$prediction)



df %>% 
  group_by(ncodpers) %>%
  filter(fecha_dato == max(fecha_dato))


mutate(numprod = ind_ahor_fin_ult1+ ind_aval_fin_ult1+ ind_cco_fin_ult1+ ind_cder_fin_ult1+ ind_cno_fin_ult1+ ind_ctju_fin_ult1+ ind_ctma_fin_ult1+ ind_ctop_fin_ult1+ ind_ctpp_fin_ult1+ ind_deco_fin_ult1+ ind_deme_fin_ult1+ ind_dela_fin_ult1+ ind_ecue_fin_ult1+ ind_fond_fin_ult1+ ind_hip_fin_ult1+ ind_plan_fin_ult1+ ind_pres_fin_ult1+ ind_reca_fin_ult1+ ind_tjcr_fin_ult1+ ind_valo_fin_ult1+ ind_viv_fin_ult1+ ind_nomina_ult1+ ind_nom_pens_ult1+ ind_recibo_ult1) %>%
  #.$numprod %>% 
  group_by(numprod) %>%
  summarise(cnt = n())

min(na.rm=T)

df %>% select(ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  max(na.rm = T)

df %>% 
  gather(prod,value,ind_ahor_fin_ult1:ind_recibo_ult1)

#=================================================
# Test
#=================================================

test <- read_csv("./data/test.csv",
                 col_types = 
                   cols(
                     .default = col_character(),
                     fecha_dato = col_date(format = ""),
                     ncodpers = col_character(),
                     age = col_integer(),
                     fecha_alta = col_date(format = ""),
                     ind_nuevo = col_integer(),
                     antiguedad = col_integer(),
                     indrel = col_integer(),
                     ult_fec_cli_1t = col_date(format = ""),
                     indrel_1mes = col_double(),
                     tipodom = col_integer(),
                     cod_prov = col_integer(),
                     ind_actividad_cliente = col_integer(),
                     renta = col_double()
                   )
)

test %>% group_by(ncodpers) %>%
  summarise(cnt = n()) 

library(stringr)
test %>% 
  filter(!(str_detect(ncodpers,"e"))) %>% 
  distinct(ncodpers)

df %>%   filter(!(str_detect(ncodpers,"e"))) %>% 
  distinct(ncodpers)

ind_hip_fin_ult1

#=================================================
# Spark
#=================================================

sc <- spark_connect(master = "local")

df_tbl <- spark_read_csv(sc,name = "df_spark",path = "./data/train.csv",
                         memory = F)



