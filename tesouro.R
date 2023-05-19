# Silga Tesouros ----
# https://www.tradingcomdados.com/8-blog/34-analisando-dados-do-tesouro-direto-com-r

# "LFT" ••• Tesouro Selic
# "NTN-B" ••• Tesouro IPCA + com Juros Semestrais
# "NTN-B PRINCIPAL" ••• Tesouro IPCA +
# "NTN-F" ••• Tesouro Prefixado com Juros Semestrais
# "LTN" ••• Tesouro Prefixado

# Pacotes R ---- 
#Packages list
pkgList <- c("tidyverse", "GetTDData", "lubridate",
             "extrafont", "ggrepel", "ggthemes", "plotly",
             "formattable", "data.table", "sidrar")
# Install packages if not already installed
newPkgs <- pkgList[!(pkgList %in% 
                       installed.packages()[,"Package"])]
if(length(newPkgs)) install.packages(newPkgs)
# Loading packages if not already loaded
lapply(pkgList, require, character.only = TRUE)
font_import()
loadfonts(device = "win")
custom_font <- "Gadugi" 

# Baixar dados -----

LTN_T_Pref %>%
  filter(grepl(c("2024|2026"), matur.date)) %>% tibble() %>%
  pull(matur.date) %>% unique

# Tesouro Prefixado
  download.TD.data(asset.codes = "LTN") #2024,2026
  LTN_T_Pref <- read.TD.files(asset.codes = "LTN") %>% 
    mutate(tesouro = rep("Pre Fix. ", nrow(.))) %>%
    filter(grepl(c("2024|2026"), matur.date)) %>% tibble()

# Tesouro Prefixado Juros Semestrais
  download.TD.data(asset.codes = "NTN-F") # 2031
  LTN_T_Pref_J_Sem <- read.TD.files(asset.codes = "NTN-F") %>% 
    mutate(tesouro = rep("Pre Fix. J. Sem. ", nrow(.))) %>%
    filter(grepl("2031", matur.date)) %>% tibble()

  # Tesouro IPCA +
  download.TD.data(asset.codes = "NTN-B Principal") # 2026, 2035, 2045
  NTNB_IPCA_Princ <- read.TD.files(asset.codes = "NTN-B Principal") %>%
    mutate(tesouro = rep("IPCA+ ", nrow(.))) %>%
  filter(grepl(c("2026|2035|2045"), matur.date)) %>% tibble()

  
  # Tesouro IPCA Juros Semestrais
  download.TD.data(asset.codes = "NTN-B") # 2030, 2040, 2055
  NTNB_IPCA_J_Sem <- read.TD.files(asset.codes = "NTN-B") %>% 
    mutate(tesouro = rep("IPCA+ J. Sem. ", nrow(.))) %>% 
    filter(grepl(c("2030|2040|2055"), matur.date)) %>% tibble()
  
# Tesouro Selic
  download.TD.data(asset.codes = "LFT") #2024, 2027
  LFT_selic <- read.TD.files(asset.codes = "LFT") %>% 
    mutate(tesouro = rep("Selic ", nrow(.))) %>% 
    filter(grepl(c("2024|2027"), matur.date)) %>% tibble()
  

  

tesouro_df <- bind_rows(LTN_T_Pref, LTN_T_Pref_J_Sem, 
                        NTNB_IPCA_Princ, 
                        NTNB_IPCA_J_Sem, LFT_selic) %>%
  mutate(TD_venc = year(matur.date),
         taxa = yield.bid*100) %>%
  group_by(tesouro, matur.date) %>%
  mutate("tx_Min %" = round(min(taxa), 2),
         "tx_Max %" = round(max(taxa), 2),
         tx_hoje = paste0(max(ref.date))) %>%
  ungroup %>%#group_by(tx_hoje) %>%
  mutate(valor_hj = ifelse(tx_hoje == ref.date, price.bid, NA)) %>%
  group_by(tesouro, matur.date) %>%
  mutate(q2 = quantile(taxa)[2][[1]],
         mediana = quantile(taxa)[3][[1]],
         q4 = quantile(taxa)[4][[1]]) %>%
  mutate(dec_compra = case_when(
    taxa <= q2 ~ "Péssimo",
    taxa > q2 & taxa <= mediana ~ "Ruím",
    taxa > mediana & taxa <= q4 ~ "Boa",
    taxa > q4 ~ "Comprar")) %>%
  filter(ref.date >= today()-180) %>%
  mutate(dias_venc = difftime(matur.date,
                              max(ref.date), units = c("days")),
         dias_inic = difftime(min(ref.date),
                              max(ref.date), units = c("days")),
         "taxa hj %" = round(taxa, 2),
         "mediana %" = round(mediana, 2),
         tesouro = paste0(tesouro, TD_venc)) %>% ungroup %>%
  dplyr::select(-ref.date, -yield.bid, -price.bid, -tx_hoje, -TD_venc,
                -asset.code, -matur.date, -q2, -q4, -taxa, -mediana) %>%
  na.omit()

#forestgreen, honeydew4, goldenrod1, firebrick2

impr_form <- formatter(
  "span",style = x ~ style(
    font.weight = "bold", 
    color = ifelse(x == "Comprar", "green", 
                   ifelse(x == "Boa", "blue", 
                          ifelse(x == "Ruím", "orange",
                                 ifelse(x == "Péssimo", "red",
                                        "black"))))), 
  x ~ icontext(ifelse(x == "Excelente"|x == "Boa", "thumbs-up", "thumbs-down"), x))

formattable(tesouro_df %>% dplyr::select(-dias_venc, -dias_inic), list(
  `tesouro` = formatter("span",
                        style = x ~ style(color = "black", font.weight = "bold")),
  `taxa hj %` = color_bar("lightgrey"),
  `tx_Min %` = color_bar("lightgrey"),
  `tx_Max %` = color_bar("lightgrey"),
  `mediana %` = color_bar("lightgrey"),
  `dec_compra` = impr_form)) %>%
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE),
               rownames = FALSE)





A = data.frame(X="x", Y="y", 'P/E%'="z", check.names=FALSE)
gg <- tibble(frt = 1:5, "send%" = 11:15) %>%
  mutate("ggsgs%" = frt)

#1. Catástrofe (Vermelho)
#2. Horrível
#3. Péssimo
#4. Ruim
#5. Pensando
#6. regular (Amarelo)
#7. Bom
#8. Muito Bom
#9. Excelente
# 10. Considere compra (Verde)

#https://www.littlemissdata.com/blog/prettytables
  

#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk


# Função para checar janela de compras do Tesouro
tesOURO_ref <- function(datfr, year_inp, qndo = "hoje", qualTes = "tesouro?"){
#---------------------------------#
  # taxa hoje ou ontem
  if(qndo == "hoje"){qndox <- 1} else if(qndo == "ontem"){qndox <- 2}
  taxaHoje <- datfr %>%
    # formato de data para variaveis
  mutate(matur.date = as.Date(matur.date, format= "%Y-%m-%d"),
         ref.date = as.Date(ref.date, format= "%Y-%m-%d")) %>%
    # selec. ano vencimento - arg "year_inp" (só ano)
  dplyr::filter(grepl(year_inp, matur.date)) %>%
    # taxa em porcento (%)
  mutate(taxa = yield.bid*100) %>%
    # col ref a data de atualiz. em orden decresc.
  arrange(desc(ref.date)) %>% 
    # pega só a 1a linha (hoje)
    slice(qndox) %>% pull(taxa)
  #---------------------------------#
  dfInp <- datfr %>%
    # selec. ano vencimento - arg "year_inp" (só ano)
    dplyr::filter(grepl(year_inp, matur.date)) %>%
    # formato de data para variaveis
    mutate(matur.date = as.Date(matur.date, format= "%Y-%m-%d"),
           ref.date = as.Date(ref.date, format= "%Y-%m-%d")) %>%
    # taxa em porcento (%)
  mutate(taxa = yield.bid*100)
  #---------------------------------#
# parte do texto de saida
txtout <- paste0(qualTes, " ", year_inp, "(", taxaHoje, "%) •")
# 2ndo, 3iro (mediana) e 3 4rto quartil das taxas
q1 <- quantile(dfInp[, "taxa"][[1]])[1][[1]]
q2 <- quantile(dfInp[, "taxa"][[1]])[2][[1]]
q3 <- quantile(dfInp[, "taxa"][[1]])[3][[1]]
q4 <- quantile(dfInp[, "taxa"][[1]])[4][[1]]
q5 <- quantile(dfInp[, "taxa"][[1]])[5][[1]]
#---------------------------------#
if(taxaHoje < q2){ # < 25% do hist. taxas
  output <- paste(txtout, "Compra Péssima", "min:", q1, "max:",  q5)
  # > 25% e < 50% (mediana) do hist. taxas
} else if(taxaHoje > q2 & taxaHoje < q3){
  output <- paste(txtout, "Compra Ruim", "min:", q1, "max:",  q5)
  # > 50% (mediana) e < 75% do hist. taxas
} else if(taxaHoje > q3 & taxaHoje < q4){
  output <- paste(txtout, "Compra Boa", "min:", q1, "max:",  q5)
} else if(taxaHoje > q4 ){# > 75% do hist. taxas
  output <- paste(txtout, "Compra Excelente", "min:", q1, "max:",  q5)}
# informação final
return(output)
}

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

tesOURO_ref(LTN_T_Pref, year_inp = "2024", qndo = "hoje", qualTes = "Tesouro Prefixado")
tesOURO_ref(LTN_T_Pref, year_inp = "2026", qndo = "hoje", qualTes = "Tesouro Prefixado")
#------------------------------------------#
tesOURO_ref(LTN_T_Pref_J_Sem, year_inp = "2031", qndo = "hoje", qualTes = "Tes. Prefix. Jur. Semest.")
#------------------------------------------#
tesOURO_ref(NTNB_IPCA_Princ, year_inp = "2026", qndo = "hoje", qualTes = "Tesouro IPCA Princ.")
tesOURO_ref(NTNB_IPCA_Princ, year_inp = "2035", qndo = "hoje", qualTes = "Tesouro IPCA Princ.")
tesOURO_ref(NTNB_IPCA_Princ, year_inp = "2045", qndo = "hoje", qualTes = "Tesouro IPCA Princ.")
#------------------------------------------#
tesOURO_ref(NTNB_IPCA_J_Sem, year_inp = "2030", qndo = "hoje", qualTes = "Tes. IPCA J. Semest.")
tesOURO_ref(NTNB_IPCA_J_Sem, year_inp = "2040", qndo = "hoje", qualTes = "Tes. IPCA J. Semest.")
tesOURO_ref(NTNB_IPCA_J_Sem, year_inp = "2055", qndo = "hoje", qualTes = "Tes. IPCA J. Semest.")
#------------------------------------------#
tesOURO_ref(LFT_selic, year_inp = "2024", qndo = "hoje", qualTes = "Tesouro Selic")
tesOURO_ref(LFT_selic, year_inp = "2027", qndo = "hoje", qualTes = "Tesouro Selic")






# PLOTTING

# filter bonds by maturity
venc_ntnbs <- as.Date(c("2020-08-15", "2024-08-15", 
                        "2035-05-15", "2050-08-15"),
                      format= "%Y-%m-%d")
plotNTNB <- NTNB_IPCA_Princ %>%
  mutate(matur.date = as.Date(matur.date, format= "%Y-%m-%d"),
         ref.date = as.Date(ref.date, format= "%Y-%m-%d")) %>%
  dplyr::filter(matur.date %in% venc_ntnbs,
                ref.date > as.Date("2019-08-01"),
                !str_detect(asset.code, "Principal")) %>%
  mutate(taxa = yield.bid*100,
         NTN_B = format(matur.date, "%Y/%m"),
         label = if_else(ref.date == min(ref.date),
                         format(matur.date, "%Y"),
                         NA_character_)) %>%
  arrange(NTN_B, desc(ref.date), .by_group = TRUE) %>%
  dplyr::select(-c(yield.bid, asset.code, price.bid)) %>%
  ggplot(aes(x = ref.date, y = taxa, color = NTN_B)) +
  geom_line(size = .5) +
  theme_bw()

ggplotly(plotNTNB)

tesouro_df1 <- bind_rows(LTN_T_Pref, LTN_T_Pref_J_Sem, 
                        NTNB_IPCA_Princ, 
                        NTNB_IPCA_J_Sem, LFT_selic) %>%
  mutate(TD_venc = year(matur.date),
         taxa = yield.bid*100) %>%
  group_by(tesouro, matur.date) %>%
  mutate(tx_Min = paste0(round(min(taxa), 2), "%"),
         tx_Max = paste0(round(max(taxa), 2), "%"),
         tx_hoje = paste0(max(ref.date))) %>%
  ungroup %>%#group_by(tx_hoje) %>%
  mutate(valor_hj = ifelse(tx_hoje == ref.date, price.bid, NA)) %>%
  group_by(tesouro, matur.date) %>%
  mutate(q2 = quantile(taxa)[2][[1]],
         mediana = quantile(taxa)[3][[1]],
         q4 = quantile(taxa)[4][[1]]) %>%
  mutate(dec_compra = case_when(
    taxa <= q2 ~ "Péssimo",
    taxa > q2 & taxa <= mediana ~ "Ruím",
    taxa > mediana & taxa <= q4 ~ "Boa",
    taxa > q4 ~ "Excelente")) 

tesouro_df2 <- tesouro_df1 %>% 
  mutate(TD = paste0(tesouro, "•", TD_venc)) %>%
  filter(TD == "TD_IPCA•2035" & ref.date >= today()-180) %>%
  ggplot(aes(x = ref.date, y = taxa, color = TD)) +
  geom_line(size = 1.0) +
  geom_point(aes(color = TD), size = 1.5) +
  theme_bw()


ggplotly(tesouro_df2)



# ANOTAÇÕES GERAIS
#1) O tempo de existência do tesouro influencia na avaliação...
#... para marcação a mercado, baseado no variação das taxas e preçoes?

#2) Tables R - #https://www.littlemissdata.com/blog/prettytables

# Preço Tesouro: 1000/((1+taxa)^(DU/252))
# Incluir mensagem se o max for igual a taxa do dia
# http://tdireto.com/index.php
# quantile(0:100,  probs = seq(0,100, by=10)/100)
# Se as taxas variarem em uma só direção três dias consecutivos indicar alerta.
#...ou se variar mais de 20% da taxa atual ou maxima alerta. Sei la
#.. se vender com prejuizo não há IR
# Info IOF e IR TD - https://www.xpi.com.br/investimentos/renda-fixa/titulos-publicos-federais/


#-------------------------------------------------------------.

#############################################################-
#############################################################-
#############################################################-

tibdat <- tibble( TD = rep(c("PreFix•2024", "PreF-JuS•2024", "IPCA•2026", 
                             "IPCA•2035", "IPCA•2045", "IPCA-JuS•2030", "Selic•2024"),50), 
                  dateR =  rep(seq(today()-49, today() ,by = 1), 7), values = sample(1:10, 350, replace = T))
# função teste para selecionar o grafico de tesouro filtrando por periodo e ano
filtTD <- function(df, colTDs, filt = "todos", datper = 30, col_data){
  #-------------------------------------------------------------.
  colTDs <- rlang::sym(colTDs);   col_data <- rlang::sym(col_data)
  #-------------------------------------------------------------.
  if(datper == "todos"){ # Se filt for igual a "todos" gerar dados de todos os DTs
    df <- df
  } else if(datper == "30 dias"){
    df <- df %>% filter(!!col_data >= today()-30)
  } else if(datper == "90 dias"){
    df <- df %>% filter(!!col_data >= today()-90)
  }else if(datper == "1 ano"){
    df <- df %>% filter(!!col_data >= today()-365)
  }
  #-------------------------------------------------------------.
  if(filt == "Todos"){ # Se filt for igual a "todos" gerar dados de todos os DTs
    selecTD <- df[, colTDs][[1]] %>% unique
  } else {selecTD <- filt} # Se filt for outra coisa predefinida entao o filtro sera esse
  #-------------------------------------------------------------.
  output <- df %>% filter(!!colTDs %in% selecTD) # filtrando a partir dos argumentos acima
  return(output)
}

# selecior period: todos, 30 dias, 90 dias, 1 ano
# selecior TD: Nome correto

filtTD(tibdat, colTDs = "TD", filt = "IPCA•2045", col_data = "dateR", datper = "90 dias") %>%
  ggplot(aes(x = dateR, y = values, color = TD)) +
  geom_line(size = 1.5)+
  theme_bw()
  
tibdat %>%
  filter(dateR >= today()- (today() - min(dateR))[[1]])

ggg <-(today() - min(tibdat$dateR))[[1]]
filtTD(tibdat, colTDs = "TD")

tibdat %>%
  filter_if()

filterFunc <- function(df, coldate, fltr){
  coldate <- rlang::sym(coldate)
if(fltr == "Todos"){ 
  df <- df
} else if(fltr == "30 dias"){
  df <- df %>% filter(!!coldate >= today()-30)
} else if(fltr == "90 dias"){
  df <- df %>% filter(!!coldate >= today()-90)
} else{df <- df %>% filter(!!coldate >= today()-365)
}
  return(df)
}

filterFunc(tibdat, coldate = "dateR", fltr = "30 dias") %>%
  filter(TD == )


#############################################################-
#############################################################-
#############################################################-


quantile(tsttt$taxa)[[2]]
tesouro_df1 %>%
  filter(tesouro == "IPCA+ 2035") 

tsttt <- tesouro_df1 %>%
  filter(tesouro == "IPCA+ 2035" &
           ref.date >= today()-180) 
ttre <- tesouro_df1 %>%
  filter(tesouro == "IPCA+ 2035" &
           ref.date >= today()-90) %>%
  ######################-
  ggplot(aes(x =ref.date, y = taxa, color = tesouro))+
  geom_line(size = 1.0) +
  geom_point(size = 1.5) +
  ######################-
  geom_hline(data=tesouro_df1 %>% 
             filter(tesouro == "IPCA+ 2035" & 
                      ref.date >= today()-180) , 
           aes(yintercept= quantile(taxa)[[2]]), 
           linetype="dashed", color = "red", size=.3) +
  geom_label(data=tesouro_df1 %>% 
              filter(tesouro == "IPCA+ 2035" & 
                       ref.date >= today()-180),
              aes(x = today()-90, y =  quantile(taxa)[[2]]), 
            color = "gray", label = "25%") +
  ######################-
  geom_hline(data=tesouro_df1 %>% 
             filter(tesouro == "IPCA+ 2035" & 
                      ref.date >= today()-180) , 
           aes(yintercept= quantile(taxa)[[3]]), 
           linetype="dashed", color = "red", size=.3)+
  geom_label(data=tesouro_df1 %>% 
               filter(tesouro == "IPCA+ 2035" & 
                        ref.date >= today()-180),
             aes(x = today()-90, y =  quantile(taxa)[[3]]), 
             color = "gray", label = "50%") +
  ######################-
  geom_hline(data=tesouro_df1 %>% 
               filter(tesouro == "IPCA+ 2035" & 
                        ref.date >= today()-180) , 
             aes(yintercept= quantile(taxa)[[4]]), 
             linetype="dashed", color = "red", size=.3)+
  geom_label(data=tesouro_df1 %>% 
               filter(tesouro == "IPCA+ 2035" & 
                        ref.date >= today()-180),
             aes(x = today()-90, y =  quantile(taxa)[[4]]), 
             color = "gray", label = "75%") +
  ######################-
  ttre + ylim(min(tesouro_df1 %>%
             filter(tesouro == "IPCA+ 2035" &
                      ref.date >= today()-90)%>%
             pull(taxa)), 
       max(tesouro_df1 %>%
             filter(tesouro == "IPCA+ 2035" &
                      ref.date >= today()-90)%>%
             pull(taxa)))+

  theme_bw()
  





# load necessary package
require(ggplot2)
# create sample data
dfp <- data.frame(matrix(rnorm(147), nrow=21))
# original plot (removed unnecessary call to y)
p <- ggplot(data=dfp, aes(x=seq(0,20,1)))
# loop
for (i in 1:7) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = names(dfp)[i]))
}
# print the result
print(p)





jai <- tesouro_df1 %>%
  filter(tesouro == "IPCA+ 2035" &
           ref.date >= today()-180) %>%
  ggplot(aes(x =ref.date, y = taxa, color = tesouro))
for(i in 1:5){
  jai <- jai + 
    geom_hline(data=tesouro_df1 %>% 
                 filter(tesouro == "IPCA+ 2035" & 
                          ref.date >= today()-180) , 
               aes(yintercept= quantile(taxa)[[i]]), 
               linetype="dashed", color = "red", size=.3)+
    geom_label(data=tesouro_df1 %>% 
                 filter(tesouro == "IPCA+ 2035" & 
                          ref.date >= today()-180),
               aes(x = today()-180, y =  quantile(taxa)[[i]]), 
               color = "gray", label = "50%") 
  
}
jai+
geom_line(size = 1.0) +
  geom_point(size = 1.5)


# load necessary package
require(ggplot2)
# create sample data
dfp <- data.frame(matrix(rnorm(147), nrow=21))
# original plot (removed unnecessary call to y)
p <- ggplot(data=dfp, aes(x=seq(0,20,1)))
# loop
for (i in 1:7) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = names(dfp)[i]))
}
# print the result
print(p)
