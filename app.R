# rodando os pacotes R ########
source("packages.R")

# baixando dados do tesouro direto
if(!length(list.files("TD Files")) > 0){
  source("dataApp.R")
} else{
  # Restore the object
  TD_df <- readRDS(file = "TD Files/tesouro_df.rds")
  source("functions_TD.R")
}

##############################################################+
##############################################################+
##############################################################+

tesouro_df <- TD_df %>%
  mutate(matur.date = matur_date, yield.bid = yield_bid, asset.code = asset_code,
         ref.date = ref_date, price.bid = price_bid) %>%
  mutate(TD_venc = year(matur.date),
         taxa = yield.bid*100) %>%
  group_by(tesouro, matur.date) %>%
  mutate("tx_Min %" = round(min(taxa), 2),
         "tx_Max %" = round(max(taxa), 2),
         tx_hoje = paste0(max(ref.date))) %>%
  ungroup %>%#group_by(tx_hoje) %>%
  mutate(valor_hj = ifelse(tx_hoje == ref.date, price.bid, NA)) %>%
  filter(ref.date >= today()-180) %>% #################error#########
group_by(tesouro, matur.date) %>%
  
  mutate(q2 = quantile(taxa)[2][[1]],
         mediana = quantile(taxa)[3][[1]],
         q4 = quantile(taxa)[4][[1]]) %>%
  mutate(dec_compra = case_when(
    taxa <= q2 ~ "Péssimo",
    taxa > q2 & taxa <= mediana ~ "Ruím",
    taxa > mediana & taxa <= q4 ~ "Boa",
    taxa > q4 ~ "Excelente")) %>%
  
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



plotTDdat <- TD_df %>%
  mutate(matur.date = matur_date, yield.bid = yield_bid, asset.code = asset_code,
         ref.date = ref_date, price.bid = price_bid) %>%
  mutate(TD_venc = year(matur.date),
         taxa = yield.bid*100) %>%
  group_by(tesouro, matur.date) %>%
  mutate(tx_Min = paste0(round(min(taxa), 2), "%"),
         tx_Max = paste0(round(max(taxa), 2), "%"),
         tx_hoje = paste0(max(ref.date))) %>%
  ungroup %>%#group_by(tx_hoje) %>%
  mutate(valor_hj = ifelse(tx_hoje == ref.date, price.bid, NA),
         tesouro = paste0(tesouro, TD_venc)) %>%

#function (){#%>% desativado
  group_by(tesouro, matur.date) %>%
  mutate(q2 = quantile(taxa)[2][[1]],
         mediana = quantile(taxa)[3][[1]],
         q4 = quantile(taxa)[4][[1]]) %>%
  mutate(dec_compra = case_when(
    taxa <= q2 ~ "Péssimo",
    taxa > q2 & taxa <= mediana ~ "Ruím",
    taxa > mediana & taxa <= q4 ~ "Boa",
    taxa > q4 ~ "Excelente")) %>%
  #  tesouro = paste0(tesouro, TD_venc)) %>%
  filter(ref.date >= today()-180) 
#}

plotTDdat$tesouro


# selecior period: todos, 30 dias, 90 dias, 1 ano
# selecior TD: Nome correto

#####################################################-
#####################################################-
#####################################################-
#####################################################-
source("functions_TD.R")


ui <- fluidPage(
  
  img(src = "test4.svg", height = 50, width = 1400),
  
  # App title ----
  # titlePanel("Pirata do Tesouro"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 4,
                 
                 selectizeInput(
                   inputId = "TDinp", 
                   label = "Selecione TD", 
                   choices = unique(plotTDdat %>% pull(tesouro)),
                   # c("Pre Fix. 2024", "Pre Fix. J. Sem. 2031",
                   #   "IPCA+ 2026","IPCA+ 2035","IPCA+ 2045",
                   #   "IPCA+ J. Sem. 2030","Selic 2024"),
                   selected = unique(plotTDdat %>% pull(tesouro))[1],
                   multiple = F),
                 
                 selectizeInput(
                   inputId = "quantil", 
                   label = "Selecione Quantil", 
                   choices= c("10%" = .10, 
                              "20%" = .20, 
                              "25%" = .25),
                   selected = "25%",
                   multiple = F),
                 #####################################################-
                 selectizeInput(
                   inputId = "Anosinp", 
                   label = "Selecione Periodo", 
                   choices= c("Todos" = 0, 
                              "30 dias" = 30, 
                              "90 dias" = 90,
                              "180 dias" = 180, 
                              "1 ano" = 365),
                   selected = "Todos",
                   multiple = F
                   #####################################################-
                 )),
    mainPanel(
      DT::dataTableOutput(outputId = "table1"),
      
      plotlyOutput(outputId = "p")
    )
  )
)

#choices_df$id[choices_df$names=="Name 3"]

server <- function(input, output, ...) {
  output$p <- renderPlotly({
    
    mainPlotFuncQ(df = plotTDdat, inpAno = input$Anosinp, 
                   inpTD = input$TDinp, inpquantis = input$quantil,
                   TDtype = "tesouro", refDate = "ref.date")
})
  
  #####################################################-
  output$table1 <- DT::renderDataTable({
    
    impr_form <- formatter(
      "span",style = x ~ style(
        font.weight = "bold", 
        color = ifelse(x == "Excelente", "green", 
                       ifelse(x == "Boa", "blue", 
                              ifelse(x == "Ruím", "orange",
                                     ifelse(x == "Péssimo", "red",
                                            "black"))))), 
      x ~ icontext(ifelse(x %in% c("Excelente", "Boa"), "thumbs-up", "thumbs-down"), x))
    

    tesouro_df <- bind_rows(tesouro_df[tesouro_df[["tesouro"]] == input$TDinp, ], 
                            tesouro_df[tesouro_df[["tesouro"]] != input$TDinp, ] )
    
    formattable(tesouro_df %>% dplyr::select(-dias_venc, -dias_inic), list(

      `taxa hj %` = color_bar("lightgrey"),
      #`tesouro` = impr_formTD,
      `tesouro` = formatter("span",
                                   style = x ~ style(
                                     color = ifelse(x == input$TDinp, "purple", "black"), x, 
                                    font.weight = "bold"),
                                   x ~ icontext(ifelse(x == input$TDinp, "star", ""), x)), 
      `tx_Min %` = color_bar("lightgrey"),
      `tx_Max %` = color_bar("lightgrey"),
      `mediana %` = color_bar("lightgrey"),
      `dec_compra` = impr_form)) %>%
      as.datatable(escape = FALSE,
                   options = list(scrollX = TRUE),
                   rownames = FALSE)
    
  })
}

shinyApp(ui, server)

#####################################################-
#####################################################-
#####################################################-
#####################################################-
