



filterFunc <- function(df, coldate, fltr){
  coldate <- rlang::sym(coldate)
  if(fltr == 0){ 
    df <- df
  } else if(fltr == 30){
    df <- df %>% filter(!!coldate >= today()-30)
  } else if(fltr == 90){
    df <- df %>% filter(!!coldate >= today()-90)
  } else if(fltr == 180){
    df <- df %>% filter(!!coldate >= today()-180)
  }else{df <- df %>% filter(!!coldate >= today()-365)
  }
  return(df)
}

# mainPlotFuncQ(df = plotTDdat, inpAno = 90, inpTD = "Pre Fix. 20292029", inpquantis = .20, TDtype = "tesouro", refDate = "ref.date")

mainPlotFuncQ <- function(df, inpAno, inpTD, 
                           inpquantis, TDtype, refDate){
  ######################-
  TDtype0 <- rlang::sym(TDtype)
  refDate0 <- rlang::sym(refDate)
  ######################-
plotout <- filterFunc(df, coldate = refDate, fltr = as.numeric(inpAno)) %>%
  filter(!!TDtype0 == inpTD) %>%
  ggplot(aes(x = !!refDate0, y = taxa, color = TDtype)) +
  scale_fill_brewer(palette="Dark2")+
  scale_color_brewer(palette="Dark2")+
  geom_line(size = .7) +
  geom_point(size = 1.2)
  ######################-
for(i in 1:((1/as.numeric(inpquantis))+1)){
  ######################-
  plotout <- plotout + geom_hline(
    yintercept = df %>% 
      filter(!!TDtype0 == inpTD & !!refDate0 >= today()-180) %>%
      pull(taxa) %>% 
      quantile(., seq(0,1, by = as.numeric(inpquantis))) %>%
      unname %>% 
      nth(., i),
    linetype="dashed", color = "gray", size=.4, alpha = .3) +
    ######################-
    annotate("text", 
             x = today()-as.numeric(inpAno),
             y = df %>% 
               filter(!!TDtype0 == inpTD & !!refDate0 >= today()-180) %>%
               pull(taxa) %>% 
               quantile(., seq(0,1, by = as.numeric(inpquantis))) %>%
               unname %>% 
               nth(., i),
             label = df %>% 
               filter(!!TDtype0 == inpTD & !!refDate0 >= today()-180) %>%
               pull(taxa) %>% 
               quantile(., seq(0,1, by = as.numeric(inpquantis))) %>%
               names %>% 
               nth(., i),
             color = "gray", size = 3)
}
######################-
if(inpAno == 0){
  plotout <- plotout + theme_bw()
  } else{
    plotout <- plotout + 
    ylim(
      min(df %>% 
            filter(!!TDtype0 == inpTD & 
                     !!refDate0 >= today()- as.numeric(inpAno))%>% 
            pull(taxa)),
      max(df %>% 
            filter(!!TDtype0 == inpTD & 
                     !!refDate0 >= today()- as.numeric(inpAno))%>% 
            pull(taxa))) +
    theme_bw()
}
######################-
return(plotout)
}

