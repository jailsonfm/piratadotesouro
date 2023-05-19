
# Baixar dados -----
dl_folder <- "TD Files"
# Tesouro Prefixado
LTN_T_Pref <- td_get( 
  asset_codes = "LTN",
  first_year = 2005,
  last_year = as.numeric(format(Sys.Date(), "%Y")),
  dl_folder = dl_folder) %>%
  mutate(tesouro = rep("Pre Fix. ", nrow(.))) %>%
 # filter(grepl(c("2024|2026"), matur.date)) %>% 
  tibble()

# Tesouro Prefixado Juros Semestrais
LTN_T_Pref_J_Sem <- td_get( 
  asset_codes = "NTN-F",
  first_year = 2005,
  last_year = as.numeric(format(Sys.Date(), "%Y")),
  dl_folder = dl_folder) %>%
  mutate(tesouro = rep("Pre Fix. J. Sem. ", nrow(.))) %>%
  # filter(grepl("2031", matur.date)) %>% 
  tibble()

# Tesouro IPCA +
NTNB_IPCA_Princ <- td_get( 
  asset_codes = "NTN-B Principal",
  first_year = 2005,
  last_year = as.numeric(format(Sys.Date(), "%Y")),
  dl_folder = dl_folder) %>%
  mutate(tesouro = rep("IPCA+ ", nrow(.))) %>%
  # filter(grepl("2031", matur.date)) %>% 
  tibble()

# Tesouro IPCA Juros Semestrais
NTNB_IPCA_J_Sem <- td_get( 
  asset_codes = "NTN-B",
  first_year = 2005,
  last_year = as.numeric(format(Sys.Date(), "%Y")),
  dl_folder = dl_folder) %>%
  mutate(tesouro = rep("IPCA S. Sem. ", nrow(.))) %>%
  # filter(grepl(c("2030|2040|2055"), matur.date)) %>%
  tibble()

# Tesouro SELIC
LFT_selic <- td_get( 
  asset_codes = "LFT",
  first_year = 2005,
  last_year = as.numeric(format(Sys.Date(), "%Y")),
  dl_folder = dl_folder) %>%
  mutate(tesouro = rep("Selic ", nrow(.))) %>%
  # filter(grepl(c("2024|2027"), matur.date)) %>%
  tibble()


tesouro_df <- bind_rows(LTN_T_Pref, LTN_T_Pref_J_Sem, 
                        NTNB_IPCA_Princ, 
                        NTNB_IPCA_J_Sem, LFT_selic) 

# Save an object to a file
saveRDS(tesouro_df, file = "TD files/tesouro_df.rds")
