# ==============================================================================
# LIBRERIE NECESSARIE
# ==============================================================================

library(dplyr)   # Per la manipolazione dei dati (mutate, filter, select, join)
library(tidyr)   # Per le trasformazioni geometriche (pivot_longer, pivot_wider)
library(readxl)  # Per leggere i file Excel del clima
library(stringr) # Per manipolare le stringhe di testo (es. estrarre l'anno)
library(purrr)   # Per i cicli funzionali avanzati (map_df, reduce)
library(readr)   # Per leggere e salvare i file CSV veloci

# ==============================================================================
# PARTE 1: PULIZIA DATI CLIMATICI
# ==============================================================================

library(tidyverse)
library(readxl)

# Funzione 1: Pulisce un singolo foglio Excel convertendo anche gli NA in 0
sheet_cleaner <- function(file_path, sheet_name) {
  
  # Legge il foglio specifico dal file Excel
  df_monthly <- read_excel(file_path, sheet = sheet_name)
  
  df_annual <- df_monthly %>%
    # Trasforma le colonne degli anni in formato verticale
    pivot_longer(
      cols = -c(code, name), 
      names_to = "year_month", 
      values_to = "annual_value"
    ) %>%
    # Estrae l'anno e converte i valori mancanti (NA) in 0
    mutate(
      year = as.numeric(str_sub(year_month, 1, 4)),
      annual_value = replace_na(annual_value, 0) # <--- FIX: Sostituisce gli NA con 0
    ) %>%
    # Rimuove la vecchia colonna di testo "year_month"
    select(-year_month) %>%
    # Riordina le colonne per consistenza
    select(code, name, year, annual_value) %>%
    # Aggiunge il nome dell'indice climatico
    mutate(indicator = sheet_name)
  
  return(df_annual)
}

# Funzione 2: Applica sheet_cleaner a tutto il file, unisce i fogli e traccia il file di origine
file_cleaner <- function(file_path) {
  
  sheet_names <- excel_sheets(file_path)
  file_source <- tools::file_path_sans_ext(basename(file_path))
  
  file_data <- map_df(sheet_names, ~sheet_cleaner(file_path, .x)) %>%
    mutate(source_file = file_source)
  
  return(file_data)
}

# --- ESECUZIONE MULTI-FILE ---

path_cartella <- "../DATA/ANNUAL_RAW" 

files <- list.files(path = path_cartella, pattern = "\\.xlsx$", full.names = TRUE)

print(paste("Trovati:", length(files), "file Excel."))

df_climate_index_raw <- map_df(files, file_cleaner)


# Visualizza un'anteprima del risultato finale
print(head(df_climate_index_raw))

library(dplyr)

# 1. Calcola le medie e assegna le classi
classificazione <- df_climate_index_raw %>%
  filter(indicator == "hd30") %>%
  group_by(name) %>%
  summarise(media_hd30 = mean(annual_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    classe_clima = case_when(
      media_hd30 < 10  ~ "Freddo",
      media_hd30 >= 10  ~ "Caldo"
    )
  )

write_csv(classificazione, "../DATA/CLEAN_DATA/classificazione.csv")

df_climate_clean <- df_climate_index_raw %>%
  pivot_wider(
    names_from = indicator,        
    values_from = annual_value     
  ) %>%
  mutate(Code = toupper(code)) %>%
  rename(Year = year, Name = name) %>%
  dplyr::select(Code, Name, Year, everything(), -code) %>%

  
    
# ----------------------------------------------------------------------------
# AGGIUSTAMENTO CLIMATICO: Finestra Espandibile Sfasata (Lagged Expanding Window)
# ----------------------------------------------------------------------------
group_by(Code) %>%
  arrange(Year) %>% # Fondamentale ordinare cronologicamente!
  mutate(
    # 1. Calcoliamo la media di TUTTI gli anni passati dall'inizio fino a t-1
    hd30_normale  = lag(cummean(hd30)),
    cdd_normale   = lag(cummean(cdd)),
    r20mm_normale = lag(cummean(r20mm)),
    
    # 2. Lo shock è la differenza rispetto alla storia accumulata ex-ante
    hd30_shock  = hd30 - hd30_normale,
    cdd_shock   = cdd - cdd_normale,
    r20mm_shock = r20mm - r20mm_normale
  ) %>%
  ungroup() %>%
  
  # Teniamo le anomalie. Il primo anno (1950) sarà automaticamente NA e viene rimosso
  dplyr::select(Code, Name, Year, hd30 = hd30_shock, cdd = cdd_shock, r20mm = r20mm_shock) %>%
  filter(!is.na(hd30))
# ==============================================================================
# PARTE 2: PULIZIA DATI ECONOMICI (PWT & GMD)
# ==============================================================================

# Lista ufficiale dei 41 paesi target (Manteniamo i nomi per un primo filtro, 
# ma useremo i codici ISO per i join successivi per evitare errori di battitura)
countries_list <- c(
  "Algeria", "Argentina", "Australia", "Bangladesh", "Brazil", "Cambodia", 
  "Canada", "China", "Colombia", "Ecuador", "Arab Republic of Egypt", "Ethiopia", 
  "France", "Germany", "India", "Indonesia", "Islamic Republic of Iran", "Italy", 
  "Japan", "Kenya", "Republic of Korea", "Malaysia", "Mexico", "Morocco", "Nigeria", 
  "Pakistan", "Peru", "Philippines", "Russian Federation", "Saudi Arabia", 
  "Senegal", "South Africa", "Spain", "Sweden", "Thailand", "Türkiye",
  "United Kingdom", "United Arab Emirates", "United States", "R. B. de Venezuela", 
  "Viet Nam"
)
# Crea la lista dei codici ISO corrispondenti ai tuoi 41 file
iso_target <- c(
  "DZA", "ARG", "AUS", "BGD", "BRA", "KHM", "CAN", "CHN", "COL", "ECU", 
  "EGY", "ETH", "FRA", "DEU", "IND", "IDN", "IRN", "ITA", "JPN", "KEN", 
  "KOR", "MYS", "MEX", "MAR", "NGA", "PAK", "PER", "PHL", "RUS", "SAU", 
  "SEN", "ZAF", "ESP", "SWE", "THA", "TUR", "ARE", "GBR", "USA", "VEN", "VNM"
)

# 1. ESTRAZIONE DAL PENN WORLD TABLE
df_pwt <- read_excel("../DATA/ECONOMIC_RAW_DATA/pwt110.xlsx", sheet = "Data") %>%
  dplyr::select(
    Code = countrycode, # Questo è l'ISO (es. "ARG", "EGY")
    Name = country,     
    Year = year,     
    GDP = rgdpna,     
    emp = emp,  
    rnna = rnna,
    hc = hc
  ) %>%
  # FIX: Filtriamo per codice ISO, non per nome stringa!
  filter(Code %in% iso_target)

# 2. ESTRAZIONE DAL GLOBAL MACROECONOMIC DATABASE (GMD.csv)
# Variabili: Short-term rate (strate), Central Bank rate (cbrate)
df_gmd <- read_csv("../DATA/ECONOMIC_RAW_DATA/GMD.csv") %>%
  dplyr::select(
    Code = ISO3, 
    # Non importiamo il nome del paese per evitare conflitti (es. Name.x e Name.y)
    Year = year, 
    infl = infl
  )

# 3. UNIONE DEI DUE DATASET ECONOMICI
# Facciamo il join rigorosamente tramite Codice ISO e Anno
df_economy_clean <- df_pwt %>%
  left_join(df_gmd, by = c("Code", "Year"))

# ==============================================================================
# PARTE 3: JOIN FINALE DEI DUE DATASET (ECONOMIA + CLIMA)
# ==============================================================================

# Unisce l'economia al clima usando il codice ISO e l'anno
df_final <- df_economy_clean %>%
  left_join(df_climate_clean, by = c("Code", "Year")) %>%
  # Rimuoviamo l'eventuale colonna Name proveniente dal file del clima
  dplyr::select(-any_of("Name.y")) %>%
  # Se il nome economia è diventato Name.x, lo sistemiamo (se necessario)
  rename_with(~"Name", any_of("Name.x")) %>%
  dplyr::select(Code, Name, Year, everything()) %>%
  # Tagliamo fuori i dati post-2024
  filter(Year < 2025)

write_csv(df_final, "../DATA/CLEAN_DATA/Final_Panel_Data.csv")
# ==============================================================================
# PARTE 4: CONTROLLI DI QUALITÀ FINALI
# ==============================================================================

# Conta quanti anni di dati sono disponibili per ogni singola nazione
anni_per_nazione <- df_final %>%
  count(Name, name = "anni_totali") %>%
  arrange(Name) 
print(anni_per_nazione, n = Inf) 

# Conta quante nazioni sono presenti per ogni singolo anno
nazioni_per_anno <- df_final %>%
  count(Year, name = "nazioni_totali") %>%
  arrange(Year) # Ordine cronologico dal 1960 in poi
print(nazioni_per_anno, n = Inf)

