# ==============================================================================
# LIBRERIE NECESSARIE
# ==============================================================================
library(plm)         # Per modelli panel data (effetti fissi/random)
library(readr)       # Per leggere/scrivere CSV velocemente
library(dplyr)       # Per manipolazione dati (filter, mutate, select, group_by)
library(lmtest)      # Per test diagnostici sui modelli
library(whitestrap)  # Per test di White (eteroschedasticità)
library(missRanger)  # Per l'imputazione dei dati mancanti tramite Random Forest

# ==============================================================================
# 1. CARICAMENTO DATI E VERIFICA DATI MANCANTI (NA)
# ==============================================================================
df <- read_csv("../DATA/CLEAN_DATA/Final_Panel_Data.csv")
classificazione<- read_csv("../DATA/CLEAN_DATA/classificazione.csv")

lista_caldi   <- classificazione %>% filter(classe_clima == "Caldo")  %>% pull(name)
lista_freddi  <- classificazione %>% filter(classe_clima == "Freddo") %>% pull(name)

df <- df %>%
  filter(Year >= 1960)


# Calcolo dei NA focalizzato sulle sole variabili di interesse (escludendo Code e Year)
countries_NA <- df %>%
  group_by(Name) %>%
  summarise(across(-c(Code, Year), ~sum(is.na(.))), .groups = "drop") %>%
  arrange(Name)

print("--- (NA) per ogni variabile e Paese ---")
print(countries_NA, n = Inf)

paesi_da_scartare <- c("Russian Federation" ,"Cambodia", "Saudi Arabia", "United Arab Emirates", "Viet Nam", "Ethiopia")

df <- df %>%
  filter(!Name %in% paesi_da_scartare)

# Calcolo dei NA focalizzato sulle sole variabili di interesse (escludendo Code e Year)
countries_NA <- df %>%
  group_by(Name) %>%
  summarise(across(-c(Code, Year), ~sum(is.na(.))), .groups = "drop") %>%
  arrange(Name)

print("--- (NA) per ogni variabile e Paese ---")
print(countries_NA, n = Inf)

# ==============================================================================
# 3. CARICAMENTO E UNIONE AUGMENTED HUMAN DEVELOPMENT INDEX (AHDI)
# ==============================================================================
# L'AHDI è già in formato lungo. Lo leggiamo e rinominiamo le colonne necessarie.

path_economic <- "../DATA/ECONOMIC_RAW_DATA"
file_ahdi <- file.path(path_economic, "AHDI.csv")
df_ahdi <- read_csv(file_ahdi, col_types = cols(`Augmented Human Development Index (AHDI) (Annotations)` = col_character()))

lista_codici_iso3 <- unique(df$Code)

df_ahdi <- df_ahdi %>%
  select(Name = 1, Code = 2, Year = 3, ahdi = 4) %>%
  mutate(
    Year = as.numeric(Year),
    Code = toupper(Code) # Standardizziamo in maiuscolo per evitare discrepanze
  ) %>%
  # Sostituiamo il filtro sui nomi con il filtro PULITO sui codici ISO3
  filter(Code %in% lista_codici_iso3) %>%
  select(Code, Name, Year, ahdi)

ahdi_mapped <- df_ahdi %>%
  filter(Year >= 1960 & Year <= 2023) %>% 
  group_by(Code) %>% # Raggruppiamo per Codice, più sicuro del nome!
  summarise(
    Name = first(Name), # Ci teniamo il nome associato al codice per la visualizzazione
    AHDI_avg = mean(ahdi, na.rm = TRUE),
    n_obs   = sum(!is.na(ahdi)),
    .groups = "drop"
  ) %>%
  mutate(
    Label_HDI = if_else(AHDI_avg > 0.45, "Developed", "Developing")
  ) %>%
  arrange(desc(AHDI_avg))

print(as.data.frame(ahdi_mapped))

# ==============================================================================
# INNESTO: UNIONE CLASSIFICAZIONE CLIMATICA E CREAZIONE SOTTO-DATASET
# ==============================================================================

# 1. Prepariamo il dataframe delle etichette di sviluppo (Developed/Developing)
df_labels_sviluppo <- ahdi_mapped %>% 
  select(Code, Label_HDI)

# 2. Prepariamo il tuo dataframe "classificazione" climatica.
# Assumiamo che la colonna "classe_clima" (o classe_hd30) contenga i valori "Caldo" e "Freddo" 
# (o comunque le stringhe che identificano il clima). 
# NOTA: Standardizziamo il Nome in maiuscolo se necessario, o usiamo un left_join sul testo.
df_classificazione_clima <- classificazione %>%
  select(name, classe_clima) 

# 3. Uniamo TUTTE le classificazioni (Sviluppo + Clima) al dataset principale pulito.
# NOTA: Sostituisci "df_imputed_clean" o il nome del tuo dataset globale consolidato 
# prima dei filtri (nel tuo script finale potrebbe chiamarsi df_full prima del salvataggio).
df_master_classificato <- df %>%
  left_join(df_labels_sviluppo, by = "Code") %>%
  left_join(df_classificazione_clima, by = c("Name" = "name"))

# 4. Creazione dei dataset principali separati solo per Sviluppo (Ricchi vs Poveri)
# Usiamo i termini Rich/Poor per coerenza con i tuoi comandi di salvataggio write_csv
df_rich  <- df_master_classificato %>% filter(Label_HDI == "Developed")
df_poor <- df_master_classificato %>% filter(Label_HDI == "Developing")

# 5. DIVISIONE INCROCIATA IN BASE AL CLIMA (I 4 SOTTO-DATASET MANCANTI)
# (Modifica le stringhe "Caldo" e "Freddo" se nel tuo df classificazione sono scritte in inglese o minuscolo)

# --- Sotto-dataset per i Paesi Ricchi (Developed) ---
df_rich_caldi  <- df_rich %>% filter(classe_clima == "Caldo")  %>% select(-Label_HDI, -classe_clima)
df_rich_freddi <- df_rich %>% filter(classe_clima == "Freddo") %>% select(-Label_HDI, -classe_clima)

# --- Sotto-dataset per i Paesi Poveri (Developing) ---
df_poor_caldi  <- df_poor %>% filter(classe_clima == "Caldo")  %>% select(-Label_HDI, -classe_clima)
df_poor_freddi <- df_poor %>% filter(classe_clima == "Freddo") %>% select(-Label_HDI, -classe_clima)

# Puliamo anche i dataset Rich e Poor generici dalle colonne di servizio prima del salvataggio
df_rich  <- df_rich  %>% select(-Label_HDI, -classe_clima)
df_poor <- df_poor %>% select(-Label_HDI, -classe_clima)

# Definizione del df_full generale (senza colonne di classificazione) se serve per il primo write_csv
df_full <- df_master_classificato %>% select(-Label_HDI, -classe_clima)

# --- Conteggi aggiuntivi per i sotto-dataset ---
print("--- Dettaglio Sotto-Dataset (Ricchi/Poveri x Caldi/Freddi) ---")
print(paste("Ricchi - Caldi :", length(unique(df_rich_caldi$Name)), "paesi"))
print(paste("Ricchi - Freddi:", length(unique(df_rich_freddi$Name)), "paesi"))
print(paste("Poveri - Caldi :", length(unique(df_poor_caldi$Name)), "paesi"))
print(paste("Poveri - Freddi:", length(unique(df_poor_freddi$Name)), "paesi"))

# ==============================================================================
# 7. SALVATAGGIO DEI DATI PRONTI PER LE REGRESSIONI
# ==============================================================================
write_csv(df_full, "../DATA/CLEAN_DATA/Dataset_Panel.csv")
write_csv(df_rich, "../DATA/CLEAN_DATA/Dataset_Panel_Rich.csv")
write_csv(df_poor, "../DATA/CLEAN_DATA/Dataset_Panel_Poor.csv")

write_csv(df_rich_caldi, "../DATA/CLEAN_DATA/Dataset_Panel_Rich_Caldi.csv")
write_csv(df_rich_freddi, "../DATA/CLEAN_DATA/Dataset_Panel_Rich_Freddi.csv")

write_csv(df_poor_caldi, "../DATA/CLEAN_DATA/Dataset_Panel_Poor_Caldi.csv")
write_csv(df_poor_freddi, "../DATA/CLEAN_DATA/Dataset_Panel_Poor_Freddi.csv")

print(paste("Dataset salvati. Paesi Ricchi:", length(unique(df_rich$Name)), 
            "| Paesi Poveri:", length(unique(df_poor$Name))))

# --- Conteggi aggiuntivi per i sotto-dataset ---
print("--- Dettaglio Sotto-Dataset (Ricchi/Poveri x Caldi/Freddi) ---")
print(paste("Ricchi - Caldi :", length(unique(df_rich_caldi$Name)), "paesi"))
print(paste("Ricchi - Freddi:", length(unique(df_rich_freddi$Name)), "paesi"))
print(paste("Poveri - Caldi :", length(unique(df_poor_caldi$Name)), "paesi"))
print(paste("Poveri - Freddi:", length(unique(df_poor_freddi$Name)), "paesi"))

################################################################################
#############         STOP      ####################################################
################################################################################






