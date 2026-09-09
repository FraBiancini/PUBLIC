# ==============================================================================
# INITIALIZATION AND ENVIRONMENT SETUP
# ==============================================================================

# Loading necessary libraries for data manipulation, visualization, and time-series analysis
library(readr)  
library(dplyr)  
library(ggplot2)
library(tidyr)
library(urca)

# Setting the seed to ensure exact reproducibility of the results
set.seed(42)

# Clearing any existing graphic devices to prevent plotting overlaps
graphics.off()

# Selecting the target dataset configuration for the analysis
dataset_key <- "base"

# Mapping dataset keys to their respective local file paths
files <- list(
  base        = "../DATA/CLEAN_DATA/Dataset_Panel.csv",
  rich        = "../DATA/CLEAN_DATA/Dataset_Panel_Rich.csv",
  poor        = "../DATA/CLEAN_DATA/Dataset_Panel_Poor.csv",
  
  rich_caldi  = "../DATA/CLEAN_DATA/Dataset_Panel_Rich_Caldi.csv",
  rich_freddi = "../DATA/CLEAN_DATA/Dataset_Panel_Rich_Freddi.csv",
  
  poor_caldi  = "../DATA/CLEAN_DATA/Dataset_Panel_Poor_Caldi.csv",
  poor_freddi = "../DATA/CLEAN_DATA/Dataset_Panel_Poor_Freddi.csv"
)

# Loading the selected panel dataset into the global environment
df <- read_csv(files[[dataset_key]])

# ==============================================================================
# PREPROCESSING: LOGARITHMIC TRANSFORMATIONS
# ==============================================================================

# Defining a function to apply logarithmic transformations to macroeconomic variables 
# in accordance with standard econometric literature, mitigating exponential growth trends
apply_log_transform <- function(dataset) {
  dataset %>%
    mutate(
      log_GDP  = log(GDP), 
      log_emp  = log(emp),
      log_rnna = log(rnna),
      # Applying a signed log transformation for inflation to correctly handle negative values (deflation)
      log_infl = sign(infl) * log(1 + abs(infl))
    ) %>%
    # Relocating the newly created variables next to their original counterparts for dataframe readability
    relocate(log_GDP, .after = GDP) %>% 
    relocate(log_emp, .after = emp) %>%
    relocate(log_infl, .after = infl) %>%
    relocate(log_rnna, .after = rnna) 
}

# Executing the transformation on the main dataframe
df <- apply_log_transform(df)

# ==============================================================================
# PREPROCESSING: VISUAL INSPECTION OF PANEL DYNAMICS
# ==============================================================================

# Specifying the target variables for visual trend analysis
variabili_grafici <- c("log_GDP", "log_rnna", "log_emp", "log_infl", "hc", "hd30", "cdd", "r20mm")

# Iterating through each variable to generate time-series plots for visual inspection
for (var in variabili_grafici) {
  
  # Constructing the ggplot object dynamically using the .data pronoun
  p <- ggplot(df, aes(x = Year, y = .data[[var]], color = Name, group = Name)) +
    
    # Implementing slight transparency to maintain visibility across intersecting country lines
    geom_line(alpha = 0.7, linewidth = 0.8) +
    
    # Applying a clean, academic visual theme
    theme_minimal() +
    
    # Automatically generating labels and titles based on the variable name
    labs(
      title = paste("Andamento Temporale:", var),
      subtitle = "Panel di 41 Paesi (1951-2023)",
      x = "Anno",
      y = var,
      color = "Paese"
    ) +
    
    # Optimizing the legend layout to accommodate the extensive cross-sectional dimension (41 countries)
    theme(
      legend.position = "right",
      legend.text = element_text(size = 7),       
      legend.key.size = unit(0.4, "cm"),          
      plot.title = element_text(face = "bold")    
    ) +
    
    # Splitting the legend into two columns to prevent screen overflow
    guides(color = guide_legend(ncol = 2))
  
  # Printing the plot to the graphic device
  print(p)
}

# ==============================================================================
# PREPROCESSING: UNIVARIATE STATIONARITY ANALYSIS (ADF TEST)
# ==============================================================================

# Defining the deterministic components (trend, drift, or none) for the 
# Augmented Dickey-Fuller (ADF) test equations at different levels of differencing
config_adf <- list(
  # Macroeconomic variables (exhibiting historical underlying trends)
  "log_GDP"  = c(L0 = "trend", D1 = "drift", D2 = "none"),
  "log_emp"  = c(L0 = "trend", D1 = "trend", D2 = "none"),
  "log_rnna" = c(L0 = "trend", D1 = "trend", D2 = "none"),
  "infl"     = c(L0 = "drift", D1 = "drift", D2 = "none"),
  "hc"       = c(L0 = "trend", D1 = "trend", D2 = "none"),
  
  # Climate variables (exhibiting deterministic trends in their residual anomalies based on visual inspection)
  "hd30"     = c(L0 = "trend", D1 = "none",  D2 = "none"),
  "cdd"      = c(L0 = "trend", D1 = "none",  D2 = "none"),
  "r20mm"    = c(L0 = "trend", D1 = "none",  D2 = "none")
)

# Defining a custom function to determine the integration order (I(d)) of a single time series
get_integration_order_custom <- function(serie, nome_var, config) {
  
  # Removing missing values to prevent algorithm failure
  serie_pulita <- na.omit(serie)
  
  # Checking for sufficient sample size and non-zero variance
  if(length(serie_pulita) < 20 || var(serie_pulita) == 0) {
    return("Non testabile")
  }
  
  # Internal helper function to execute the ADF test utilizing the BIC criterion for optimal lag selection
  run_adf_test <- function(x, test_type) {
    tryCatch({
      test_obj <- ur.df(x, type = test_type, selectlags = "BIC")
      stat <- test_obj@teststat[1]
      crit_5pct <- test_obj@cval[1, "10pct"]
      return(stat < crit_5pct) # Returns TRUE if the null hypothesis of a unit root is rejected at the 5% level
    }, error = function(e) {
      return(NA)
    })
  }
  
  # Retrieving the appropriate deterministic parameters, defaulting to a standard macro setup if unspecified
  if(nome_var %in% names(config)) {
    params <- config[[nome_var]]
  } else {
    params <- c(L0 = "trend", D1 = "drift", D2 = "none") 
  }
  
  # Executing the ADF test on the level series (I(0))
  is_i0 <- run_adf_test(serie_pulita, test_type = params["L0"])
  if (!is.na(is_i0) && is_i0 == TRUE) return("I(0)")
  
  # Executing the ADF test on the first-differenced series (I(1))
  diff1 <- diff(serie_pulita)
  is_i1 <- run_adf_test(diff1, test_type = params["D1"])
  if (!is.na(is_i1) && is_i1 == TRUE) return("I(1)")
  
  # Executing the ADF test on the second-differenced series (I(2))
  diff2 <- diff(diff1)
  is_i2 <- run_adf_test(diff2, test_type = params["D2"])
  if (!is.na(is_i2) && is_i2 == TRUE) return("I(2)")
  
  # Returning > I(2) if the series remains non-stationary after two differences
  return("> I(2)")
}

# Specifying the vector of variables requiring formal unit root testing
variabili_da_testare <- c("log_GDP", "log_rnna", "log_emp", "log_infl", "hc", "hd30", "cdd", "r20mm")

# Initializing an empty dataframe to collect and store the integration orders
report_integrazione <- data.frame(
  Paese = character(), 
  Variabile = character(), 
  Ordine = character(), 
  stringsAsFactors = FALSE
)

# Extracting the unique cross-sectional identifiers
lista_paesi <- unique(df$Name)

# Iterating over each country and each variable to perform the sequential ADF testing
for (paese in lista_paesi) {
  df_paese <- df %>% filter(Name == paese) %>% arrange(Year)
  
  for (var in variabili_da_testare) {
    serie_corrente <- as.numeric(df_paese[[var]])
    
    ordine <- get_integration_order_custom(serie_corrente, var, config_adf)
    
    report_integrazione <- rbind(report_integrazione, data.frame(
      Paese = paese, Variabile = var, Ordine = ordine
    ))
  }
}

# Reshaping the output into a wide matrix format for cross-country comparative readability
matrice_integrazione <- report_integrazione %>%
  pivot_wider(names_from = Variabile, values_from = Ordine)

# Printing the final univariate stationarity results
print(matrice_integrazione, n = "inf")

# ==============================================================================
# PANEL STATIONARITY ANALYSIS (IPS TEST)
# ==============================================================================
library(plm)
library(dplyr)

# Converting the standard dataframe into a structured panel dataframe
p_df <- pdata.frame(df, index = c("Name", "Year"))

# Configuring the deterministic components for the Im-Pesaran-Shin (IPS) test at levels (L0), 
# guided by macroeconomic literature and preliminary graphical analysis
ips_L0_config <- list(
  "log_GDP"    = "trend",
  "hd30"       = "trend",
  "cdd"        = "trend",
  "r20mm"      = "trend",
  "log_infl"   = "intercept",
  "log_rnna"   = "trend",
  "log_emp"    = "trend",
  "hc"         = "trend"
)

# Iterating through each variable to determine the exact order of integration
for (var in names(ips_L0_config)) {
  
  d <- 0                 
  stazionaria <- FALSE  
  
  # Iterating through integration orders (up to I(2)) until stationarity is achieved
  while (d <= 2 && !stazionaria) {
    
    if (d == 0) {
      # Testing the variable in levels
      test_serie <- p_df[[var]]
      exo_type   <- ips_L0_config[[var]]
      
    } else if (d == 1 && var %in% c("log_pop", "hc")) {
      # Accounting for the long-term deceleration in population and human capital growth 
      # by retaining a linear trend in their first differences
      test_serie <- diff(p_df[[var]], differences = d)
      exo_type   <- "trend" 
      
    } else {
      # Applying an intercept-only specification for all other variables at the first-difference level
      test_serie <- diff(p_df[[var]], differences = d)
      exo_type   <- "intercept" 
    }
    
    tryCatch({
      # Executing the Im-Pesaran-Shin panel unit root test with automatic AIC lag selection
      test_res <- purtest(test_serie, test = "ips", exo = exo_type, lags = "AIC", pmax = 4)
      
      p_value <- test_res$statistic$p.value
      stat_val <- test_res$statistic$statistic
      
      # Evaluating the p-value against the standard 5% significance level
      if (p_value < 0.05) {
        stazionaria <- TRUE
        # Logging the successful identification of the integration order
        cat(sprintf("%-12s | Trovata: I(%d) | Modello test: %-9s | p-value: %.4f\n", 
                    var, d, exo_type, p_value))
      } else {
        # Incrementing the differencing parameter if the null hypothesis of a unit root is not rejected
        d <- d + 1
      }
      
    }, error = function(e) {
      # Handling potential estimation errors by automatically forcing a higher integration order
      d <<- d + 1 
    })
  }
  
  # Logging a warning if the variable remains non-stationary after two differences
  if (!stazionaria) {
    cat(sprintf("%-12s | NON stazionaria dopo 2 differenze (o test fallito)\n", var))
  }
}

c("log_GDP", "log_rnna", "log_emp", "log_infl", "hc", "hd30", "cdd", "r20mm")

# Computing the first logarithmic differences to induce stationarity, 
# representing approximate growth rates for the macroeconomic variables
df <- df %>%
  arrange(Name, Year) %>%
  group_by(Name) %>%
  mutate(
    d_log_GDP  = c(NA, diff(log_GDP)),
    d_log_emp  = c(NA, diff(log_emp)),
    d_hc       = c(NA, diff(hc)),
    d_log_rnna = c(NA, diff(log_rnna))
  ) %>%
  ungroup()

# Removing the initial NA rows generated by the differencing process
df <- df %>%
  filter(!is.na(d_log_GDP))


# ==============================================================================
# POST-DIFFERENCING STATIONARITY VERIFICATION
# ==============================================================================

# Defining the deterministic configuration for the differenced macroeconomic variables
config_adf <- list(
  "d_log_GDP"  = c(L0 = "drift", D1 = "none", D2 = "none"),
  "d_log_emp"  = c(L0 = "trend", D1 = "none", D2 = "none"),
  "d_log_rnna" = c(L0 = "trend", D1 = "none", D2 = "none"),
  "d_hc"       = c(L0 = "trend", D1 = "none", D2 = "none")
)

# Specifying the subset of differenced variables requiring formal re-testing
variabili_da_testare <- c("d_log_GDP", "d_log_emp", "d_log_rnna", "d_hc")

# Initializing a dataframe to store the secondary integration results
report_integrazione <- data.frame(
  Paese = character(), 
  Variabile = character(), 
  Ordine = character(), 
  stringsAsFactors = FALSE
)

lista_paesi <- unique(df$Name)

# Iterating cross-sectionally to verify that all transformed series are now strictly I(0)
for (paese in lista_paesi) {
  df_paese <- df %>% filter(Name == paese) %>% arrange(Year)
  
  for (var in variabili_da_testare) {
    serie_corrente <- as.numeric(df_paese[[var]])
    
    ordine <- get_integration_order_custom(serie_corrente, var, config_adf)
    
    report_integrazione <- rbind(report_integrazione, data.frame(
      Paese = paese, Variabile = var, Ordine = ordine
    ))
  }
}

# Formatting and displaying the final verification matrix
matrice_integrazione <- report_integrazione %>%
  pivot_wider(names_from = Variabile, values_from = Ordine)

print(matrice_integrazione, n = "inf")

# Confirming the robustness of the stationarity transformation


# ==============================================================================
# MULTICOLLINEARITY DIAGNOSTICS
# ==============================================================================

library(plm)
library(corrplot)

# Preparing the dataset for the correlation matrix analysis
panel_total_free <- pdata.frame(df, index = c("Name", "Year"))

# Extracting the specific covariates included in the primary model
covariates <- panel_total_free[, c("hd30", "r20mm", "cdd", "d_log_emp", "d_hc", "log_infl", "d_log_rnna")]

# Computing the Pearson correlation matrix
cor_matrix <- cor(covariates, method = "pearson")

# Visualizing the correlation matrix via an elliptical correlogram
corrplot(cor_matrix, 
         method = "ellipse", 
         type = "upper", 
         order = "hclust", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.srt = 45, 
         diag = FALSE, 
         mar = c(0, 0, 2, 0), 
         title = "Matrice di Correlazione per la Diagnosi di Collinearità")

# Performing a Variance Inflation Factor (VIF) analysis using an auxiliary pooled OLS regression
library(car)

modello_vif <- lm(
  d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna, 
  data = df
)

valori_vif <- vif(modello_vif)

print(valori_vif)

# Confirming the absence of severe multicollinearity, as all estimated VIF scores 
# remain strictly below the standard critical threshold of 5

# ==============================================================================
# OUTLIER DETECTION AND ELIMINATION (MACRO-LEVEL)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(broom)  

# Converting the cross-sectional identifier into a factor variable for regression modeling
df$Name <- as.factor(df$Name)

# Estimating an auxiliary Least Squares Dummy Variable (LSDV) model to explicitly 
# capture country-level fixed effects, enabling subsequent diagnostic plotting
modello_lsdv <- lm(d_log_GDP ~   hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna + Name,  
                   data = df)

# Extracting diagnostic metrics (leverage, Cook's distance, residuals) into a dedicated dataframe
df_diag <- augment(modello_lsdv, data = df)

# Computing studentized residuals to adjust for varying leverages
df_diag$stud_resid <- rstudent(modello_lsdv)

# Extracting model dimensions to calculate critical thresholds
n_obs <- nrow(df_diag)
k_params <- length(coef(modello_lsdv))

# Defining critical thresholds for leverage and Cook's distance based on standard econometric heuristics
soglia_leverage <- (2 * k_params) / n_obs
soglia_cook <- 4 / n_obs

# Plotting leverage (hat values) to identify countries exhibiting extreme values in the covariate space
ggplot(df_diag, aes(x = reorder(Name, .hat, FUN = median), y = .hat)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", alpha = 0.7) +
  geom_hline(yintercept = soglia_leverage, color = "red", linetype = "dashed", linewidth = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Leverage (Hat Values) by Country",
       subtitle = paste("Observations beyond the dashed red line (", round(soglia_leverage, 3), ") exhibit critical covariate extremity"),
       x = "Country", y = "Leverage")

# Plotting studentized residuals to detect anomalous, unobserved shocks to GDP growth
ggplot(df_diag, aes(x = reorder(Name, stud_resid, FUN = median), y = stud_resid)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", alpha = 0.7) +
  geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed", linewidth = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Studentized Residuals by Country",
       subtitle = "Values exceeding ±3 indicate severe anomalous shocks unexplained by the baseline model",
       x = "Country", y = "Studentized Residual")

# Plotting Cook's distance to evaluate the overall influence of specific observations on the estimated coefficients
ggplot(df_diag, aes(x = reorder(Name, .cooksd, FUN = max), y = .cooksd)) +
  geom_boxplot(fill = "salmon", outlier.color = "darkred", alpha = 0.7) +
  geom_hline(yintercept = soglia_cook, color = "darkred", linetype = "dashed", linewidth = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Cook's Distance by Country",
       subtitle = paste("Observations exceeding the threshold (", round(soglia_cook, 4), ") severely distort parameter estimation"),
       x = "Country", y = "Cook's Distance")

# Generating a composite influence plot integrating leverage, residuals, and Cook's distance
df_diag <- df_diag %>%
  mutate(is_influential = ifelse(.cooksd > soglia_cook | abs(stud_resid) > 3, "Critico", "Normale"))

ggplot(df_diag, aes(x = .hat, y = stud_resid, size = .cooksd, color = is_influential)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Critico" = "red", "Normale" = "darkgrey")) +
  geom_vline(xintercept = soglia_leverage, linetype = "dashed", color = "blue", alpha = 0.5) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "blue", alpha = 0.5) +
  facet_wrap(~ Name, scales = "free") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 8)) +
  labs(title = "Influence Plot by Individual Country",
       subtitle = "X: Leverage | Y: Studentized Residuals | Bubble Size: Cook's Distance",
       x = "Leverage (Covariate Extremity)", 
       y = "Studentized Residuals (Response Extremity)")

# Defining dynamic exclusion lists based on the selected dataset configuration to prevent structural distortions
paesi_da_scartare <- switch(dataset_key,
                            "base"       = c("Venezuela (Bolivarian Republic of)", "Iran (Islamic Republic of)", "Algeria"),
                            "rich"       = c(),  
                            "poor"       = c(), 
                            "rich_caldi" = c(),
                            "rich_miti" = c(),
                            "rich_freddi" = c(),
                            "poor_caldi" = c(),
                            "poor_miti" = c(),
                            "poor_freddi" = c(),
                            NULL         
)

# Filtering out the highly influential macro-level outliers from the primary analytical dataframe
df <- df %>%
  filter(!(Name %in% paesi_da_scartare))

# Preserving a clean, contiguous version of the dataset prior to single-observation trimming 
# to prevent gaps in subsequent lag structures (e.g., PVAR, ARMAX)
df_intero <- df 

# ==============================================================================
# ASSUMPTION VERIFICATION AND SINGLE OBSERVATION TRIMMING
# ==============================================================================

library(dplyr)
library(plm)

# Formatting the dataframe for rigorous panel estimations
panel <- pdata.frame(df, index = c("Name", "Year"))
formula_reg <- d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna

# Estimating baseline Fixed Effects and Random Effects models to extract initial residuals
fe_total <- plm(formula_reg, data = panel_total_free, model = "within", effect = "twoways")
re_total <- plm(formula_reg, data = panel_total_free, model = "random", effect = "twoways")
phtest(fe_total, re_total)
summary(fe_total)

# Verifying Assumption 1: Evaluating if the unconditional mean of the residuals is strictly zero
residuals = fe_total$residuals
mean(fe_total$residuals)

# Verifying Assumption 5: Assessing the Gaussian distribution of the error terms
residui_standardizzati <- scale(residuals)
qqnorm(residui_standardizzati)
qqline(residui_standardizzati, col = "red")

library(tseries)
# Performing the Jarque-Bera test for formal normality assessment
jarque.bera.test(residuals)

# Estimating a cross-sectional OLS equivalent with two-way dummy variables 
# to calculate observation-level Cook's distances
fit <- lm(d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna + as.factor(Name) + as.factor(Year), 
          data = df)

# Extracting the total number of valid observations utilized by the regression
n_obs <- length(cooks.distance(fit))

# Generating a logical vector to strictly identify robust observations lying below the critical threshold (4/n)
ix <- cooks.distance(fit) < (4 / n_obs)

# Printing the total count of trimmed individual observations for diagnostic tracking
cat("Osservazioni eliminate:", sum(!ix, na.rm = TRUE), "su", n_obs, "\n")

# Generating the final robust dataset by keeping only strictly non-influential observations,
# utilizing which() to prevent indexing errors caused by generated NA residuals
df <- df[which(ix), ]

# Re-estimating the base model on the trimmed dataset to ensure parameter stability
fit_out <- lm(d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna + as.factor(Name) + as.factor(Year),  
              data = df)

# Confirming the residual mean remains virtually zero post-trimming
cat("\nMedia dei residui del nuovo modello:", mean(fit_out$residuals), "\n")


# Re-evaluating the Q-Q plot to visually confirm improvements in the residual distribution tails
residui_standardizzati_out <- scale(fit_out$residuals)
qqnorm(residui_standardizzati_out, main = "Normal Q-Q Plot (Outliers Rimosse tramite Cook)")
qqline(residui_standardizzati_out, col = "red", lwd = 2)

# Executing the formal Jarque-Bera test on the strictly trimmed residual distribution
jarque.bera.test(fit_out$residuals)

# ==============================================================================
# EVALUATING WINSORIZATION AS AN ALTERNATIVE APPROACH
# ==============================================================================
library(DescTools)

# Applying a 1% - 99% Winsorization procedure to strictly bound extreme tails without physical deletion
df_win <- df %>%
  mutate(
    d_log_GDP  = Winsorize(d_log_GDP,  val = quantile(d_log_GDP,  probs = c(0.01, 0.99), na.rm = TRUE)),
    d_log_emp  = Winsorize(d_log_emp,  val = quantile(d_log_emp,  probs = c(0.01, 0.99), na.rm = TRUE)),
    d_hc       = Winsorize(d_hc,       val = quantile(d_hc,       probs = c(0.01, 0.99), na.rm = TRUE)),
    log_infl   = Winsorize(log_infl,   val = quantile(log_infl,   probs = c(0.01, 0.99), na.rm = TRUE)),
    d_log_rnna = Winsorize(d_log_rnna, val = quantile(d_log_rnna, probs = c(0.01, 0.99), na.rm = TRUE))
  )

# Estimating the LSDV model on the Winsorized dataset
modello_lsdv_w <- lm(d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna + as.factor(Name) + as.factor(Year),  
                     data = df_win)

residui_nuovi <- residuals(modello_lsdv_w)
qqnorm(residui_nuovi); qqline(residui_nuovi)
jarque.bera.test(residui_nuovi)

# Rejecting the Winsorization approach: The Jarque-Bera test still strictly rejects normality 
# under mild bounding, indicating that physical trimming is the superior methodological choice.


# ==============================================================================
# HOMOSCEDASTICITY AND AUTOCORRELATION DIAGNOSTICS
# ==============================================================================

# Verifying Assumption 2: Homoscedasticity
library(whitestrap)

fe_total <- plm(formula_reg, data = panel_total_free, model = "within", effect = "twoways")

# Executing the White test for heteroscedasticity
white_test(fe_total)
# Rejecting the null hypothesis, confirming the presence of strong heteroscedasticity in the panel structure.

# Noting that Assumption 4 (no perfect collinearity) has already been addressed via VIF analysis.

# Verifying Assumption 3: Absence of serial correlation in the error terms
library(forecast)

# Selecting a subset of representative countries for the visual inspection of residual autocorrelation
paesi_scelti <- switch(dataset_key,
                       "base"       = c("Italy", "Germany", "Kenya", "Brazil"),
                       "rich"       = c("Italy", "Germany", "Australia", "Canada"),  
                       "poor"       = c(), 
                       "rich_caldi" = c(),
                       "rich_miti"  = c(),
                       "rich_freddi"= c(),
                       "poor_caldi" = c(),
                       "poor_miti"  = c(),
                       "poor_freddi"= c(),
                       NULL         
)

# Configuring the graphical parameters for multi-panel plotting
par(mfrow = c(4, 2), mar = c(4, 4, 3, 1)) 

# Extracting and matching residuals to their respective temporal and cross-sectional indices
for (paese in paesi_scelti) {
  df_residui_paese <- data.frame(
    Name  = attr(residuals(fe_total), "index")[[1]],
    Year  = as.numeric(as.character(attr(residuals(fe_total), "index")[[2]])),
    Resid = as.numeric(residuals(fe_total))
  ) %>% 
    filter(Name == paese) %>%
    arrange(Year)
  
  # Plotting the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)
  acf(df_residui_paese$Resid, main = paste("ACF Residuals -", paese), col = "darkblue", lwd = 2)
  pacf(df_residui_paese$Resid, main = paste("PACF Residuals -", paese), col = "darkblue", lwd = 2)
}
# Observing the correlograms: Residuals largely fall within the 95% confidence intervals, 
# though some latent dynamic persistence may remain.

# Executing the formal Breusch-Godfrey test across multiple lag orders to detect serial correlation
for (i in 1:4) {
  bg_result <- pbgtest(fe_total, order = i)
  cat(sprintf("\nBreusch-Godfrey Test (Order %d):\n", i))
  cat(sprintf("  P-value: %.4f\n", bg_result$p.value))
}

# Concluding that serial correlation is present, thereby mandating the use of 
# Driscoll-Kraay robust standard errors (SCC) in all subsequent linear panel estimations.

# Resetting the graphical parameters
par(mfrow = c(1, 1))

# ==============================================================================
# LINEAR PANEL REGRESSIONS: FIXED AND RANDOM EFFECTS
# ==============================================================================

# Loading the necessary library for advanced coefficient testing and robust inference
library(lmtest)

# Structuring the dataset as a formal panel data frame
panel_total_free <- pdata.frame(df, index = c("Name", "Year"))

# Specifying the baseline regression formula integrating climate shocks and macroeconomic controls
formula_reg <- d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna

# Estimating the baseline Fixed Effects (Within) model
fe_total <- plm(formula_reg, data = panel_total_free, model = "within", effect = "individual") 

# Estimating the baseline Random Effects model
re_total <- plm(formula_reg, data = panel_total_free, model = "random", effect = "individual") 

# Executing the Hausman specification test to determine the appropriate panel model
# (H0: Random Effects estimator is consistent and efficient | H1: Fixed Effects estimator is required)
phtest(fe_total, re_total)

# Rejecting the null hypothesis (p-value < 0.05) indicates that the individual effects are correlated 
# with the regressors, making the Fixed Effects model the preferred and consistent specification.
# Note: Incorporating two-way effects may lead to non-rejection, but random effects often overshadow the regressors.

# Applying Driscoll-Kraay robust standard errors (SCC) to the baseline Fixed Effects model 
# to correct for general forms of cross-sectional dependence and temporal autocorrelation
tabella_corretta_scc <- coeftest(fe_total, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 4))
print(tabella_corretta_scc)

# Estimating Alternative Specification 1: Restricted macroeconomic controls
fe_total1 <- plm(d_log_GDP ~ hd30 + d_log_emp + log_infl + d_log_rnna, data = panel_total_free, model = "within", effect = "individual") 
tabella_corretta_scc1 <- coeftest(fe_total1, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 4))
print(tabella_corretta_scc1)

# Estimating Alternative Specification 2: Including multiplicative interaction terms among climate shocks
fe_total2 <- plm(d_log_GDP ~ (hd30 + r20mm + cdd)^2 + d_log_emp + d_hc + log_infl + d_log_rnna, data = panel_total_free, model = "within", effect = "individual" ) 
tabella_corretta_scc2 <- coeftest(fe_total2, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 4))
print(tabella_corretta_scc2)

# Estimating Alternative Specification 3: Including quadratic polynomial effects for extreme heat
fe_total3 <- plm(d_log_GDP ~ hd30 + I(hd30^2) + d_log_emp + d_hc + log_infl + d_log_rnna, data = panel_total_free, model = "within", effect = "twoways") 
tabella_corretta_scc3 <- coeftest(fe_total3, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 4))
print(tabella_corretta_scc3)


# ==============================================================================
# LINEAR MIXED-EFFECTS MODELS (LME)
# ==============================================================================
# Employing mixed-effects frameworks to relax strict OLS assumptions, directly 
# modeling heteroscedasticity and capturing complex hierarchical variance structures.

library(nlme)

# Configuring the optimization algorithm parameters to ensure convergence during maximum likelihood estimation
controllo <- lmeControl(maxIter = 500, msMaxIter = 500, niterEM = 500)

# Estimating the baseline Linear Mixed-Effects model, specifying random intercepts for both Country and Year
lme_panel <- lme(
  d_log_GDP ~ hd30 + r20mm + cdd + d_log_emp + d_hc + log_infl + d_log_rnna, 
  random = list(Name = ~1, Year = ~1),
  # Applying the varIdent variance function to explicitly model country-specific heteroscedasticity
  weights = varIdent(), 
  data = panel_total_free,
  control = controllo
)

summary(lme_panel)

# Estimating Alternative LME Specification 1: Restricted macroeconomic controls
lme_panel2 <- lme(
  d_log_GDP ~ hd30 + d_log_emp + log_infl + d_log_rnna, 
  random = list(Name = ~1, Year = ~1),
  weights = varIdent(), 
  data = panel_total_free,
  control = controllo
)

summary(lme_panel2)


# Estimating Alternative LME Specification 2: Interaction terms among climate shocks
lme_panel3 <- lme(
  d_log_GDP ~ (hd30 + r20mm + cdd)^2 + d_log_emp + d_hc + log_infl + d_log_rnna, 
  random = list(Name = ~1, Year = ~1),
  weights = varIdent(), 
  data = panel_total_free,
  control = controllo
)

summary(lme_panel3)

# Estimating Alternative LME Specification 3: Quadratic polynomial effects for extreme heat
lme_panel4 <- lme(
  d_log_GDP ~ hd30 + I(hd30^2) + d_log_emp + d_hc + log_infl + d_log_rnna, 
  random = list(Name = ~1, Year = ~1),
  weights = varIdent(), 
  data = panel_total_free,
  control = controllo
)

summary(lme_panel4)


# ==============================================================================
# LME DIAGNOSTIC PLOTTING
# ==============================================================================

# Plotting standardized Pearson residuals against fitted values to evaluate overall homoscedasticity
plot(lme_panel, resid(., type = "pearson") ~ fitted(.), 
     abline = 0, id = 0.05, 
     main = "Residui Standardizzati vs Valori Predetti")

# Generating a Normal Q-Q plot of the Pearson residuals to visually assess distributional normality
qqnorm(lme_panel, ~ resid(., type = "pearson"), id = 0.05,
       main = "Normal Q-Q Plot dei Residui")

# Generating boxplots of residuals grouped by country to inspect cross-sectional variance differences
plot(lme_panel, Name ~ resid(., type = "pearson"), 
     abline = 0, main = "Distribuzione dei Residui per Soggetto")

# Plotting observed target values against predicted fitted values to visually assess predictive accuracy
plot(lme_panel, d_log_GDP ~ fitted(.), 
     abline = c(0, 1), id = 0.05,
     main = "Valori Osservati vs Valori Predetti")

# ==============================================================================
# UNIVARIATE ARMA MODELING AND OUT-OF-SAMPLE FORECASTING (2015-2023)
# ==============================================================================
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Verifying the autoregressive structure of the GDP growth series to formally 
# justify the implementation of an ARMA framework
for (paese in paesi_scelti) {
  df_temp <- df_intero[df_intero$Name == paese, ]
  df_temp <- df_temp[order(df_temp$Year), ]
  
  # Implementing a safety catch to bypass empty country subsets
  if(nrow(df_temp) == 0) {
    message(paste("Attenzione: il paese", paese, "non trovato nel dataset. Salto..."))
    next
  }
  
  # Plotting the Autocorrelation and Partial Autocorrelation Functions
  acf(df_temp$d_log_GDP, main = paste("ACF -", paese), na.action = na.pass, ylab = "ACF")
  pacf(df_temp$d_log_GDP, main = paste("PACF -", paese), na.action = na.pass, ylab = "PACF")
  
  cat(sprintf("\nTest per %s:\n", paese))
  for (i in 1:4) {
    # Executing the Ljung-Box test to formally detect serial correlation
    lb_result <- Box.test(df_temp$d_log_GDP, lag = i, type = "Ljung-Box")
    # H0: Absence of serial correlation
    cat(sprintf("\nLjung-Box Test (Lag %d):\n", i))
    cat(sprintf("  P-value:              %.4f\n", lb_result$p.value))
  }  
}

# Confirming the general presence of significant autocorrelation across the selected sample, 
# thereby supporting the utilization of autoregressive modeling


# Automatically identifying the optimal ARMA specification for each target country
for (paese in paesi_scelti) {
  df_temp <- df_intero[df_intero$Name == paese, ]
  df_temp <- df_temp[order(df_temp$Year), ]
  
  # Converting the vector into a formal time-series object
  ts_gdp <- ts(df_temp$d_log_GDP, start = min(df_temp$Year), frequency = 1)
  
  # Estimating the optimal ARMA model utilizing information criteria (AICc), 
  # restricting the integration order (d=0) since the series is already differenced
  modello_ottimo <- auto.arima(ts_gdp, d = 0, max.d = 0)
  
  cat("====================================\n")
  cat("Miglior modello ARMA per", paese, ":\n")
  print(modello_ottimo)
}

# Defining the out-of-sample evaluation window
anni_test <- 2015:2023
h_total <- length(anni_test)

# Initializing a dataframe to store the predictive accuracy metrics
accuracy_table <- data.frame()

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

cat("Rolling Forecast con Refitting Annuale\n")

for (paese in paesi_scelti) {
  
  df_temp <- df_intero %>% filter(Name == paese) %>% arrange(Year)
  
  x_date <- df_temp$Year
  y_totale <- df_temp$d_log_GDP
  
  # Extracting the actual observed values for the target forecasting horizon
  y_test_reale <- df_temp %>% filter(Year %in% anni_test) %>% pull(d_log_GDP)
  
  pred_1step <- numeric(h_total)
  
  # EXECUTING THE ANNUAL REFITTING LOOP (EXPANDING WINDOW APPROACH)
  for (i in 1:h_total) {
    
    anno_da_prevedere <- anni_test[i]
    
    # Defining the training set by incorporating all historical data up to the year prior to the target horizon
    train_data <- df_temp %>% filter(Year < anno_da_prevedere)
    ts_train <- ts(train_data$d_log_GDP, start = min(train_data$Year), frequency = 1)
    
    # Re-estimating the optimal ARMA specification incorporating the newly revealed observation
    modello_refittato <- auto.arima(ts_train, d = 0, max.d = 0)
    
    # Generating the one-step-ahead point forecast
    previsione <- forecast(modello_refittato, h = 1)
    
    pred_1step[i] <- previsione$mean[1]
  }
  
  # Calculating out-of-sample accuracy metrics comparing predictions against realized values
  errore <- y_test_reale - pred_1step
  mse <- mean(errore^2)
  mae <- mean(abs(errore))
  mape <- mean(abs(errore / y_test_reale)) * 100
  
  accuracy_table <- rbind(accuracy_table, 
                          data.frame(Paese = paese, MSE = mse, MAE = mae, MAPE = mape))
  
  y_limits <- range(c(y_totale, pred_1step), na.rm = TRUE)
  
  # Plotting the historical series alongside the rolling predictions
  plot(x_date, y_totale, type = "l", col = "black", lwd = 2, 
       main = paste(paese, "- Refitting Annuale"),
       xlab = "Anno", ylab = "d_log_GDP", ylim = y_limits)
  
  abline(v = min(anni_test), col = "gray50", lty = 3, lwd = 2)
  
  lines(anni_test, pred_1step, col = "red", lwd = 2, type = "b", pch = 16)
  
  legend("topleft", legend = c("Reale", "Forecast 1-step"), 
         col = c("black", "red"), lty = 1, lwd = 2, bty = "n", cex = 0.8)
  
  cat("Completato refitting per:", paese, "\n")
}

par(mfrow = c(1, 1))

print(accuracy_table)

cat("\nIl paese con l'errore percentuale minore (MAPE) è:\n")
print(accuracy_table[which.min(accuracy_table$MAPE), ])

# ==============================================================================
# EXPANDING THE ROLLING 1-STEP FORECAST TO THE ENTIRE CROSS-SECTIONAL PANEL
# ==============================================================================
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)

tutti_i_paesi <- unique(df_intero$Name)
anni_test <- 2015:2023

# Initializing a global dataframe to store panel-wide forecasting results
df_risultati_globali <- data.frame()

for (paese in tutti_i_paesi) {
  
  df_temp <- df_intero %>% filter(Name == paese) %>% arrange(Year)
  
  df_temp$Forecast <- NA 
  
  indici_test <- which(df_temp$Year %in% anni_test)
  
  # Skipping cross-sections with insufficient longitudinal depth
  if (length(indici_test) == 0 || nrow(df_temp) < 15) {
    next 
  }
  
  # Restricting the rolling forecast execution exclusively to the predefined out-of-sample test window
  for (idx in indici_test) {
    anno_da_prevedere <- df_temp$Year[idx]
    
    train_data <- df_temp %>% filter(Year < anno_da_prevedere)
    
    # Ensuring a minimum training sample size before attempting estimation
    if(nrow(train_data) >= 10) {
      ts_train <- ts(train_data$d_log_GDP, start = min(train_data$Year), frequency = 1)
      
      modello_refittato <- suppressWarnings(auto.arima(ts_train, d = 0, max.d = 0))
      previsione <- forecast(modello_refittato, h = 1)
      
      df_temp$Forecast[idx] <- previsione$mean[1]
    }
  }
  
  df_risultati_globali <- bind_rows(df_risultati_globali, 
                                    df_temp %>% dplyr::select(Name, Year, d_log_GDP, Forecast))
  
  cat(".") # Printing a progress indicator for each processed country
}

df_plot <- df_risultati_globali %>% filter(Year >= 2005)

# Generating a comprehensive facet grid to visually compare actuals vs. forecasts across all countries
grafico_globale <- ggplot(df_plot, aes(x = Year)) +
  
  geom_line(aes(y = d_log_GDP), color = "black", size = 0.8) +
  
  geom_line(aes(y = Forecast), color = "red", size = 1, linetype = "dashed", na.rm = TRUE) +
  geom_point(aes(y = Forecast), color = "red", size = 1.5, na.rm = TRUE) +
  
  facet_wrap(~ Name, scales = "free_y", ncol = 5) + 
  
  labs(title = "Previsione 1-Step-Ahead (Refitting Annuale) vs Dati Reali",
       subtitle = "Linea Nera: Dati Reali | Linea Rossa Tratteggiata: Forecast ARMA",
       x = "Anno",
       y = "d_Log GDP") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        panel.spacing = unit(1, "lines"))

print(grafico_globale)

# ==============================================================================
# RESIDUAL DIAGNOSTICS 
# (Conducted on a representative sample country for analytical brevity)
# ==============================================================================

primo_paese <- paesi_scelti[1]

df_primo_paese <- df_intero %>% filter(Name == primo_paese) %>% arrange(Year)
train_finale <- df_primo_paese %>% filter(Year < 2024)
ts_train_finale <- ts(train_finale$d_log_GDP, start = min(train_finale$Year), frequency = 1)

modello_finale <- auto.arima(ts_train_finale, d = 0, max.d = 0)

residui_insample <- residuals(modello_finale)

# Generating a comprehensive diagnostic dashboard for the final ARMA model
checkresiduals(modello_finale)

# Verifying Assumption 1: Evaluating if the unconditional mean of the residuals is strictly zero
mean(residui_insample)

# Verifying Assumption 2: Testing for homoscedasticity utilizing the Ljung-Box test on squared residuals
test_omoschedasticita <- Box.test(residui_insample^2, lag = 10, type = "Ljung-Box")
print(test_omoschedasticita)

# Verifying Assumption 3: Assessing serial independence (visually confirmed via the ACF plot generated by checkresiduals)

# Verifying Assumption 5: Testing for residual normality utilizing the Jarque-Bera formal test
jarque.bera.test(residui_insample)

# ==============================================================================
# LOCAL PROJECTIONS (JORDÀ, 2005) ESTIMATION FRAMEWORK
# ==============================================================================

# Clearing active graphic devices to ensure a clean plotting environment
graphics.off()

library(plm)
library(lmtest)
library(dplyr)
library(ggplot2)

# Subsetting the main dataset to isolate the specific variables required for the Local Projections
df_lp <- df_intero %>%
  dplyr::select(Code, Name, Year, d_log_GDP, hd30, cdd, r20mm, d_hc, d_log_rnna, d_log_emp, log_infl)

# ==============================================================================
# INITIAL CONFIGURATION FOR LOCAL PROJECTIONS
# ==============================================================================

# Structuring the subset as a formal panel data frame to enable plm operations
p_df_lp <- pdata.frame(df_lp, index = c("Name", "Year")) 

# Defining the maximum projection horizon (h = 0 represents the contemporaneous shock, while 1-4 represent subsequent years)
max_h <- 4 

# Specifying the target climate shock variables for the impulse response extraction
climate_vars <- c("hd30", "cdd", "r20mm")

# Initializing an empty dataframe to systematically store the estimated coefficients and confidence intervals
lp_results <- data.frame()

cat("==============================================================================\n")
cat("      ESTIMATING LOCAL PROJECTIONS (JORDÀ, 2005) WITH 2 LAGS FOR MULTI-SHOCKS   \n")
cat("==============================================================================\n")

# ==============================================================================
# ITERATING OVER PROJECTION HORIZONS
# ==============================================================================

for (h in 0:max_h) {
  
  # Dynamically adjusting the dependent variable: utilizing contemporaneous GDP growth for h=0, and lead operators for h>0
  dep_var <- ifelse(h == 0, "d_log_GDP", paste0("lead(d_log_GDP, ", h, ")"))
  
  # Constructing the structural formula incorporating 2 lags for both the endogenous and exogenous variables to capture dynamic persistence
  lp_formula <- as.formula(paste(
    dep_var, "~",
    "hd30 + lag(hd30, 1) + lag(hd30, 2) +",
    "cdd + lag(cdd, 1) + lag(cdd, 2) +",
    "r20mm + lag(r20mm, 1) + lag(r20mm, 2) +",
    "d_hc + lag(d_hc, 1) + lag(d_hc, 2) +",
    "d_log_rnna + lag(d_log_rnna, 1) + lag(d_log_rnna, 2) +",
    "d_log_emp + lag(d_log_emp, 1) + lag(d_log_emp, 2) +",
    "log_infl + lag(log_infl, 1) + lag(log_infl, 2) +",
    "lag(d_log_GDP, 1) + lag(d_log_GDP, 2)"
  ))
  
  # Estimating the Local Projection model utilizing the Within estimator to control for unobserved individual fixed effects
  lp_model <- plm(lp_formula, data = p_df_lp, model = "within", effect = "individual")
  
  # Applying Driscoll-Kraay robust standard errors (SCC) with a maximum lag of 5 to account for cross-sectional dependence and the overlapping serial correlation inherent in Local Projections
  robust_stats <- coeftest(lp_model, vcov = function(x) vcovSCC(x, type = "HC1", maxlag = 5))
  
  # Extracting the point estimates and standard errors exclusively for the contemporaneous climate shocks
  for (var in climate_vars) {
    estimate <- robust_stats[var, "Estimate"]
    std_err  <- robust_stats[var, "Std. Error"]
    
    # Computing the 95% confidence intervals and appending the horizon-specific results to the main dataframe
    temp_res <- data.frame(
      Variable = var,
      Horizon  = h,
      Effect   = estimate,
      Lower    = estimate - 1.96 * std_err,
      Upper    = estimate + 1.96 * std_err
    )
    lp_results <- rbind(lp_results, temp_res)
  }
  cat(sprintf("Horizon H = %d successfully estimated.\n", h))
}

# ==============================================================================
# PLOTTING MULTIPLE IMPULSE RESPONSE FUNCTIONS (IRF)
# ==============================================================================

# Refactoring the variable names into descriptive factors to ensure professional graphic labeling
lp_results$Variable <- factor(lp_results$Variable, 
                              levels = c("hd30", "cdd", "r20mm"),
                              labels = c("Shock: Hot Days (hd30)", 
                                         "Shock: Drought (cdd)", 
                                         "Shock: Extreme Precipitation (r20mm)"))
print("Generating combined IRF plots...")

# Initializing a new graphic device window for rendering
x11() 
ggplot(lp_results, aes(x = Horizon, y = Effect)) +
  geom_line(color = "#004c99", size = 1.2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#004c99") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  # Arranging the plots into a 3-column facet grid for comparative visualization
  facet_wrap(~ Variable, ncol = 3, scales = "free_y") + 
  labs(
    title = "Local Projections (IRF): dynamic impact of climate shocks on GDP growth",
    subtitle = "( 2 Lags and 95% confidence region Driscoll-Kraay)",
    x = "Time Horizon (years)",
    y = "GDP response"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )

# Clearing the intermediate results dataframe to optimize memory
rm(list = "lp_results")

# Retaining the graphics.off() command commented out to prevent immediate window closure after rendering
# graphics.off()

# ==============================================================================
# PANEL VECTOR AUTOREGRESSION (OLS FIXED EFFECTS): LAG SELECTION AND OIRF
# ==============================================================================
library(dplyr)
library(panelvar)
library(vars)     
library(ggplot2)

# ==============================================================================
# PREPARING THE DATASET
# ==============================================================================

# Removing missing values to ensure perfectly balanced matrices for optimal OLS estimation
df_pvar <- df_intero %>%
  dplyr::select(Code, Name, Year, d_log_GDP, hd30, cdd, r20mm, d_hc, d_log_rnna, d_log_emp, log_infl) %>%
  drop_na()

# Sorting the panel and generating a deterministic linear time trend to control for 
# unobserved global macroeconomic shifts and technological progress
df_pvar <- df_pvar %>%
  arrange(Name, Year) %>%
  group_by(Name) %>%
  mutate(time_trend = row_number()) %>%
  ungroup()

# Casting the tibble to a standard dataframe to comply with the 'panelvar' package requirements
df_pure <- as.data.frame(df_pvar)

# Defining the endogenous vector strictly ordered according to the Wold causal chain (Cholesky identification),
# placing strictly exogenous physical climate shocks before macroeconomic responses
endogenous_ordered <- c("hd30", "cdd", "r20mm", "d_hc", "d_log_rnna", "d_log_emp", "d_log_GDP", "log_infl")

# Specifying the exogenous time trend variable to absorb non-stationary deterministic components
exogenous_vars <- "time_trend"

# ==============================================================================
# SELECTING THE OPTIMAL LAG ORDER VIA INFORMATION CRITERIA
# ==============================================================================
cat("COMPUTING INFORMATION CRITERIA FOR LAG SELECTION...\n")

# Simulating the 'demean' (Within) transformation manually to correctly compute information criteria,
# effectively partialling out country-specific fixed effects prior to VAR selection
df_demeaned <- df_pure %>%
  group_by(Name) %>%
  mutate(across(all_of(endogenous_ordered), ~ .x - mean(.x))) %>%
  ungroup() %>%
  dplyr::select(all_of(endogenous_ordered))

# Computing information criteria up to a maximum testable lag length
lag_criteria <- VARselect(as.matrix(df_demeaned), lag.max = 3, type = "none")

cat("==============================================================================\n")
cat(" LAG SELECTION CRITERIA (Lower scores indicate superior model fit) \n")
cat("==============================================================================\n")
print(lag_criteria$criteria)
cat("\nSuggested lags by respective criteria:\n")
print(lag_criteria$selection)
cat("==============================================================================\n")

# ==============================================================================
# ESTIMATING THE STRUCTURAL PANEL VAR MODEL (OLS)
# ==============================================================================

# Assigning the optimal lag order based on the Schwarz Criterion (BIC) penalty for parsimony
opt_lag <- 1 

cat(sprintf("\nESTIMATING THE PANEL VAR OLS (LAG = %d) VIA pvarfeols...\n", opt_lag))

pvar_ols_model <- pvarfeols(
  dependent_vars = endogenous_ordered,
  exog_vars      = exogenous_vars,     
  lags           = opt_lag,
  transformation = "demean",           
  data           = df_pure,
  panel_identifier = c("Name", "Year")
)

# ==============================================================================
# EXTRACTING ORTHOGONALIZED IMPULSE RESPONSE FUNCTIONS AND EXECUTING A MANUAL BLOCK BOOTSTRAP
# ==============================================================================

# Mapping the column indices corresponding to the target response variable (GDP growth) 
# and the structural climate shocks based on the Cholesky ordering
target_pos      <- 7  
shock_positions <- c(1, 2, 3) 
shock_names     <- c("hd30", "cdd", "r20mm")
horizon         <- 4 

# Adjusting the temporal axis since the VAR calculates the contemporaneous impact at t=0
periods         <- 0:(horizon - 1) 

cat("\nCALCULATING THE OIRF AND INITIATING THE MANUAL BLOCK BOOTSTRAP...\n")
irf_cholesky <- oirf(pvar_ols_model, n.ahead = horizon)

# Configuring the cross-sectional block bootstrap parameters
set.seed(42)
B <- 200 
countries <- unique(df_pure$Name)
N <- length(countries)

boot_results <- list(
  hd30  = matrix(NA, nrow = horizon, ncol = B),
  cdd   = matrix(NA, nrow = horizon, ncol = B),
  r20mm = matrix(NA, nrow = horizon, ncol = B)
)

# Executing the bootstrap replication loop, sampling countries with replacement 
# to preserve the intra-group temporal dependence
for (b in 1:B) {
  cat(sprintf("\rExecuting Bootstrap sample replication: %d/%d", b, B))
  utils::flush.console()
  
  sampled_units <- sample(countries, size = N, replace = TRUE)
  
  boot_data <- lapply(seq_along(sampled_units), function(i) {
    df_pure %>% 
      filter(Name == sampled_units[i]) %>%
      mutate(Name = paste0("Clone_", i))
  }) %>% bind_rows()
  
  tryCatch({
    boot_model <- pvarfeols(
      dependent_vars = endogenous_ordered,
      exog_vars      = exogenous_vars,
      lags           = opt_lag,
      transformation = "demean",
      data           = boot_data,
      panel_identifier = c("Name", "Year")
    )
    
    boot_irf <- oirf(boot_model, n.ahead = horizon)
    
    # Extracting the specific impulse-response coordinates: lists represent shocks, columns represent responses
    boot_results$hd30[, b]  <- as.numeric(boot_irf[[1]][, target_pos])
    boot_results$cdd[, b]   <- as.numeric(boot_irf[[2]][, target_pos])
    boot_results$r20mm[, b] <- as.numeric(boot_irf[[3]][, target_pos])
  }, error = function(e) {
    NULL
  })
}

cat("\nAnalytically calculating the 95% empirical quantiles...\n")
lower_hd30  <- apply(boot_results$hd30, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_hd30  <- apply(boot_results$hd30, 1, quantile, probs = 0.975, na.rm = TRUE)

lower_cdd   <- apply(boot_results$cdd, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_cdd   <- apply(boot_results$cdd, 1, quantile, probs = 0.975, na.rm = TRUE)

lower_r20mm <- apply(boot_results$r20mm, 1, quantile, probs = 0.025, na.rm = TRUE)
upper_r20mm <- apply(boot_results$r20mm, 1, quantile, probs = 0.975, na.rm = TRUE)

# ==============================================================================
# CONSTRUCTING THE PLOTTING DATAFRAME
# ==============================================================================

# Aggregating the point estimates and bootstrapped confidence bounds into a unified structure
df_irf_combined <- data.frame(
  Variable = rep(c("hd30", "cdd", "r20mm"), each = horizon),
  Years    = rep(periods, times = 3),
  
  # Applying the exact coordinate mapping for the point estimates
  Point_Estimate = c(as.numeric(irf_cholesky[[1]][, target_pos]),
                     as.numeric(irf_cholesky[[2]][, target_pos]),
                     as.numeric(irf_cholesky[[3]][, target_pos])),
  Lower_Bound    = c(lower_hd30, lower_cdd, lower_r20mm),
  Upper_Bound    = c(upper_hd30, upper_cdd, upper_r20mm)
)

# Formatting the variables as factors for aesthetically consistent faceting
df_irf_combined$Variable <- factor(df_irf_combined$Variable, 
                                   levels = c("hd30", "cdd", "r20mm"),
                                   labels = c("Shock: Giorni Caldi Estremi (hd30)", 
                                              "Shock: Siccità (cdd)", 
                                              "Shock: Precipitazioni Estreme (r20mm)"))

# ==============================================================================
# GENERATING THE MULTI-PANEL STRUCTURAL GRAPHIC
# ==============================================================================
cat("\nGENERATING THE THREE-PANEL STRUCTURAL GRAPHIC...\n")

x11() 
plot_var_combined <- ggplot(df_irf_combined, aes(x = Years, y = Point_Estimate)) +
  geom_ribbon(aes(ymin = Lower_Bound, ymax = Upper_Bound), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # Aligning the x-axis breaks with the corrected temporal periods
  scale_x_continuous(breaks = periods) + 
  facet_wrap(~ Variable, ncol = 3, scales = "free_y") + 
  theme_bw(base_size = 12) +
  labs(
    title = "Panel VAR OLS (Fixed Effects): Dynamic GDP response",
    subtitle = "(Cholesky Ordering - 95% Confidence Bands via Bootstrap)",
    x = "Time Horizon (Years)",
    y = "GDP response"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_var_combined)

path_clean <- "../DATA/clean_data"
ggsave(file.path(path_clean, "PVAR_OLS_Combined_GDP.pdf"), plot = plot_var_combined, width = 12, height = 5)

cat("Procedure finalized. The graph displays the isolated structural response of GDP growth.\n")

# ==============================================================================
# CLIMATE DATA INGESTION AND PREPROCESSING
# ==============================================================================
library(stringr)
library(readxl)

# Defining a custom function to systematically ingest and process raw annual climate data from Excel repositories
proc_clim_dir_annual_explicit <- function(dir_path) {
  
  # Scanning the specified directory for target Excel files
  f_list <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)
  print(paste("Trovati:", length(f_list), "file da processare"))
  
  # Initializing an empty list to aggregate the processed dataframes
  all_data_list <- list()
  
  # Iterating over each identified file
  for (f_path in f_list) {
    sh_list <- excel_sheets(f_path)
    
    # Iterating over each sheet within the current file
    for (sh_name in sh_list) {
      df_sh <- read_excel(f_path, sheet = sh_name)
      
      # Reshaping the data from wide to long format for standardized processing
      df_long_temp <- df_sh %>%
        pivot_longer(
          cols = -c(code, name), 
          names_to = "yr_mo", 
          values_to = "val"
        )
      
      # Extracting the numeric year from the time-stamp string
      df_long_temp$yr <- as.numeric(str_sub(df_long_temp$yr_mo, 1, 4))
      df_long_temp$ind <- sh_name
      
      # Retaining only the first observation per group to eliminate duplicated entries
      df_clean_temp <- df_long_temp %>%
        dplyr::select(code, name, yr, ind, val) %>%
        arrange(code, name, yr) %>% 
        distinct(code, name, yr, ind, .keep_all = TRUE)
      
      # Appending the cleaned temporal data to the aggregate list
      all_data_list[[length(all_data_list) + 1]] <- df_clean_temp
    }
  }
  
  # Assembling all localized dataframes into a unified longitudinal structure
  df_combined <- bind_rows(all_data_list)
  
  print("Struttura dei dati combinati prima del pivot:")
  print(str(df_combined))
  
  # Pivoting the assembled dataset back to a wide panel format and standardizing country codes
  df_pnl_clean <- df_combined %>%
    pivot_wider(names_from = ind, values_from = val) %>%
    mutate(Code = toupper(code)) %>%
    rename(Year = yr, Name = name) %>%
    dplyr::select(Code, Name, Year, everything()) %>%
    dplyr::select(-code)
  
  return(df_pnl_clean)
}

path_climate <- "../DATA/ANNUAL_RAW"
df_climate <- proc_clim_dir_annual_explicit(path_climate)

# ==============================================================================
# DATA TYPE STANDARDIZATION AND MISSING VALUE IMPUTATION
# ==============================================================================

# Casting columns to correct data types and imputing missing climate anomaly values with zero, 
# reflecting a zero-shock baseline assumption for missing records
df_climate_clean <- df_climate %>%
  mutate(
    Code = as.character(Code), 
    Name = as.character(Name), 
    Year = as.numeric(Year)
  ) %>%
  mutate(across(any_of(c("hd30", "cdd", "r20mm")), ~ replace_na(.x, 0)))

# Executing a robust left join to integrate the newly processed climate variables with the primary 
# macroeconomic panel, strictly matching by ISO code and year to prevent misalignment
df_mix <- df_pvar %>%
  dplyr::mutate(Code = as.character(Code), Name = as.character(Name), Year = as.numeric(Year)) %>%
  dplyr::select(-any_of(c("hd30", "cdd", "r20mm"))) %>%
  left_join(df_climate_clean %>% dplyr::select(-Name), by = c("Code", "Year")) %>%
  dplyr::select(Code, Name, Year, everything()) %>%
  dplyr::arrange(Code, Year)

# ==============================================================================
# FIXED SCENARIOS PROCESSING: HARDCODED SHEET MAPPING TO PREVENT STRING BUGS
# ==============================================================================
library(purrr)

# Defining a robust parsing function to extract IPCC long-term climate scenarios
proc_scenarios_dir_fixed <- function(dir_path) {
  
  f_list <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)
  cat(sprintf("Found %d scenario files to process.\n", length(f_list)))
  
  process_scenario_file <- function(f_path) {
    sh_list <- excel_sheets(f_path)
    
    map(sh_list, function(sh_name) {
      # Explicitly mapping sheet names to their corresponding SSP scenarios to bypass string parsing anomalies
      scen_name <- ifelse(str_detect(sh_name, "ssp245"), "ssp245", "ssp370")
      ind_name  <- str_remove(sh_name, "_ssp245|_ssp370")
      
      read_excel(f_path, sheet = sh_name) %>%
        pivot_longer(
          cols = -c(code, name), 
          names_to = "yr_mo", 
          values_to = "val"
        ) %>%
        mutate(
          Year = as.numeric(str_sub(yr_mo, 1, 4)),
          Indicator = ind_name,
          Scenario = scen_name
        ) %>%
        dplyr::select(code, name, Year, Indicator, Scenario, val)
    }) %>% 
      bind_rows()
  }
  
  cat("Extracting scenario data with fixed string matching...\n")
  df_scen_long <- map(f_list, process_scenario_file) %>% 
    bind_rows()
  
  # Structuring the extracted scenario data into a wide panel format using the clean indicators
  df_scen_wide <- df_scen_long %>%
    distinct(code, name, Year, Indicator, Scenario, .keep_all = TRUE) %>%
    pivot_wider(names_from = Indicator, values_from = val) %>%
    mutate(
      Code = as.character(toupper(code)),
      Name = as.character(name)
    ) %>%
    dplyr::select(Code, Name, Year, Scenario, everything(), -code, -name)
  
  return(df_scen_wide)
}

path_scenarios <- "../DATA/ipcc_scenarios"

# Executing the bug-free scenario processing
df_scenarios <- proc_scenarios_dir_fixed(path_scenarios)

# Imputing missing future climate values with zero to represent baseline atmospheric conditions
df_scenarios <- df_scenarios %>%
  mutate(across(any_of(c("hd30", "cdd", "r20mm")), ~ replace_na(.x, 0)))

cat("Splitting and recombining final forecasting dataframes...\n")

# ==============================================================================
# ISOLATING HISTORICAL BASELINES AND PROJECTED SCENARIOS
# ==============================================================================

# Slicing the integrated historical panel to isolate the out-of-sample testing window
df_test            <- df_mix %>% filter(Year >= 2015)
df_historical_base <- df_mix %>% filter(Year < 2015)

# Isolating the target IPCC scenarios into dedicated dataframes
df_scen_245 <- df_scenarios %>% filter(Scenario == "ssp245") %>% dplyr::select(-Scenario)
df_scen_370 <- df_scenarios %>% filter(Scenario == "ssp370") %>% dplyr::select(-Scenario)

# Identifying the exact cross-sectional sample utilized in the historical estimations
iso_target <- unique(df_mix$Code)

# Filtering the projected scenarios to strictly match the established macroeconomic panel
df_scen_245 <- df_scen_245 %>% filter(Code %in% iso_target)
df_scen_370 <- df_scen_370 %>% filter(Code %in% iso_target)

# ==============================================================================
# ALIGNING CROSS-SECTIONAL IDENTIFIERS FOR CONTIGUOUS MERGING
# ==============================================================================

# Extracting a definitive dictionary mapping ISO codes to standardized country names
dizionario_nomi_storici <- df_mix %>%
  distinct(Code, Name)

# Overwriting potentially misaligned country names in the IPCC datasets utilizing the historical dictionary 
# to ensure perfect contiguous merging along the temporal axis
df_scen_245 <- df_scen_245 %>%
  dplyr::select(-any_of("Name")) %>%
  left_join(dizionario_nomi_storici, by = "Code") %>%
  dplyr::relocate(Name, .after = Code) 

df_scen_370 <- df_scen_370 %>%
  dplyr::select(-any_of("Name")) %>%
  left_join(dizionario_nomi_storici, by = "Code") %>%
  dplyr::relocate(Name, .after = Code)

# Assembling the final contiguous panel structures spanning from the historical origin to the terminal projection year
df_ssp245 <- bind_rows(df_historical_base, df_scen_245) %>% dplyr::arrange(Code, Year)
df_ssp370 <- bind_rows(df_historical_base, df_scen_370) %>% dplyr::arrange(Code, Year)

# ==============================================================================
# FINAL DATA INTEGRITY VERIFICATION
# ==============================================================================

cat("Verifica del numero di paesi unici nel dataset finale:\n")
cat(" -> Numero di CODICI unici in df_ssp245:", length(unique(df_ssp245$Code)), "\n")
cat(" -> Numero di NOMI unici in df_ssp245:  ", length(unique(df_ssp245$Name)), "\n")

# ==============================================================================
# CLIMATE SHOCK QUANTIFICATION AND TEMPORAL TREND EXTENSION
# ==============================================================================

# Defining a function to compute climate anomalies (shocks) relative to historical cumulative means,
# ensuring the isolation of short-term weather variability from underlying climatic trends
calc_clim_shocks <- function(df_input) {
  
  df_shocks <- df_input %>%
    group_by(Code) %>%
    # Sorting by year to ensure the 1950 observation serves as the initial anchor
    arrange(Year, .by_group = TRUE) %>% 
    mutate(
      # Utilizing dplyr::lag to enforce missing values for the first year of each country
      hd30_base  = dplyr::lag(cummean(hd30)),
      cdd_base   = dplyr::lag(cummean(cdd)),
      r20mm_base = dplyr::lag(cummean(r20mm)),
      
      # Calculating the climate shock as the deviation from the cumulative historical mean
      hd30_shock  = hd30 - hd30_base,
      cdd_shock   = cdd - cdd_base,
      r20mm_shock = r20mm - r20mm_base
    ) %>%
    # Physically discarding the initial observation (1950) due to lack of lagged reference
    slice(-1) %>%
    ungroup() %>%
    # Filtering out sparsely populated years where shock computation was infeasible
    filter(!is.na(hd30_shock) & !is.na(cdd_shock) & !is.na(r20mm_shock)) %>%
    # Overwriting the original climate variables with their calculated anomaly counterparts
    mutate(
      hd30  = hd30_shock,
      cdd   = cdd_shock,
      r20mm = r20mm_shock
    ) %>%
    dplyr::select(-ends_with("_base"), -ends_with("_shock"))
  
  return(df_shocks)
}

# Executing the climate shock transformation on both scenario datasets
df_ssp245_clean <- calc_clim_shocks(df_ssp245)
df_ssp370_clean <- calc_clim_shocks(df_ssp370)

# ==============================================================================
# EXTENDING THE DETERMINISTIC TIME TREND
# ==============================================================================

cat("Extending the deterministic time trend for the scenario datasets...\n")

# Re-calculating the linear time trend to ensure complete coverage of both historical 
# and future projection horizons up to 2100
df_ssp245_clean <- df_ssp245_clean %>%
  group_by(Code) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(time_trend = row_number()) %>%
  ungroup()

df_ssp370_clean <- df_ssp370_clean %>%
  group_by(Code) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(time_trend = row_number()) %>%
  ungroup()

cat("Time trend successfully integrated; datasets are prepared for dynamic_pvar_forecast.\n")

# ==============================================================================
# PANEL VAR FORECASTING CONFIGURATION
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)

# Specifying the endogenous and exogenous system variables based on the established Cholesky identification
endogenous_forecasting <- c("d_hc", "d_log_rnna", "d_log_emp", "d_log_GDP", "log_infl")
exogenous_forecasting  <- c("time_trend", "hd30", "cdd", "r20mm")

# ==============================================================================
# ONE-STEP-AHEAD RECURSIVE WINDOW FORECASTING ENGINE
# ==============================================================================

# Defining the expanding-window forecasting engine to evaluate out-of-sample predictive performance
recursive_onestep_pvar_forecast <- function(df_full_timeline, endog_names, exog_names, start_year, end_year) {
  
  results_list <- list()
  cat(sprintf("\nInitializing the Expanding Window Forecast (One-Step-Ahead) from %d to %d...\n", start_year, end_year))
  
  # Iterating through the forecast horizon with a recursively expanding training set
  for (yr in start_year:end_year) {
    cat(sprintf(" -> Training model through %d... Generating prediction for %d\n", yr - 1, yr))
    
    # Subsetting the training timeline
    df_train <- df_full_timeline %>% filter(Year < yr) %>% as.data.frame()
    
    # Estimating the PVAR model utilizing fixed effects transformation (demeaning)
    step_model <- pvarfeols(
      dependent_vars   = endog_names,
      exog_vars        = exog_names,
      lags             = 1,
      transformation   = "demean",
      data             = df_train,
      panel_identifier = c("Name", "Year")
    )
    
    coef_matrix        <- coef(step_model)
    dep_names_matrix   <- rownames(coef_matrix)  
    indep_names_matrix <- colnames(coef_matrix)  
    
    # Calculating country-specific historical means for fixed effect restoration
    country_means <- df_train %>%
      group_by(Code) %>%
      summarise(across(all_of(c(endog_names, exog_names)), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
      rename_with(~ paste0(.x, "_mean"), -Code)
    
    # Extracting the most recent real lags (T-1) to initialize the one-step-ahead forecast
    df_lags_reali <- df_train %>%
      filter(Year == (yr - 1)) %>%
      dplyr::select(Code, all_of(endog_names)) %>%
      rename_with(~ paste0(.x, "_lag"), all_of(endog_names))
    
    # Extracting exogenous variables for the target year (T)
    df_exog_target <- df_full_timeline %>%
      filter(Year == yr) %>%
      dplyr::select(Code, Year, all_of(exog_names))
    
    # Merging input data for structural prediction
    step_data <- df_exog_target %>%
      left_join(df_lags_reali,  by = "Code") %>%
      left_join(country_means,  by = "Code") %>%
      drop_na()
    
    predicted_endog <- step_data %>% dplyr::select(Code, Year)
    
    # Generating point forecasts for each endogenous variable
    for (dep_var in endog_names) {
      pred_value_demeaned <- rep(0, nrow(step_data))
      
      # Identifying the structural position of the dependent variable
      idx_dep <- which(dep_names_matrix == paste0("demeaned_", dep_var))[1]
      if (is.na(idx_dep)) {
        warning(sprintf("Variabile dipendente '%s' non trovata nella matrice dei coefficienti. Salto.", dep_var))
        next
      }
      
      # Computing autoregressive contributions
      for (lag_var in endog_names) {
        lag_col_name <- paste0("demeaned_lag1_", lag_var)
        idx_indep    <- which(indep_names_matrix == lag_col_name)[1]
        
        if (!is.na(idx_indep)) {
          weight              <- coef_matrix[idx_dep, idx_indep]
          valore_demeaned     <- step_data[[paste0(lag_var, "_lag")]] - step_data[[paste0(lag_var, "_mean")]]
          pred_value_demeaned <- pred_value_demeaned + (weight * valore_demeaned)
        }
      }
      
      # Computing exogenous contributions
      for (exo_var in exog_names) {
        exo_col_name <- paste0("demeaned_", exo_var)
        idx_indep    <- which(indep_names_matrix == exo_col_name)[1]
        
        if (!is.na(idx_indep)) {
          weight              <- coef_matrix[idx_dep, idx_indep]
          valore_demeaned     <- step_data[[exo_var]] - step_data[[paste0(exo_var, "_mean")]]
          pred_value_demeaned <- pred_value_demeaned + (weight * valore_demeaned)
        }
      }
      
      # Restoring fixed effects to generate the final prediction in the original scale
      predicted_endog[[dep_var]] <- pred_value_demeaned + step_data[[paste0(dep_var, "_mean")]]
    }
    
    results_list[[as.character(yr)]] <- predicted_endog
  }
  
  cat("\nRecursive window forecasting successfully completed (Fixed Effects activated)!\n")
  return(bind_rows(results_list))
}

# ==============================================================================
# OUT-OF-SAMPLE PERFORMANCE EVALUATION (2015-2023)
# ==============================================================================

cat("Generating out-of-sample forecasts for the test set (2015-2023)...\n")

# Recombining historical and test data into a unified temporal timeline
df_continuous_timeline <- bind_rows(df_historical_base, df_test) %>%
  arrange(Code, Year)

# Recalculating climate shocks over the extended continuous timeline
df_continuous_shocks <- calc_clim_shocks(df_continuous_timeline)

# Retrieving the historical dictionary mapping ISO codes to country names
dizionario_nomi_storici <- df_mix %>%
  distinct(Code, Name)

# Aligning cross-sectional identifiers in the continuous shocks dataframe
df_continuous_shocks <- df_continuous_shocks %>%
  dplyr::select(-any_of("Name")) %>%
  left_join(dizionario_nomi_storici, by = "Code") %>%
  dplyr::relocate(Name, .after = Code)

# Performing final data integrity checks
cat("Verifying alignment of df_continuous_shocks:\n")
cat(" -> Unique codes in df_continuous_shocks:", length(unique(df_continuous_shocks$Code)), "\n")
cat(" -> Unique names in df_continuous_shocks: ", length(unique(df_continuous_shocks$Name)), "\n")

# Partitioning the dataset into training and out-of-sample evaluation segments
df_historical_base_clean <- df_continuous_shocks %>% filter(Year < 2015)
df_test_clean            <- df_continuous_shocks %>% filter(Year >= 2015)

cat("Generating out-of-sample projections via recursive window forecasting...\n")

# Executing the recursive one-step-ahead forecasting engine
df_forecast_recursive <- recursive_onestep_pvar_forecast(
  df_full_timeline = df_continuous_shocks,
  endog_names      = endogenous_forecasting,
  exog_names       = exogenous_forecasting,
  start_year       = 2015,
  end_year         = 2023
)

# Merging point forecasts with realized GDP growth values for accuracy assessment
eval_df_recursive <- df_forecast_recursive %>%
  dplyr::select(Code, Year, pred_gdp = d_log_GDP) %>%
  left_join(
    df_continuous_shocks %>% dplyr::select(Code, Year, actual_gdp = d_log_GDP),
    by = c("Code", "Year")
  ) %>%
  drop_na()

# Diagnosing the forecast sample properties
cat(sprintf("\n[DIAGNOSTICS] Total observations: %d\n",   nrow(eval_df_recursive)))
cat(sprintf("[DIAGNOSTICS] Range of actual GDP: [%.4f, %.4f]\n", min(eval_df_recursive$actual_gdp), max(eval_df_recursive$actual_gdp)))
cat(sprintf("[DIAGNOSTICS] Range of predicted GDP: [%.4f, %.4f]\n", min(eval_df_recursive$pred_gdp),   max(eval_df_recursive$pred_gdp)))

# Calculating standard forecast performance metrics
mse_rec   <- mean((eval_df_recursive$actual_gdp - eval_df_recursive$pred_gdp)^2)
mae_rec   <- mean(abs(eval_df_recursive$actual_gdp - eval_df_recursive$pred_gdp))
smape_rec <- mean(
  2 * abs(eval_df_recursive$actual_gdp - eval_df_recursive$pred_gdp) /
    (abs(eval_df_recursive$actual_gdp) + abs(eval_df_recursive$pred_gdp))
) * 100

ss_res  <- sum((eval_df_recursive$actual_gdp - eval_df_recursive$pred_gdp)^2)
ss_tot  <- sum((eval_df_recursive$actual_gdp - mean(eval_df_recursive$actual_gdp))^2)
r2_oos  <- 1 - (ss_res / ss_tot)

cat("\n==============================================================================\n")
cat(" OUT-OF-SAMPLE PERFORMANCE (ONE-STEP RECURSIVE WINDOW: 2015-2023) \n")
cat("==============================================================================\n")
cat(sprintf(" Mean Squared Error  (MSE):      %.6f\n", mse_rec))
cat(sprintf(" Mean Absolute Error (MAE):      %.6f\n", mae_rec))
cat(sprintf(" Symmetric MAPE      (sMAPE):    %.2f %%\n", smape_rec))
cat(sprintf(" R² Out-of-Sample    (R²_oos):   %.4f\n", r2_oos))
cat("==============================================================================\n\n")

# Isolating the influence of the 2020 pandemic shock on aggregate forecast accuracy
eval_no2020 <- eval_df_recursive %>% filter(Year != 2020)

ss_res_no20 <- sum((eval_no2020$actual_gdp - eval_no2020$pred_gdp)^2)
ss_tot_no20 <- sum((eval_no2020$actual_gdp - mean(eval_no2020$actual_gdp))^2)
r2_no2020   <- 1 - (ss_res_no20 / ss_tot_no20)

cat(sprintf("R²_oos excluding 2020: %.4f\n", r2_no2020))
cat(sprintf("MAE excluding 2020: %.6f\n", mean(abs(eval_no2020$actual_gdp - eval_no2020$pred_gdp))))

# Analyzing annual performance to identify temporal forecast deterioration
eval_df_recursive %>%
  group_by(Year) %>%
  summarise(
    MAE  = mean(abs(actual_gdp - pred_gdp)),
    BIAS = mean(pred_gdp - actual_gdp),  
    N    = n()
  ) %>%
  print(n = Inf)

# Plotting panel-wide average realized versus predicted GDP growth
eval_df_recursive %>%
  group_by(Year) %>%
  summarise(
    actual_mean = mean(actual_gdp),
    pred_mean   = mean(pred_gdp)
  ) %>%
  tidyr::pivot_longer(-Year, names_to = "tipo", values_to = "valore") %>%
  ggplot(aes(x = Year, y = valore, color = tipo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("actual_mean" = "steelblue", "pred_mean" = "firebrick"),
                     labels = c("Actual (Panel Mean)", "Predicted (Panel Mean)")) +
  theme_minimal() +
  labs(title = "Out-of-Sample: Actual vs. Predicted (Panel Mean)",
       subtitle = "Divergence in 2020 highlights the systemic impact of the pandemic shock",
       x = "Year", y = "d_log_GDP", color = NULL)

# Re-evaluating performance metrics excluding the 2020-2021 pandemic cycle
eval_no_covid <- eval_df_recursive %>% filter(!Year %in% c(2020, 2021))

ss_res_nc <- sum((eval_no_covid$actual_gdp - eval_no_covid$pred_gdp)^2)
ss_tot_nc <- sum((eval_no_covid$actual_gdp - mean(eval_no_covid$actual_gdp))^2)

cat(sprintf("R²_oos excluding 2020-2021: %.4f\n", 1 - ss_res_nc / ss_tot_nc))
cat(sprintf("MAE excluding 2020-2021: %.6f\n", mean(abs(eval_no_covid$actual_gdp - eval_no_covid$pred_gdp))))

# ==============================================================================
# LONG-TERM MULTI-STEP FORECASTING ENGINE (2024-2100)
# ==============================================================================

# Defining the multi-step dynamic projection function
dynamic_pvar_forecast <- function(df_future, df_history, coef_vector,
                                  endog_names, exog_names,
                                  start_year, end_year) {
  
  cat(sprintf("  Initializing multi-step dynamic forecast: %d -> %d\n", start_year, end_year))
  
  dep_names_matrix   <- rownames(coef_vector)
  indep_names_matrix <- colnames(coef_vector)
  
  # Calculating country-specific historical means for fixed effect anchoring
  country_means <- df_history %>%
    group_by(Code) %>%
    summarise(
      across(all_of(c(endog_names, exog_names)), \(x) mean(x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rename_with(~ paste0(.x, "_mean"), -Code)
  
  # Extracting real T-1 values (e.g., 2023) as initial lags
  lag_correnti <- df_history %>%
    filter(Year == (start_year - 1)) %>%
    dplyr::select(Code, all_of(endog_names)) %>%
    rename_with(~ paste0(.x, "_lag"), all_of(endog_names))
  
  results_list <- list()
  
  # Iterating through the projection horizon
  for (yr in start_year:end_year) {
    
    df_exog_yr <- df_future %>%
      filter(Year == yr) %>%
      dplyr::select(Code, Year, all_of(exog_names))
    
    step_data <- df_exog_yr %>%
      left_join(lag_correnti,  by = "Code") %>%
      left_join(country_means, by = "Code") %>%
      drop_na()
    
    if (nrow(step_data) == 0) {
      cat(sprintf("  [WARN] No data available for year %d. Skipping.\n", yr))
      next
    }
    
    predicted_endog <- step_data %>% dplyr::select(Code, Year)
    
    # Computing projections iteratively
    for (dep_var in endog_names) {
      pred_value_demeaned <- rep(0, nrow(step_data))
      
      idx_dep <- which(dep_names_matrix == paste0("demeaned_", dep_var))[1]
      if (is.na(idx_dep)) next
      
      # Autoregressive components (Lag 1)
      for (lag_var in endog_names) {
        idx_indep <- which(indep_names_matrix == paste0("demeaned_lag1_", lag_var))[1]
        
        if (!is.na(idx_indep)) {
          weight          <- coef_vector[idx_dep, idx_indep]
          valore_demeaned <- step_data[[paste0(lag_var, "_lag")]] - step_data[[paste0(lag_var, "_mean")]]
          pred_value_demeaned <- pred_value_demeaned + (weight * valore_demeaned)
        }
      }
      
      # Exogenous components
      for (exo_var in exog_names) {
        idx_indep <- which(indep_names_matrix == paste0("demeaned_", exo_var))[1]
        
        if (!is.na(idx_indep)) {
          weight          <- coef_vector[idx_dep, idx_indep]
          valore_demeaned <- step_data[[exo_var]] - step_data[[paste0(exo_var, "_mean")]]
          pred_value_demeaned <- pred_value_demeaned + (weight * valore_demeaned)
        }
      }
      
      # Restoring fixed effects to generate projections in the original scale
      predicted_endog[[dep_var]] <- pred_value_demeaned + step_data[[paste0(dep_var, "_mean")]]
    }
    
    results_list[[as.character(yr)]] <- predicted_endog
    
    # Dynamically updating lags: projections for T become the lags for T+1
    lag_correnti <- predicted_endog %>%
      dplyr::select(Code, all_of(endog_names)) %>%
      rename_with(~ paste0(.x, "_lag"), all_of(endog_names))
    
    cat(sprintf("  -> Year %d completed (%d countries)\n", yr, nrow(predicted_endog)))
  }
  
  df_out <- bind_rows(results_list)
  cat(sprintf("  Dynamic forecasting completed: %d total observations\n", nrow(df_out)))
  return(df_out)
}

# ==============================================================================
# STRUCTURAL PANEL VAR OLS ESTIMATION AND MULTI-STEP FORECASTING
# ==============================================================================

cat("Estimating the definitive structural Panel VAR OLS model over the entire historical series (1960-2023)...\n")

# Extracting the structural coefficients estimated through 2023 to maximize predictive precision
pvar_definitivo <- pvarfeols(
  dependent_vars   = endogenous_forecasting,
  exog_vars        = exogenous_forecasting, 
  lags             = 1,                     
  transformation   = "demean",
  data             = as.data.frame(df_continuous_shocks), 
  panel_identifier = c("Name", "Year")
)

# Saving the updated structural coefficient vectors
model_coefs_definitivi <- coef(pvar_definitivo)

# ==============================================================================
# LONG-TERM CLIMATE SCENARIO FORECASTING (2024 - 2100)
# ==============================================================================

# Defining the projection horizon for the climate scenarios
anno_inizio_forecast <- 2024
anno_fine_forecast   <- 2100

# Executing long-term dynamic forecasting for the SSP2-4.5 scenario
cat("\nExecuting long-term dynamic forecasting for SSP245 (2024-2100)...\n")

# Constraining the horizon to the minimum between the IPCC terminal year and 2100
max_year_245 <- min(max(df_ssp245_clean$Year, na.rm = TRUE), anno_fine_forecast)

# Generating dynamic multi-step projections
df_ssp245_forecast <- dynamic_pvar_forecast(
  df_future   = df_ssp245_clean,
  df_history  = df_continuous_shocks, # Anchoring to realized 2023 lags
  coef_vector = model_coefs_definitivi,
  endog_names = endogenous_forecasting,
  exog_names  = exogenous_forecasting,
  start_year  = anno_inizio_forecast,
  end_year    = max_year_245
)

# Executing long-term dynamic forecasting for the SSP3-7.0 scenario
cat("Executing long-term dynamic forecasting for SSP370 (2024-2100)...\n")

max_year_370 <- min(max(df_ssp370_clean$Year, na.rm = TRUE), anno_fine_forecast)

df_ssp370_forecast <- dynamic_pvar_forecast(
  df_future   = df_ssp370_clean,
  df_history  = df_continuous_shocks, # Anchoring to realized 2023 lags
  coef_vector = model_coefs_definitivi,
  endog_names = endogenous_forecasting,
  exog_names  = exogenous_forecasting,
  start_year  = anno_inizio_forecast,
  end_year    = max_year_370
)

cat("\n==============================================================================\n")
cat(" SCENARIO PROJECTIONS SUCCESSFULLY GENERATED (2024-2100) \n")
cat("==============================================================================\n")

# ==============================================================================
# PROBABILISTIC OUT-OF-SAMPLE FORECASTING (PARALLELIZED)
# ==============================================================================

library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(foreach)     
library(doParallel)  

# Defining the pinball loss function to evaluate the quality of the probabilistic intervals
calculate_interval_pinball_loss <- function(actual, lower_bound, upper_bound) {
  tau_lower <- 0.025
  tau_upper <- 0.975
  error_lower <- actual - lower_bound
  error_upper <- actual - upper_bound
  loss_lower <- ifelse(error_lower >= 0, tau_lower * error_lower, (tau_lower - 1) * error_lower)
  loss_upper <- ifelse(error_upper >= 0, tau_upper * error_upper, (tau_upper - 1) * error_upper)
  total_loss <- mean(loss_lower + loss_upper, na.rm = TRUE)
  return(total_loss)
}

# Initializing parallel processing to accelerate cross-sectional expanding-window estimations
countries <- unique(df_historical_base_clean$Code)
num_cores <- parallel::detectCores() - 1 

cat(sprintf("Initializing parallel cluster on %d cores...\n", num_cores))
cat("Methodology: Expanding Window One-Step-Ahead\n")

cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Executing the expanding-window forecasting loop in parallel
parallel_results <- foreach(
  country_code = countries,
  .packages = c("dplyr", "tidyr", "forecast", "ggplot2"), 
  .export = c("df_historical_base_clean", "df_test_clean", "calculate_interval_pinball_loss") 
) %dopar% {
  
  # Extracting country-specific longitudinal data
  hist_data <- df_historical_base_clean %>% filter(Code == country_code) %>% arrange(Year) %>% drop_na(d_log_GDP, hd30, cdd, r20mm)
  test_data <- df_test_clean %>% filter(Code == country_code) %>% arrange(Year) %>% drop_na(d_log_GDP, hd30, cdd, r20mm)
  
  train_full <- hist_data
  train_cp   <- hist_data %>% filter(Year <= 2000)
  calib_cp   <- hist_data %>% filter(Year > 2000 & Year < 2015)
  
  # Skipping cross-sections with insufficient historical depth
  if(nrow(train_cp) < 20 | nrow(calib_cp) < 10 | nrow(test_data) == 0) {
    return(NULL)
  }
  
  full_country_data <- bind_rows(hist_data, test_data) %>% arrange(Year)
  test_years <- test_data$Year
  y_test <- test_data$d_log_GDP
  
  # Estimating parametric and conformal hyperparameters
  y_train_full <- train_full$d_log_GDP
  x_train_full <- as.matrix(train_full[, c("hd30", "cdd", "r20mm")])
  model_std_base <- auto.arima(y_train_full, xreg = x_train_full, stationary = TRUE, seasonal = FALSE, ic = "aicc", stepwise = FALSE, approximation = FALSE)
  
  y_train_cp <- train_cp$d_log_GDP
  x_train_cp <- as.matrix(train_cp[, c("hd30", "cdd", "r20mm")])
  y_calib_cp <- calib_cp$d_log_GDP
  x_calib_cp <- as.matrix(calib_cp[, c("hd30", "cdd", "r20mm")])
  model_cp_base <- auto.arima(y_train_cp, xreg = x_train_cp, stationary = TRUE, seasonal = FALSE, ic = "aicc", stepwise = FALSE, approximation = FALSE)
  
  # Calibrating the conformal prediction threshold
  calib_fit <- Arima(y_calib_cp, model = model_cp_base, xreg = x_calib_cp)
  scores_non_conformity <- abs(y_calib_cp - as.numeric(fitted(calib_fit)))
  alpha <- 0.05
  n_calib <- length(scores_non_conformity)
  quantile_prob <- min(ceiling((n_calib + 1) * (1 - alpha)) / n_calib, 1.0)
  q_threshold <- as.numeric(quantile(scores_non_conformity, probs = quantile_prob))
  
  # Running the expanding window forecast loop
  pred_std_mean <- numeric(length(test_years))
  pred_std_lower <- numeric(length(test_years))
  pred_std_upper <- numeric(length(test_years))
  pred_cp_mean <- numeric(length(test_years))
  
  for (i in seq_along(test_years)) {
    t_yr <- test_years[i]
    current_train <- full_country_data %>% filter(Year < t_yr)
    y_curr <- current_train$d_log_GDP
    x_curr <- as.matrix(current_train[, c("hd30", "cdd", "r20mm")])
    
    current_test <- full_country_data %>% filter(Year == t_yr)
    x_target <- as.matrix(current_test[, c("hd30", "cdd", "r20mm")])
    
    step_model_std <- tryCatch(update(model_std_base, x = y_curr, xreg = x_curr), error = function(e) model_std_base)
    step_model_cp  <- tryCatch(update(model_cp_base, x = y_curr, xreg = x_curr), error = function(e) model_cp_base)
    
    fc_std <- forecast(step_model_std, h = 1, xreg = x_target)
    fc_cp  <- forecast(step_model_cp, h = 1, xreg = x_target)
    
    pred_std_mean[i]  <- as.numeric(fc_std$mean)
    pred_std_lower[i] <- as.numeric(fc_std$lower[, 2])
    pred_std_upper[i] <- as.numeric(fc_std$upper[, 2])
    pred_cp_mean[i]   <- as.numeric(fc_cp$mean)
  }
  
  # Compiling performance metrics and generating diagnostic visualizations
  df_plot_std <- data.frame(Year = test_years, Actual = y_test, Forecast = pred_std_mean, Lower_95 = pred_std_lower, Upper_95 = pred_std_upper)
  df_plot_cp  <- data.frame(Year = test_years, Actual = y_test, Point_Forecast = pred_cp_mean, Conformal_Lower = pred_cp_mean - q_threshold, Conformal_Upper = pred_cp_mean + q_threshold)
  
  pinball_std <- calculate_interval_pinball_loss(df_plot_std$Actual, df_plot_std$Lower_95, df_plot_std$Upper_95)
  pinball_cp  <- calculate_interval_pinball_loss(df_plot_cp$Actual, df_plot_cp$Conformal_Lower, df_plot_cp$Conformal_Upper)
  
  # Packaging the output for the master process
  return(list(
    code = country_code,
    metrics = data.frame(Code = country_code, Pinball_Loss_Standard = pinball_std, Pinball_Loss_Conformal = pinball_cp),
    plot1 = ggplot(df_plot_std, aes(x = Year)) + geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightblue", alpha = 0.5) + geom_line(aes(y = Forecast)) + theme_minimal(),
    plot2 = ggplot(df_plot_cp, aes(x = Year)) + geom_ribbon(aes(ymin = Conformal_Lower, ymax = Conformal_Upper), fill = "mediumseagreen", alpha = 0.3) + geom_line(aes(y = Point_Forecast)) + theme_minimal()
  ))
}

stopCluster(cl)

# ==============================================================================
# POST-PROCESSING: PDF EXPORT AND PERFORMANCE METRIC AGGREGATION
# ==============================================================================

# Filtering out non-convergent models from the parallel processing results
parallel_results <- Filter(Negate(is.null), parallel_results)
graphics.off()

# Generating a unified PDF report summarizing the probabilistic forecasting results
cat("Exporting unified PDF report...\n")
pdf("../DATA/clean_data/Comparative_Forecast_ARMAX_vs_Conformal_2015_2023.pdf", width = 12, height = 10)

# Iterating through the valid results to arrange and render diagnostic plots
for (res in parallel_results) {
  grid.arrange(res$plot1, res$plot2, ncol = 1)
  cat(sprintf("Saving plot for: %s\n", res$code))
}
dev.off()
cat("PDF report successfully exported.\n")

# Aggregating probabilistic Pinball Loss metrics into a consolidated dataframe
list_evaluation_metrics <- lapply(parallel_results, function(x) x$metrics)

# Calculating summary statistics to evaluate the performance of Conformal vs. Standard intervals
df_pinball_results <- bind_rows(list_evaluation_metrics) %>%
  mutate(
    Winning_Model = ifelse(Pinball_Loss_Conformal < Pinball_Loss_Standard, "Conformal", "Standard"),
    Improvement_Margin = Pinball_Loss_Standard - Pinball_Loss_Conformal
  )

summary_comparison <- df_pinball_results %>%
  summarise(
    Avg_Pinball_Standard  = mean(Pinball_Loss_Standard, na.rm = TRUE),
    Avg_Pinball_Conformal = mean(Pinball_Loss_Conformal, na.rm = TRUE),
    Conformal_Wins        = sum(Winning_Model == "Conformal", na.rm = TRUE),
    Standard_Wins         = sum(Winning_Model == "Standard", na.rm = TRUE)
  )

cat("\n==============================================================================\n")
cat(" OVERALL PROBABILISTIC FORECASTING PERFORMANCE (PARALLELIZED) \n")
cat("==============================================================================\n")
print(summary_comparison)
cat("==============================================================================\n")

# ==============================================================================
# COMPARATIVE OUT-OF-SAMPLE PERFORMANCE EVALUATION (2015-2023)
# Evaluating FE vs LME vs PVAR vs ARMAX across the full cross-sectional panel
# ==============================================================================

library(dplyr)
library(plm)
library(nlme)
library(forecast)
library(ggplot2)
library(tidyr)

anni_test    <- 2015:2023
paesi_tutti  <- unique(df_continuous_shocks$Name)

# Initializing a global dataframe to collect comparative model predictions
df_confronto <- data.frame()

# Iterating through each country and year to generate out-of-sample forecasts
for (paese in paesi_tutti) {
  for (anno in anni_test) {
    
    # Defining training and testing subsets
    train_panel <- df_continuous_shocks %>% filter(Year < anno)
    train_paese <- train_panel %>% filter(Name == paese) %>% arrange(Year)
    test_row    <- df_continuous_shocks %>% filter(Name == paese, Year == anno)
    
    # Skipping iterations with insufficient longitudinal data depth
    if (nrow(test_row) == 0 || nrow(train_paese) < 10) next
    
    # 1. Estimating the Fixed Effects (FE) model via the Within estimator
    pred_fe <- tryCatch({
      p_train <- pdata.frame(as.data.frame(train_panel), index = c("Name", "Year"))
      fe_mod  <- plm(d_log_GDP ~ hd30 + cdd + r20mm + d_log_emp + log_infl + d_log_rnna + d_hc,
                     data = p_train, model = "within", effect = "individual")
      
      xvars   <- c("hd30", "cdd", "r20mm", "d_log_emp", "log_infl", "d_log_rnna", "d_hc")
      beta    <- coef(fe_mod)
      x_new   <- as.numeric(test_row[, xvars])
      fe_paese <- mean(residuals(fe_mod)[attr(residuals(fe_mod), "index")$Name == paese], na.rm = TRUE)
      sum(beta * x_new, na.rm = TRUE) + fe_paese
    }, error = function(e) NA)
    
    # 2. Estimating the Mixed-Effects (LME) model with random intercepts for country clusters
    pred_lme <- tryCatch({
      lme_mod <- lme(d_log_GDP ~ hd30 + cdd + r20mm + d_log_emp + log_infl + d_log_rnna + d_hc,
                     random   = ~1 | Name,
                     weights  = varExp(),
                     data     = as.data.frame(train_panel),
                     na.action = na.omit)
      as.numeric(predict(lme_mod, newdata = as.data.frame(test_row), level = 1))
    }, error = function(e) NA)
    
    # 3. Extracting Panel VAR (PVAR) predictions from the previously computed recursive estimates
    pred_pvar <- tryCatch({
      country_code <- df_continuous_shocks %>% filter(Name == paese) %>% pull(Code) %>% .[1]
      
      df_forecast_recursive %>%
        filter(Code == country_code, Year == anno) %>%
        pull(d_log_GDP) %>%
        .[1]
    }, error = function(e) NA)
    
    # 4. Estimating the ARMAX benchmark via expanding-window auto.arima selection
    pred_armax <- tryCatch({
      y_tr <- train_paese$d_log_GDP
      x_tr <- as.matrix(train_paese[, c("hd30", "cdd", "r20mm")])
      x_te <- as.matrix(test_row[,  c("hd30", "cdd", "r20mm")])
      
      armax_mod <- auto.arima(y_tr, xreg = x_tr,
                              stationary  = TRUE,
                              seasonal    = FALSE,
                              ic          = "aicc",
                              stepwise    = TRUE,
                              approximation = TRUE)
      as.numeric(forecast(armax_mod, h = 1, xreg = x_te)$mean)
    }, error = function(e) NA)
    
    # Storing the comparative point forecasts
    df_confronto <- bind_rows(df_confronto, data.frame(
      Name     = paese,
      Year     = anno,
      actual   = as.numeric(test_row$d_log_GDP),
      pred_FE  = pred_fe,
      pred_LME = pred_lme,
      pred_PVAR= pred_pvar,
      pred_ARMAX = pred_armax
    ))
  }
  cat(sprintf("Processing complete for: %s\n", paese))
}

# ==============================================================================
# COMPUTING PERFORMANCE METRICS
# ==============================================================================

# Defining a function for evaluating predictive accuracy across competing model specifications
calcola_metriche <- function(actual, pred, nome_modello) {
  idx    <- !is.na(actual) & !is.na(pred)
  a      <- actual[idx]
  p      <- pred[idx]
  mse    <- mean((a - p)^2)
  mae    <- mean(abs(a - p))
  smape  <- mean(2 * abs(a - p) / (abs(a) + abs(p))) * 100
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  r2_oos <- 1 - ss_res / ss_tot
  data.frame(Modello = nome_modello, N = sum(idx),
             MSE = mse, MAE = mae, sMAPE = smape, R2_oos = r2_oos)
}

# Consolidating performance metrics for final comparative reporting
metriche_finali <- bind_rows(
  calcola_metriche(df_confronto$actual, df_confronto$pred_FE,   "Fixed Effects (FE)"),
  calcola_metriche(df_confronto$actual, df_confronto$pred_LME,  "Mixed Effects (LME)"),
  calcola_metriche(df_confronto$actual, df_confronto$pred_PVAR, "Panel VAR (PVAR)"),
  calcola_metriche(df_confronto$actual, df_confronto$pred_ARMAX,"ARMAX Standard")
) %>% arrange(MAE)

cat("\n==============================================================================\n")
cat(" COMPARATIVE MODELS PERFORMANCE — OUT-OF-SAMPLE 2015-2023 (Panel Aggregate)\n")
cat("==============================================================================\n")
print(metriche_finali, digits = 4)
cat("==============================================================================\n")

# ==============================================================================
# DIAGNOSTIC VISUALIZATIONS
# ==============================================================================

# Plotting the Mean Absolute Error (MAE) per annum to evaluate the forecast accuracy 
# and the impact of systemic shocks (e.g., 2020 pandemic)
df_confronto %>%
  filter(!is.na(actual)) %>%
  pivot_longer(cols = starts_with("pred_"), names_to = "Modello", values_to = "pred",
               names_prefix = "pred_") %>%
  group_by(Modello, Year) %>%
  summarise(MAE = mean(abs(actual - pred), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Year, y = MAE, color = Modello)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = anni_test) +
  theme_minimal(base_size = 13) +
  labs(title    = "MAE per annum — Comparative out-of-sample assessment",
       subtitle = "Observed 2020 peak reflecting exogenous pandemic shocks",
       y = "Mean Absolute Error", x = "Year", color = "Model") +
  theme(legend.position = "top")

# Plotting the out-of-sample R² by country to determine model fitness at the cross-sectional level
df_confronto %>%
  filter(!is.na(actual)) %>%
  pivot_longer(cols = starts_with("pred_"), names_to = "Modello", values_to = "pred",
               names_prefix = "pred_") %>%
  group_by(Modello, Name) %>%
  summarise(
    R2 = 1 - sum((actual - pred)^2, na.rm = TRUE) /
      sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(Name, R2, FUN = median), y = R2, fill = Modello)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  theme_minimal(base_size = 11) +
  labs(title = "Out-of-Sample R² by country and model",
       subtitle = "Negative values denote predictive performance inferior to the historical sample mean",
       x = NULL, y = "R²_oos", fill = "Model") +
  theme(legend.position = "top")

# ==============================================================================
# SCENARIO PREPARATION: CETERIS PARIBUS BASELINE (ANTI-CRASH)
# ==============================================================================

cat("Freezing macroeconomic variables at 2023 levels for future scenario projections...\n")

# Defining the vector of macroeconomic control variables to be kept constant in the projection horizon
var_economiche <- c("d_log_emp", "log_infl", "d_log_rnna", "d_hc", "time_trend")

# Defining a function for total data cleaning to prevent observation dropping within the LME models
prepara_scenario_sicuro <- function(df_scen) {
  df_scen %>%
    group_by(Code) %>%
    arrange(Year, .by_group = TRUE) %>%
    # Carrying forward the 2023 values to populate future years (Ceteris Paribus assumption)
    fill(all_of(var_economiche), .direction = "down") %>%
    # Replacing missing values with zero (zero growth) for variables missing 2023 data 
    # to ensure model predict() compatibility
    mutate(across(all_of(var_economiche), ~ replace_na(.x, 0))) %>%
    ungroup() %>%
    # Implementing additional safety measures for climate indicators
    mutate(across(c(hd30, cdd, r20mm), ~ replace_na(.x, 0)))
}

# Preparing the stabilized scenario datasets
df_ssp245_frozen <- prepara_scenario_sicuro(df_ssp245_clean)
df_ssp370_frozen <- prepara_scenario_sicuro(df_ssp370_clean)

# ==============================================================================
# APPROACH 1: INTEGRATED MODEL (Climate + Macroeconomic Controls)
# ==============================================================================

# Defining the forecasting function for the integrated model using Fixed and Mixed effects
forecast_scenario_completo <- function(df_scenario_frozen, fe_model, fe_eff, lme_model, label_scenario) {
  
  beta_fe <- coef(fe_model)
  names_beta <- names(beta_fe)
  
  df_future <- df_scenario_frozen %>% filter(Year >= 2024) %>% arrange(Name, Year)
  
  # Mapping country-specific fixed effects intercepts
  df_fe_effects <- data.frame(Name = names(fe_eff), fe_intercept = as.numeric(fe_eff))
  
  df_future <- df_future %>% 
    left_join(df_fe_effects, by = "Name") %>% 
    mutate(fe_intercept = replace_na(fe_intercept, 0))
  
  # Implementing a robust internal function for coefficient matching via fuzzy grep
  get_coef <- function(var_name, beta_vector, beta_names) {
    idx <- grep(var_name, beta_names, ignore.case = TRUE)[1]
    if (!is.na(idx)) return(as.numeric(beta_vector[idx]))
    return(0) # Defaulting to zero-impact under ceteris paribus if the regressor is absent
  }
  
  # Generating dynamic FE projections utilizing fuzzy coefficient matching
  df_future <- df_future %>%
    mutate(
      d_log_GDP_FE = get_coef("hd30", beta_fe, names_beta)       * hd30       +
        get_coef("cdd", beta_fe, names_beta)        * cdd        +
        get_coef("r20mm", beta_fe, names_beta)      * r20mm      +
        get_coef("emp", beta_fe, names_beta)        * d_log_emp  +
        get_coef("infl", beta_fe, names_beta)       * log_infl   +
        get_coef("rnna", beta_fe, names_beta)       * d_log_rnna +
        get_coef("hc", beta_fe, names_beta)         * d_hc       +
        fe_intercept
    )
  
  # Generating LME projections utilizing the nlme predict method
  df_future$d_log_GDP_LME <- as.numeric(
    predict(lme_model, newdata = as.data.frame(df_future), level = 1)
  )
  
  df_future$Scenario <- label_scenario
  return(df_future)
}

# ==============================================================================
# ESTIMATING MODELS FOR LONG-TERM PROJECTIONS (2024-2100)
# ==============================================================================
cat("Estimating definitive FE and LME models for long-term projections...\n")

panel_proiezioni <- pdata.frame(as.data.frame(df_continuous_shocks), index = c("Name", "Year"))

# 1. Estimating the Integrated Model (Climate + Macroeconomic Controls)
fe_completo <- plm(d_log_GDP ~ hd30 + cdd + r20mm + d_log_emp + log_infl + d_log_rnna + d_hc, 
                   data = panel_proiezioni, model = "within", effect = "individual")

fe_effects_completo <- fixef(fe_completo)

lme_completo <- lme(d_log_GDP ~ hd30 + cdd + r20mm + d_log_emp + log_infl + d_log_rnna + d_hc, 
                    random = ~1 | Name, 
                    weights = varExp(), 
                    data = as.data.frame(df_continuous_shocks),
                    na.action = na.omit)

# 2. Estimating the Climate-Only Model (Isolating pure climatic impact)
fe_clima <- plm(d_log_GDP ~ hd30 + cdd + r20mm, 
                data = panel_proiezioni, model = "within", effect = "individual")

fe_effects <- fixef(fe_clima)

lme_clima <- lme(d_log_GDP ~ hd30 + cdd + r20mm, 
                 random = ~1 | Name, 
                 weights = varExp(), 
                 data = as.data.frame(df_continuous_shocks),
                 na.action = na.omit)

cat("Executing Integrated Model Projections...\n")
df_forecast_245_completo <- forecast_scenario_completo(df_ssp245_frozen, fe_completo, fe_effects_completo, lme_completo, "SSP2-4.5")
df_forecast_370_completo <- forecast_scenario_completo(df_ssp370_frozen, fe_completo, fe_effects_completo, lme_completo, "SSP3-7.0")
df_forecast_completo     <- bind_rows(df_forecast_245_completo, df_forecast_370_completo)

# ==============================================================================
# APPROACH 2: CLIMATE-ONLY MODEL
# ==============================================================================

# Defining the forecasting function for the climate-only model
forecast_scenario_clima <- function(df_scenario_frozen, fe_model, fe_eff, lme_model, label_scenario) {
  beta_fe <- coef(fe_model)
  
  df_future <- df_scenario_frozen %>% filter(Year >= 2024) %>% arrange(Name, Year)
  
  df_fe_effects <- data.frame(Name = names(fe_eff), fe_intercept = as.numeric(fe_eff))
  
  df_future <- df_future %>% 
    left_join(df_fe_effects, by = "Name") %>% 
    mutate(fe_intercept = replace_na(fe_intercept, 0))
  
  df_future <- df_future %>%
    mutate(
      d_log_GDP_FE = beta_fe["hd30"]  * hd30  +
        beta_fe["cdd"]   * cdd   +
        beta_fe["r20mm"] * r20mm +
        fe_intercept
    )
  
  df_future$d_log_GDP_LME <- as.numeric(
    predict(lme_model, newdata = as.data.frame(df_future), level = 1)
  )
  
  df_future$Scenario <- label_scenario
  return(df_future)
}

cat("Executing Climate-Only Model Projections...\n")
df_forecast_245_clima <- forecast_scenario_clima(df_ssp245_frozen, fe_clima, fe_effects, lme_clima, "SSP2-4.5")
df_forecast_370_clima <- forecast_scenario_clima(df_ssp370_frozen, fe_clima, fe_effects, lme_clima, "SSP3-7.0")
df_forecast_clima     <- bind_rows(df_forecast_245_clima, df_forecast_370_clima)

# ==============================================================================
# RECONSTRUCTING GDP LEVELS AND VISUALIZATION
# ==============================================================================

# Computing index-based GDP levels via cumulative summation of log-growth projections
ricostruisci_livelli <- function(df_forecast) {
  df_forecast %>%
    group_by(Name, Code, Scenario) %>%
    arrange(Year, .by_group = TRUE) %>%
    mutate(
      idx_FE  = exp(cumsum(d_log_GDP_FE)),
      idx_LME = exp(cumsum(d_log_GDP_LME))
    ) %>%
    ungroup()
}

df_livelli_clima    <- ricostruisci_livelli(df_forecast_clima)
df_livelli_completo <- ricostruisci_livelli(df_forecast_completo)

# Defining the visualization function for comparative scenario plotting
plot_scenario <- function(df_livelli, titolo, sottotitolo, paesi_plot = c("Italy", "Germany", "Kenya", "Brazil")) {
  df_livelli %>%
    filter(Name %in% paesi_plot) %>%
    pivot_longer(cols = c(idx_FE, idx_LME), names_to = "Modello", values_to = "GDP_idx") %>%
    mutate(Modello = case_when(Modello == "idx_FE" ~ "Fixed Effects (FE)", Modello == "idx_LME" ~ "Mixed Effects (LME)")) %>%
    ggplot(aes(x = Year, y = GDP_idx, color = Scenario, linetype = Modello)) +
    geom_line(linewidth = 1.1) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +
    facet_wrap(~ Name, scales = "free_y") +
    scale_color_manual(values = c("SSP2-4.5" = "steelblue", "SSP3-7.0" = "firebrick")) +
    theme_minimal(base_size = 12) +
    labs(title = titolo, subtitle = sottotitolo, y = "GDP (Base Index 2024 = 1)", x = "Year", color = "IPCC Scenario", linetype = "Methodology") +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

cat("Generating Final Visualizations...\n")
plot_solo_clima <- plot_scenario(df_livelli_clima, "GDP Projection 2024-2100: Pure Climatic Impact", "Climate regressors only | FE vs LME Models | IPCC Scenarios")
plot_modello_completo <- plot_scenario(df_livelli_completo, "GDP Projection 2024-2100: Integrated Model", "Climate + Macroeconomic Controls | FE vs LME Models")

x11()
print(plot_solo_clima)
x11()
print(plot_modello_completo)
cat("MISSION ACCOMPLISHED! All projections and visualizations successfully generated.\n")

# ==============================================================================
# LONG-TERM MACROECONOMIC FORECASTING VIA DEEP LEARNING (LSTM) (2024-2050)
# ==============================================================================

library(keras3)
library(dplyr)
library(tidyr)
library(ggplot2)

cat("\n==============================================================================\n")
cat(" TRAINING LSTM NEURAL NETWORKS FOR MACROECONOMIC VARIABLES THROUGH 2050 \n")
cat("==============================================================================\n")

# Defining the target projection horizon
anni_futuri <- 2024:2050
passi_futuri <- length(anni_futuri)

# Selecting the four macroeconomic covariates utilized in the Integrated Model
var_da_predire <- c("d_log_emp", "log_infl", "d_log_rnna", "d_hc")
paesi_unici <- unique(df_continuous_shocks$Code)

# Initializing a container for the generated macroeconomic projections
df_futuro_eco <- data.frame()

# Iterating cross-sectionally to train country-specific LSTM models
for (codice_paese in paesi_unici) {
  cat(sprintf("Training LSTM for: %s...", codice_paese))
  
  df_storico_paese <- df_continuous_shocks %>% 
    filter(Code == codice_paese) %>% 
    arrange(Year)
  
  matrice_storica <- df_storico_paese %>% 
    dplyr::select(all_of(var_da_predire)) %>% 
    as.matrix()
  
  # Imputing missing values with zeros to maintain temporal continuity in training matrices
  matrice_storica[is.na(matrice_storica)] <- 0
  
  # Normalizing data to the [0, 1] range to optimize neural network convergence
  min_val <- apply(matrice_storica, 2, min)
  max_val <- apply(matrice_storica, 2, max)
  max_val[max_val == min_val] <- max_val[max_val == min_val] + 1 
  matrice_scalata <- scale(matrice_storica, center = min_val, scale = max_val - min_val)
  
  # Constructing 3D tensors [samples, timesteps, features] with a look-back window of 3 years
  look_back <- 3
  X_train <- list()
  Y_train <- list()
  
  for (i in 1:(nrow(matrice_scalata) - look_back)) {
    X_train[[i]] <- matrice_scalata[i:(i + look_back - 1), ]
    Y_train[[i]] <- matrice_scalata[i + look_back, ]
  }
  
  X_array <- array(unlist(X_train), dim = c(look_back, length(var_da_predire), length(X_train)))
  X_array <- aperm(X_array, c(3, 1, 2)) 
  Y_array <- do.call(rbind, Y_train)
  
  # Configuring the LSTM neural network architecture
  modello_lstm <- keras_model_sequential() %>%
    layer_lstm(units = 20, input_shape = c(look_back, length(var_da_predire)), return_sequences = FALSE) %>%
    layer_dense(units = length(var_da_predire))
  
  modello_lstm %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = 0.01)
  )
  
  # Training the model in silent mode
  modello_lstm %>% fit(X_array, Y_array, epochs = 100, batch_size = 4, verbose = 0)
  
  # Executing iterative recursive forecasting through 2050
  input_dinamico <- matrice_scalata[(nrow(matrice_scalata) - look_back + 1):nrow(matrice_scalata), ]
  matrice_predizioni_scalate <- matrix(NA, nrow = passi_futuri, ncol = length(var_da_predire))
  
  for (t in 1:passi_futuri) {
    input_3d <- array(input_dinamico, dim = c(1, look_back, length(var_da_predire)))
    pred_passo <- as.numeric(predict(modello_lstm, input_3d, verbose = 0))
    matrice_predizioni_scalate[t, ] <- pred_passo
    input_dinamico <- rbind(input_dinamico[-1, ], pred_passo)
  }
  
  # Denormalizing the predictions to return them to their original scale
  matrice_predizioni <- t(t(matrice_predizioni_scalate) * (max_val - min_val) + min_val)
  colnames(matrice_predizioni) <- var_da_predire
  
  nome_paese <- df_storico_paese$Name[1]
  
  df_futuro_paese <- data.frame(
    Code = codice_paese,
    Name = nome_paese,
    Year = anni_futuri,
    matrice_predizioni
  )
  
  df_futuro_eco <- bind_rows(df_futuro_eco, df_futuro_paese)
  cat(" Completed!\n")
}

# ==============================================================================
# INTEGRATING LSTM FORECASTS WITH CLIMATE SCENARIOS
# ==============================================================================

cat("\nAssembling hybrid LSTM + IPCC scenario datasets...\n")

# Extracting climatic variables and deterministic time trends through 2050
clima_245_50 <- df_ssp245_clean %>% filter(Year >= 2024 & Year <= 2050) %>% dplyr::select(Code, Year, hd30, cdd, r20mm, time_trend)
clima_370_50 <- df_ssp370_clean %>% filter(Year >= 2024 & Year <= 2050) %>% dplyr::select(Code, Year, hd30, cdd, r20mm, time_trend)

# Merging LSTM economic projections with scenario-specific climatic inputs
df_ssp245_lstm <- df_futuro_eco %>% left_join(clima_245_50, by = c("Code", "Year"))
df_ssp370_lstm <- df_futuro_eco %>% left_join(clima_370_50, by = c("Code", "Year"))


# ==============================================================================
# FINAL FORECASTING: INTEGRATED MODEL (LSTM DYNAMICS)
# ==============================================================================
cat("Executing Integrated Model Projections (LSTM-driven Macro Dynamics)...\n")

# Generating projections utilizing the previously estimated Integrated Model (fe_completo, lme_completo)
df_forecast_245_lstm <- forecast_scenario_completo(df_ssp245_lstm, fe_completo, fe_effects_completo, lme_completo, "SSP2-4.5")
df_forecast_370_lstm <- forecast_scenario_completo(df_ssp370_lstm, fe_completo, fe_effects_completo, lme_completo, "SSP3-7.0")
df_forecast_ibrido   <- bind_rows(df_forecast_245_lstm, df_forecast_370_lstm)


# ==============================================================================
# RECONSTRUCTING GDP LEVELS AND VISUALIZATION
# ==============================================================================

cat("Reconstructing GDP levels (Base index 2024 = 1)...\n")
df_livelli_ibrido <- ricostruisci_livelli(df_forecast_ibrido)

cat("Generating final hybrid projection plot...\n")
plot_modello_ibrido <- plot_scenario(
  df_livelli_ibrido, 
  titolo = "GDP Projection 2024-2050: Advanced Integrated Model", 
  sottotitolo = "IPCC Climate Shocks + LSTM Macroeconomic Projections | FE vs LME Models"
)

# Rendering the visualization
x11()
print(plot_modello_ibrido)

cat("LSTM procedural pipeline and plot generation successfully concluded.\n")
