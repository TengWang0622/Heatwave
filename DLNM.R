##############################################################################
#        
#     Circulatory, respiratory, and mental health hospitalizations associated with
#               dry and humid heatwaves in China in a warming climate
#     
#                  Distributed Lag Non-linear Model (DLNM) Analysis
##############################################################################

# Contact: Teng Wang @ HKU (wang.teng19@alumni.imperial.ac.uk)
#          Hanxu Shi @ PKU (shx@bjmu.edu.cn)

# Version: 260301

# Description: DLNM analysis

############ Preparation ######################

# Load required packages
library(MASS)

library(dplyr)
library(lubridate)
library(dlnm)
library(splines)
library(ggplot2)
library(tidyr)
library(patchwork)
library(openxlsx)
library(mixmeta)

library(pbapply)
library(data.table)

select <- dplyr::select

# Set working directory
setwd("C:/Project/Humid")

##############################################################################
#                    Data Loading and Validation
##############################################################################

# Load heatwave events
all_heatwaves_ERA5 <- readRDS("Src/ERA5 Processed/Heatwave events/all_heatwaves_ERA5_2016_2024.rds")

# Load hospitalization data
df_Hospitalization <- readRDS("Src/CMC Hosp/Circ_Resp_Mental_Agg.rds")

# Standardize data types ---------------------------------------------

# Hosp data
df_Hospitalization$CountyCode=as.character(df_Hospitalization$CountyCode)
df_Hospitalization$date=as.Date(df_Hospitalization$date)

# Event data
all_heatwaves_ERA5$CountyCode=as.character(all_heatwaves_ERA5$CountyCode)
all_heatwaves_ERA5$start_date=as.Date(all_heatwaves_ERA5$start_date)
all_heatwaves_ERA5$end_date=as.Date(all_heatwaves_ERA5$end_date)


# Define disease categories
disease_types <- c("Circulatory", "Respiratory", "Mental", "Others", "All_cause")

##############################################################################
#                     Create Daily Heatwave Exposure
##############################################################################

# Expand heatwave events to daily records
heatwave_daily <- all_heatwaves_ERA5 %>%
  filter(!is.na(start_date) & !is.na(end_date)) %>%
  rowwise() %>%
  do({
    tibble(
      CountyCode = .$CountyCode,
      date = seq(.$start_date, .$end_date, by = "day"),
      heatwave_type = .$heatwave_type,
      hw_duration = .$duration,
      hw_mean_temp = .$mean_temp,
      hw_max_temp = .$max_temp,
      hw_mean_rh = .$mean_rh
    )
  }) %>%
  ungroup() %>%
  group_by(CountyCode, date) %>%
  # If multiple heatwaves on same day, keep the one with higher temperature
  arrange(desc(hw_mean_temp)) %>%
  slice(1) %>%
  ungroup()

# ************* SAVE **********************
#saveRDS(heatwave_daily, "Src/Health Processed/heatwave_daily.rds")

##############################################################################
#                    STEP 3: Aggregate Daily Hospitalization Data
##############################################################################

# Total daily hospitalizations by county
daily_hosp_total <- df_Hospitalization %>%
  group_by(CountyCode, County, date) %>%
  summarise(
    across(all_of(disease_types), ~sum(.x, na.rm = TRUE)),
    .groups = 'drop')

# By Gender
daily_hosp_gender <- df_Hospitalization %>%
  group_by(CountyCode, County, date, Gender) %>%
  summarise(
    across(all_of(disease_types), ~sum(.x, na.rm = TRUE)),
    .groups = 'drop')

# By Age
daily_hosp_age <- df_Hospitalization %>%
  group_by(CountyCode, County, date, Age_g) %>%
  summarise(
    across(all_of(disease_types), ~sum(.x, na.rm = TRUE)),
    .groups = 'drop')

# By Admission Source
daily_hosp_adminsrc <- df_Hospitalization %>%
  group_by(CountyCode, County, date, AdminSrc) %>%
  summarise(
    across(all_of(disease_types), ~sum(.x, na.rm = TRUE)),
    .groups = 'drop')


# ************* SAVE ******************************************************************
saveRDS(daily_hosp_total, "Src/Health Processed/daily_hosp_total.rds")
saveRDS(daily_hosp_gender, "Src/Health Processed/daily_hosp_gender.rds")
saveRDS(daily_hosp_age, "Src/Health Processed/daily_hosp_age.rds")
saveRDS(daily_hosp_adminsrc, "Src/Health Processed/daily_hosp_adminsrc.rds")


############## RESTART from the processed health data ########################

# Heatwave daily
heatwave_daily=readRDS("Src/Health Processed/heatwave_daily.rds")

# Daily total
daily_hosp_total=readRDS("Src/Health Processed/daily_hosp_total.rds")

# Gender 
daily_hosp_gender=readRDS("Src/Health Processed/daily_hosp_gender.rds")
daily_hosp_Male=daily_hosp_gender %>% filter(Gender=="Male")
daily_hosp_Female=daily_hosp_gender %>% filter(Gender=="Female")

# Age
daily_hosp_age=readRDS("Src/Health Processed/daily_hosp_age.rds")
daily_hosp_age19=daily_hosp_age %>% filter(Age_g=="19")
daily_hosp_age20_44=daily_hosp_age %>% filter(Age_g=="20_44")
daily_hosp_age45_64=daily_hosp_age %>% filter(Age_g=="45_64")
daily_hosp_age65=daily_hosp_age %>% filter(Age_g=="65")

# AdminSrc
daily_hosp_adminsrc=readRDS("Src/Health Processed/daily_hosp_adminsrc.rds")
daily_hosp_ED=daily_hosp_adminsrc %>% filter(AdminSrc=="ED")
daily_hosp_Outpatient=daily_hosp_adminsrc %>% filter(AdminSrc=="Outpatient")
daily_hosp_Transfer=daily_hosp_adminsrc %>% filter(AdminSrc=="Transfer")


##############################################################################
#                    Create Complete Analysis Dataset
##############################################################################

# Specify the dataset you need
daily_hosp_total=daily_hosp_total

# Define complete date range --------------------------------
date_min <- min(daily_hosp_total$date)
date_max <- max(daily_hosp_total$date)
date_range <- seq(date_min, date_max, by = "day")

# Get unique counties --------------------------------
counties <- unique(daily_hosp_total$CountyCode)

# Create complete daily framework -------------------------
complete_daily_data <- expand.grid(
  CountyCode = counties,
  date = date_range,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  # Join hospitalization data
  left_join(
    daily_hosp_total,
    by = c("CountyCode", "date")
  ) %>%
  # Join heatwave exposure
  left_join(
    heatwave_daily %>%
      dplyr::select(CountyCode, date, heatwave_type, hw_duration, 
                    hw_mean_temp, hw_mean_rh),
    by = c("CountyCode", "date")
  ) %>%
  arrange(CountyCode, date)

# Fill missing hospitalization counts with 0 --------------
complete_daily_data <- complete_daily_data %>%
  mutate(across(all_of(disease_types), ~replace_na(.x, 0)))

# Create heatwave exposure indicators ----------------------
complete_daily_data <- complete_daily_data %>%
  mutate(
    # Any heatwave (binary)
    heatwave = ifelse(!is.na(heatwave_type), 1, 0),
    
    # Dry heatwave (binary)
    dry_heatwave = ifelse(heatwave_type == "Dry", 1, 0),
    
    # Humid heatwave (binary)
    humid_heatwave = ifelse(heatwave_type == "Humid", 1, 0),
    
    # Replace NA with 0
    dry_heatwave = replace_na(dry_heatwave, 0),
    humid_heatwave = replace_na(humid_heatwave, 0)
  )

# Add temporal variables for model adjustment -------------------------
complete_daily_data <- complete_daily_data %>%
  mutate(
    year = year(date),
    month = month(date),
    day_of_week = wday(date),
    day_of_year = yday(date),
    # Time trend (days since start)
    time_seq = as.numeric(date - date_min) + 1,
    # Study year (for degrees of freedom calculation)
    n_years = as.numeric(difftime(date_max, date_min, units = "days")) / 365.25
  )


# Check for counties without heatwaves ---------------------------
counties_no_hw <- complete_daily_data %>%
  group_by(CountyCode) %>%
  summarise(total_hw_days = sum(heatwave)) %>%
  filter(total_hw_days == 0)

if (nrow(counties_no_hw) > 0) {
  cat("\nWarning:", nrow(counties_no_hw), "counties have no heatwave exposure\n")
  cat("These counties will be excluded from analysis\n")
  
  # Exclude counties without heatwaves
  complete_daily_data <- complete_daily_data %>%
    filter(!CountyCode %in% counties_no_hw$CountyCode)
  
  counties <- setdiff(counties, counties_no_hw$CountyCode)
}

############################################################################  
#                        Custom Functions 
############################################################################

# ================== Defining DLNM Model Functions ==========================

# Set maximum lag
MAX_LAG <- 28  # 0-21 days lag
LAG_DF <- 4    # Degrees of freedom for lag dimension

# Function to fit county-specific DLNM model
fit_county_dlnm <- function(data, county_code, disease, exposure_type = "all") {
  
  # Filter data for specific county
  county_data <- data %>%
    filter(CountyCode == county_code) %>%
    arrange(date)
  
  # Define exposure variable based on type
  if (exposure_type == "dry") {
    exposure_var <- county_data$dry_heatwave
  } else if (exposure_type == "humid") {
    exposure_var <- county_data$humid_heatwave
  } else {
    exposure_var <- county_data$heatwave
  }
  
  # Create lag matrix
  n_obs <- length(exposure_var)
  lag_matrix <- matrix(0, nrow = n_obs, ncol = MAX_LAG + 1)
  
  for (i in 1:n_obs) {
    for (lag in 0:MAX_LAG) {
      if (i > lag) {
        lag_matrix[i, lag + 1] <- exposure_var[i - lag]
      }
    }
  }
  
  # Create cross-basis function
  # Exposure-response: linear (for binary exposure)
  # Lag-response: natural cubic spline
  cb <- crossbasis(
    lag_matrix,
    lag = MAX_LAG,
    argvar = list(fun = "lin"),
    arglag = list(fun = "ns", df = LAG_DF)
  )
  
  # Extract outcome variable
  outcome <- county_data[[disease]]
  
  # Calculate degrees of freedom for long-term trend
  # Approximately 7 df per year
  n_years <- unique(county_data$n_years)[1]
  trend_df <- ceiling(7 * n_years)
  
  # Fit quasi-Poisson GLM with DLNM
  formula_str <- paste0(
    "outcome ~ cb + ",
    "ns(time_seq, df = ", trend_df, ") + ",  # Long-term trend
    "factor(day_of_week) + ",                 # Day of week
    "ns(day_of_year, df = 4)"                 # Seasonality
  )
  
  tryCatch({
    model <- glm(
      as.formula(formula_str),
      family = quasipoisson(),
      data = county_data
    )
    
    # Extract cross-basis predictions
    pred <- crosspred(
      cb,
      model,
      at = 1,      # Heatwave vs no heatwave
      cen = 0,     # Reference: no heatwave
      cumul = TRUE
    )
    
    return(list(
      model = model,
      crossbasis = cb,
      prediction = pred,
      county_code = county_code,
      disease = disease,
      exposure_type = exposure_type,
      n_obs = n_obs,
      n_events = sum(outcome),
      success = TRUE
    ))
    
  }, error = function(e) {
    cat("Error fitting model for county", county_code, ":", e$message, "\n")
    return(list(success = FALSE, county_code = county_code, error = e$message))
  })
}


# ============== Pool function ===================================================================================

# Function to pool county-specific estimates using meta-analysis
pool_county_estimates <- function(county_results) {
  
  # Filter successful models
  successful_models <- county_results[sapply(county_results, function(x) x$success)]
  
  if (length(successful_models) == 0) {
    cat("No successful models to pool\n")
    return(NULL)
  }
  
  cat("Pooling", length(successful_models), "county-specific estimates\n")
  
  # Extract coefficients and covariance matrices
  coef_list <- lapply(successful_models, function(x) {
    coef(x$prediction)
  })
  
  vcov_list <- lapply(successful_models, function(x) {
    vcov(x$prediction)
  })
  
  coef_matrix <- do.call(rbind, coef_list)
  
  pooled <- mixmeta(coef_matrix, vcov_list, method = "reml")  #coef_list
  
  pooled_pred <- crosspred(
    basis = successful_models[[1]]$crossbasis,
    coef = coef(pooled),
    vcov = vcov(pooled),
    model.link = "log",
    at = 1,
    cen = 0,
    cumul = TRUE
  )
  
  return(list(
    pooled_model = pooled,
    pooled_prediction = pooled_pred,
    n_counties = length(successful_models),
    counties = sapply(successful_models, function(x) x$county_code)
  ))
}

# ================ Function: extract cummulative RR ==============================================================
#  Function to extract cumulative RR with CI
extract_cumulative_rr=function(pred, disease_name, exposure_type) {
  
  if (is.null(pred)) {
    return(tibble(
      disease = disease_name,
      exposure_type = exposure_type,
      RR = NA,
      RR_lower = NA,
      RR_upper = NA,
      p_value = NA
    ))
  }
  
  # Cumulative effect over entire lag period
  cum_rr <- pred$allRRfit[1]
  cum_rr_low <- pred$allRRlow[1]
  cum_rr_high <- pred$allRRhigh[1]
  
  # Calculate p-value
  log_rr <- log(cum_rr)
  log_se <- (log(cum_rr_high) - log(cum_rr_low)) / (2 * 1.96)
  z_value <- log_rr / log_se
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  
  return(tibble(
    disease = disease_name,
    exposure_type = exposure_type,
    RR = cum_rr,
    RR_lower = cum_rr_low,
    RR_upper = cum_rr_high,
    p_value = p_value
  ))
}

# ================ Function: extract lag RR ==============================================================

# Function to extract lag-specific RR
extract_lag_specific_rr=function(pred, disease_name, exposure_type) {
  
  if (is.null(pred)) {
    return(tibble(
      disease = disease_name,
      exposure_type = exposure_type,
      lag = 0:MAX_LAG,
      RR = NA,
      RR_lower = NA,
      RR_upper = NA
    ))
  }
  
  tibble(
    disease = disease_name,
    exposure_type = exposure_type,
    lag = 0:MAX_LAG,
    RR = pred$matRRfit[1, ],
    RR_lower = pred$matRRlow[1, ],
    RR_upper = pred$matRRhigh[1, ]
  )
}

# ================ Function: Plot lag RR ===========================

plot_lag_response <- function(data, 
                              disease_name,
                              exposure_type = "all",  # 指定要绘制的热浪类型
                              line_color = "#E74C3C",  # 线条颜色
                              fill_color = "#E74C3C",  # 填充颜色
                              y_limits = NULL,         # Y轴范围，如 c(0.8, 1.3)
                              y_breaks = NULL,         # Y轴刻度，如 seq(0.8, 1.3, 0.1)
                              show_points = TRUE,      # 是否显示数据点
                              panel_border = TRUE,     # 是否显示外框
                              panel_border_size = 0.8, # 外框粗细
                              panel_border_color = "black") {  # 外框颜色
  
  plot_data <- data %>%
    filter(disease == disease_name, 
           exposure_type == !!exposure_type,
           !is.na(RR))
  
  if (nrow(plot_data) == 0) {
    stop(paste("No data available for disease:", disease_name, 
               "and exposure type:", exposure_type))
  }
  
  p <- ggplot(plot_data, aes(x = lag, y = RR))
  
  p <- p + geom_hline(yintercept = 1, 
                      linetype = "dashed", 
                      color = "gray40", 
                      linewidth = 0.7)
  
  p <- p + geom_ribbon(aes(ymin = RR_lower, ymax = RR_upper), 
                       alpha = 0.25, 
                       fill = fill_color,
                       color = NA)
  
  p <- p + geom_line(color = line_color, 
                     linewidth = 1.2)
  
  if (show_points) {
    p <- p + geom_point(size = 2, 
                        shape = 21, 
                        fill = "white", 
                        color = line_color,
                        stroke = 1)
  }
  
  p <- p + scale_x_continuous(breaks = seq(0, MAX_LAG, by = 3),
                              expand = c(0.02, 0.02))
  
  if (!is.null(y_limits)) {
    if (!is.null(y_breaks)) {
      p <- p + scale_y_continuous(limits = y_limits,
                                  breaks = y_breaks,
                                  labels = function(x) sprintf("%.2f", x))
    } else {
      p <- p + scale_y_continuous(limits = y_limits,
                                  labels = function(x) sprintf("%.2f", x))
    }
  } else {
    p <- p + scale_y_continuous(labels = function(x) sprintf("%.2f", x))
  }
  
  exposure_label <- switch(exposure_type,
                           "all" = "All Heatwaves",
                           "dry" = "Dry Heatwaves",
                           "humid" = "Humid Heatwaves",
                           exposure_type)
  
  p <- p + labs(
    #  title = paste(disease_name, "-", exposure_label),
    #  subtitle = sprintf("Distributed Lag Non-linear Model (Lag 0-%d days)", MAX_LAG),
    x = "Lag (days)",
    y = "Relative Risk (RR)"
  )
  
  p <- p + theme_minimal(base_size = 12)
  
  p <- p + theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    legend.position = "none"
  )
  
  if (panel_border) {
    p <- p + theme(
      panel.border = element_rect(color = panel_border_color, 
                                  linewidth = panel_border_size, 
                                  fill = NA)
    )
  }
  
  return(p)
}

###############################################################################
#                      Statistical Analysis
###############################################################################

# ==================== Specify model inputs ==========================================

county_code_ALL=unique(complete_daily_data$CountyCode)

county_code=county_code_ALL[1:length(county_code_ALL)]  # length(county_code_ALL)
disease="Circulatory"    #"Circulatory", "Respiratory", "Mental", "Others", "All_cause"
exp_type="humid"     # all, dry, humid

# =============== Implement county-specific DLNM model ==========================================
county_models=pblapply(county_code, function(county) {
  fit_county_dlnm(complete_daily_data, county, disease, exp_type)
})

################################ Filter ##################################################
# Filter successful models
successful <- county_models[sapply(county_models, function(x) x$success)]

# Remove extreme outliers (RR > 10)
successful <- successful[sapply(successful, function(x) {
  rr <- x$prediction$allRRfit[1]
  !is.na(rr) && is.finite(rr) && rr < 10 && rr>0.01
})]


# Extract county-level estimates and check invalid values ----------------------------------------------

# Extract RR and CI on ORIGINAL scale
county_rrs <- sapply(successful, function(x) x$prediction$allRRfit[1])
county_rr_low <- sapply(successful, function(x) x$prediction$allRRlow[1])
county_rr_high <- sapply(successful, function(x) x$prediction$allRRhigh[1])

# Check for invalid values
invalid_idx <- !is.finite(county_rrs) | 
  !is.finite(county_rr_low) | 
  !is.finite(county_rr_high) |
  county_rr_low <= 0 | 
  county_rr_high <= 0

if (any(invalid_idx)) {
  cat(sprintf("⚠ Warning: Removing %d counties with invalid CI\n", sum(invalid_idx)))
  county_rrs <- county_rrs[!invalid_idx]
  county_rr_low <- county_rr_low[!invalid_idx]
  county_rr_high <- county_rr_high[!invalid_idx]
  successful <- successful[!invalid_idx]
}

################################ Meta analysis ##################################################

pooled=pool_county_estimates(successful)

##############################################################################
#                            Result summary
##############################################################################

# Extract cumulative RR value (#pooled$pooled_prediction$matRRfit) -----------------------
lag_cum_rr=extract_cumulative_rr(pooled$pooled_prediction,disease,exp_type)

lag_cum_rr <- lag_cum_rr %>%
  mutate(RR_print = paste0(
    round(RR, 3), " (", 
    round(RR_lower, 3), ", ", 
    round(RR_upper, 3), ")"
  ))

# Extract lag RR value ----------------------------------------------
lag_specific_rr=extract_lag_specific_rr(pooled$pooled_prediction,disease,exp_type)

lag_specific_rr <- lag_specific_rr %>%
  mutate(RR_print = paste0(
    round(RR, 3), " (", 
    round(RR_lower, 3), ", ", 
    round(RR_upper, 3), ")"
  ))

# Plot lag response -------------------------------

Plot_lag_RR= plot_lag_response(lag_specific_rr, 
                               disease_name= disease,
                               exposure_type = exp_type,
                               line_color = "red", 
                               fill_color = "red", 
                               y_limits = c(0.96,1.04),  
                               y_breaks = NULL,         
                               show_points = TRUE,      
                               panel_border = TRUE,     
                               panel_border_size = 0.8, 
                               panel_border_color = "black")  

Plot_lag_RR

##############################################
#             Model Diagnostics
##############################################

# ========== Extract county-level estimates ==========

# Extract RR and CI on ORIGINAL scale
county_rrs <- sapply(successful, function(x) x$prediction$allRRfit[1])
county_rr_low <- sapply(successful, function(x) x$prediction$allRRlow[1])
county_rr_high <- sapply(successful, function(x) x$prediction$allRRhigh[1])

# Check for invalid values
invalid_idx <- !is.finite(county_rrs) | 
  !is.finite(county_rr_low) | 
  !is.finite(county_rr_high) |
  county_rr_low <= 0 | 
  county_rr_high <= 0

if (any(invalid_idx)) {
  cat(sprintf("⚠ Warning: Removing %d counties with invalid CI\n", sum(invalid_idx)))
  county_rrs <- county_rrs[!invalid_idx]
  county_rr_low <- county_rr_low[!invalid_idx]
  county_rr_high <- county_rr_high[!invalid_idx]
  successful <- successful[!invalid_idx]
}

# ========== Transform to LOG scale (CRITICAL!) ==========

county_log_rrs <- log(county_rrs)
county_log_low <- log(county_rr_low)
county_log_high <- log(county_rr_high)

# Calculate SE on log scale
county_log_ses <- (county_log_high - county_log_low) / (2 * 1.96)
county_log_vars <- county_log_ses^2

# ========== Check for Inf/NaN values ==========

if (any(!is.finite(county_log_vars))) {
  cat("⚠ WARNING: Some variances are Inf or NaN!\n")
  cat("Counties with problems:\n")
  problem_idx <- which(!is.finite(county_log_vars))
  for (i in problem_idx) {
    cat(sprintf("  County %d: RR=%.3f, CI=[%.3f, %.3f], SE=%.3f, Var=%s\n",
                i, county_rrs[i], county_rr_low[i], county_rr_high[i],
                county_log_ses[i], county_log_vars[i]))
  }
  
  # Remove problematic counties
  valid_idx <- is.finite(county_log_vars) & county_log_vars > 0
  county_log_rrs <- county_log_rrs[valid_idx]
  county_log_vars <- county_log_vars[valid_idx]
  county_rrs <- county_rrs[valid_idx]
  successful <- successful[valid_idx]
  
  cat(sprintf("\nRetaining %d valid counties\n\n", length(county_log_rrs)))
}

# ========== Cochran's Q test (CORRECTED) ==========

n_counties <- length(county_log_rrs)

# Inverse-variance weights
weights <- 1 / county_log_vars

# Weighted mean (on log scale)
weighted_mean_log_rr <- sum(weights * county_log_rrs) / sum(weights)

# Q statistic (CORRECT formula)
Q <- sum(weights * (county_log_rrs - weighted_mean_log_rr)^2)

df <- n_counties - 1
p_heterogeneity <- pchisq(Q, df, lower.tail = FALSE)

# ========== I-squared ==========
I2 <- max(0, (Q - df) / Q) * 100

# ========== Additional heterogeneity measures ==========

# Tau² (between-study variance)
C <- sum(weights) - sum(weights^2) / sum(weights)
tau2 <- max(0, (Q - df) / C)

# H² statistic
H2 <- Q / df

# ========== Prediction interval ==========
# Back-transform to RR scale
pooled_log_rr <- weighted_mean_log_rr
pooled_rr <- exp(pooled_log_rr)

# Prediction interval (accounting for heterogeneity)
t_crit <- qt(0.975, df)
pred_se <- sqrt(tau2 + 1/sum(weights))
pred_log_lower <- pooled_log_rr - t_crit * pred_se
pred_log_upper <- pooled_log_rr + t_crit * pred_se

pred_lower <- exp(pred_log_lower)
pred_upper <- exp(pred_log_upper)


# ============ Print out the p-value and I2 ============

print(lag_specific_rr)
print(lag_cum_rr)

MAX_LAG
LAG_DF

print(p_heterogeneity)
print(I2)

##############################################################################
#                                PAF
##############################################################################

calc_backward_an_fast <- function(beta_lag, lag_mat, outcome) {
  # Weighted sum of lagged exposures for each day: n × 1
  sum_effects <- as.numeric(lag_mat %*% beta_lag)
  
  # Attributable fraction for each day
  af <- 1 - exp(-sum_effects)
  
  # Attributable number (summed)
  an <- sum(outcome * af)
  return(an)
}

# ===========================================================================
#         Pre-compute lag matrices and outcome for all counties
# ===========================================================================

cat("Pre-computing lagged exposure matrices for all counties...\n")

county_precomp <- lapply(successful, function(model_result) {
  
  county_data <- complete_daily_data %>%
    filter(CountyCode == model_result$county_code) %>%
    arrange(date)
  
  # Exposure vector
  exposure <- switch(exp_type,
                     "dry"   = county_data$dry_heatwave,
                     "humid" = county_data$humid_heatwave,
                     county_data$heatwave)
  
  # Outcome vector
  outcome <- county_data[[disease]]
  n <- length(outcome)
  
  # Build lag matrix (n × (MAX_LAG + 1))
  lag_mat <- matrix(0, nrow = n, ncol = MAX_LAG + 1)
  for (l in 0:MAX_LAG) {
    if (l == 0) {
      lag_mat[, l + 1] <- exposure
    } else {
      lag_mat[(l + 1):n, l + 1] <- exposure[1:(n - l)]
    }
  }
  
  list(
    county_code = model_result$county_code,
    lag_mat     = lag_mat,
    outcome     = outcome,
    n_cases     = sum(outcome),
    n_hw_days   = sum(exposure)
  )
})

total_cases_all <- sum(sapply(county_precomp, function(d) d$n_cases))

# ===========================================================================
#               Extract basis transformation matrix (Blag)
# ===========================================================================

cb_template <- successful[[1]]$crossbasis
n_coef <- length(coef(pooled$pooled_model))

Blag <- matrix(0, nrow = MAX_LAG + 1, ncol = n_coef)

for (j in 1:n_coef) {
  e_j <- rep(0, n_coef)
  e_j[j] <- 1
  
  pred_j <- crosspred(
    basis      = cb_template,
    coef       = e_j,
    vcov       = diag(n_coef) * 1e-10,  # dummy vcov (not used for matfit)
    model.link = "log",
    at         = 1,
    cen        = 0
  )
  Blag[, j] <- pred_j$matfit[1, ]
}

# Verify Blag is correct
pooled_coef <- coef(pooled$pooled_model)
pooled_vcov <- vcov(pooled$pooled_model)

pred_verify <- crosspred(
  basis      = cb_template,
  coef       = pooled_coef,
  vcov       = pooled_vcov,
  model.link = "log",
  at         = 1,
  cen        = 0
)

cat(sprintf("Blag verification error: %.2e (should be ~0)\n",
            max(abs(pred_verify$matfit[1, ] - as.numeric(Blag %*% pooled_coef)))))


# ===========================================================================
#           Overall PAF using POOLED coefficients (point estimate)
# ===========================================================================

beta_pooled <- as.numeric(Blag %*% pooled_coef)

an_pooled_total <- 0
for (i in seq_along(county_precomp)) {
  d <- county_precomp[[i]]
  an_pooled_total <- an_pooled_total + calc_backward_an_fast(beta_pooled, d$lag_mat, d$outcome)
}

paf_pooled <- an_pooled_total / total_cases_all

cat(sprintf("\n=== PAF using Pooled Coefficients (Point Estimate) ===\n"))
cat(sprintf("AN  = %.1f\n", an_pooled_total))
cat(sprintf("PAF = %.5f (%.3f%%)\n", paf_pooled, paf_pooled * 100))


# ===========================================================================
#                         Monte Carlo CI for PAF
# ===========================================================================

nsim <- 1000
set.seed(42)

coef_sim <- mvrnorm(nsim, pooled_coef, pooled_vcov)

paf_sim <- numeric(nsim)
an_sim  <- numeric(nsim)

pb <- txtProgressBar(min = 0, max = nsim, style = 3)

for (s in 1:nsim) {
  # Reconstruct lag-specific log-RR from simulated coefficients
  beta_s <- as.numeric(Blag %*% coef_sim[s, ])
  
  # Calculate total AN across all counties
  an_s <- 0
  for (i in seq_along(county_precomp)) {
    d <- county_precomp[[i]]
    an_s <- an_s + calc_backward_an_fast(beta_s, d$lag_mat, d$outcome)
  }
  
  an_sim[s]  <- an_s
  paf_sim[s] <- an_s / total_cases_all
  
  setTxtProgressBar(pb, s)
}
close(pb)

# Confidence intervals (2.5th and 97.5th percentiles)
paf_ci <- quantile(paf_sim, c(0.025, 0.975))
an_ci  <- quantile(an_sim,  c(0.025, 0.975))

################## Summarize #########################

cat(sprintf("  PAF Results (%s - %s)\n", disease, exp_type))
cat(sprintf("PAF:  %.5f (95%% CI: %.5f, %.5f)\n", paf_pooled, paf_ci[1], paf_ci[2]))
cat(sprintf("PAF%%: %.3f%% (95%% CI: %.3f%%, %.3f%%)\n", 
            paf_pooled * 100, paf_ci[1] * 100, paf_ci[2] * 100))
cat(sprintf("AN:   %.1f (95%% CI: %.1f, %.1f)\n", an_pooled_total, an_ci[1], an_ci[2]))
cat(sprintf("Total cases: %d\n", total_cases_all))


disease
exp_type

MAX_LAG
LAG_DF

print(lag_cum_rr)
print(I2)

###############################################################################
#                 Sensitivity Analysis: Exclude Pandemic Years
###############################################################################

pandemic <- c(2019,2020,2021)   # c(2020, 2021, 2022)


complete_daily_data_sen <- complete_daily_data %>%
  filter(!year %in% pandemic)

date_min_sen <- min(complete_daily_data_sen$date)
date_max_sen <- max(complete_daily_data_sen$date)

complete_daily_data_sen <- complete_daily_data_sen %>%
  mutate(
    time_seq = as.numeric(date - date_min_sen) + 1,
    n_years = as.numeric(difftime(date_max_sen, date_min_sen, units = "days")) / 365.25
  )

counties_no_hw_sen <- complete_daily_data_sen %>%
  group_by(CountyCode) %>%
  summarise(total_hw_days = sum(heatwave), .groups = "drop") %>%
  filter(total_hw_days == 0)

county_code_sen <- unique(complete_daily_data_sen$CountyCode)

county_models_sen <- pblapply(county_code_sen, function(county) {
  fit_county_dlnm(complete_daily_data_sen, county, disease, exp_type)
})

successful_sen <- county_models_sen[sapply(county_models_sen, function(x) x$success)]

successful_sen <- successful_sen[sapply(successful_sen, function(x) {
  rr <- x$prediction$allRRfit[1]
  !is.na(rr) && is.finite(rr) && rr < 10 && rr > 0.01
})]

county_rrs_sen <- sapply(successful_sen, function(x) x$prediction$allRRfit[1])
county_rr_low_sen <- sapply(successful_sen, function(x) x$prediction$allRRlow[1])
county_rr_high_sen <- sapply(successful_sen, function(x) x$prediction$allRRhigh[1])

invalid_idx_sen <- !is.finite(county_rrs_sen) | 
  !is.finite(county_rr_low_sen) | 
  !is.finite(county_rr_high_sen) |
  county_rr_low_sen <= 0 | 
  county_rr_high_sen <= 0

if (any(invalid_idx_sen)) {
  cat(sprintf("⚠ Warning: Removing %d counties with invalid CI\n", sum(invalid_idx_sen)))
  successful_sen <- successful_sen[!invalid_idx_sen]
}

pooled_sen <- pool_county_estimates(successful_sen)

# ==================== Summary ===============================================
lag_cum_rr_sen <- extract_cumulative_rr(pooled_sen$pooled_prediction, disease, exp_type)
lag_cum_rr_sen <- lag_cum_rr_sen %>%
  mutate(
    sensitivity = paste0("Exclude_", paste(pandemic, collapse = "_")),
    RR_print = paste0(round(RR, 3), " (", round(RR_lower, 3), ", ", round(RR_upper, 3), ")")
  )

# Lag-specific RR
lag_specific_rr_sen <- extract_lag_specific_rr(pooled_sen$pooled_prediction, disease, exp_type)
lag_specific_rr_sen <- lag_specific_rr_sen %>%
  mutate(
    sensitivity = paste0("Exclude_", paste(pandemic, collapse = "_")),
    RR_print = paste0(round(RR, 3), " (", round(RR_lower, 3), ", ", round(RR_upper, 3), ")")
  )


##############################################
#                SAVE Results 
##############################################

base_dir <- "Fig/LagRR/Disease/Circulatory" 

output_dir <- file.path(base_dir, exp_type)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =========== Save Lag RR fig ==================
ggsave(
  filename = file.path(output_dir, sprintf("DLNM_lag_RR_%s_%s.png", exp_type, disease)),
  plot = Plot_lag_RR,
  width = 9,
  height = 3,
  dpi = 300
)

# =========== Save complte_daily_data =================
saveRDS(
  complete_daily_data,
  file = file.path(output_dir, sprintf("complete_daily_data_%s_%s.rds", exp_type, disease))
)
# =========== Save - lag_specific_rr ===================
lag_specific_rr

write.xlsx(
  lag_specific_rr,
  file = file.path(output_dir, sprintf("lag_specific_rr_%s_%s.xlsx", exp_type, disease)),
  overwrite = TRUE
)

# =========== Save - lag_cum_rr ===================
lag_cum_rr

write.xlsx(
  lag_cum_rr,
  file = file.path(output_dir, sprintf("lag_cum_rr_%s_%s.xlsx", exp_type, disease)),
  overwrite = TRUE
)
