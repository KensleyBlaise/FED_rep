# ================================================================
# FED_rep.xlsx (Sheet29) -> Fig 1 (Full vs No-COVID) + Fig 2 (Rolling OLS with CI)
# Y & C already real; deflate only Gov benefits and Ill wealth by Cons Deflator
# Rolling = simple OLS per window; store beta & OLS SE; plot line + 95% CI band
# No gridlines, black panel borders; COVID panel y-axis starts at 0.8
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)       # as.yearqtr
})

# ---------------- CONFIG ----------------
XLSX_FILE   <- "FED_rep.xlsx"
SHEET       <- "Sheet29"
BREAK_QTR   <- zoo::as.yearqtr("2012 Q1")
COVID_START <- zoo::as.yearqtr("2020 Q1")
COVID_END   <- zoo::as.yearqtr("2021 Q4")
ROLL_WIN    <- 40L   # quarters (≈10 years), even window

# Exact column names in your sheet
COL_Q <- "Q"              # e.g. "Mar-00"
COL_Y <- "Real income"    # already real
COL_C <- "Real Cons"      # already real
COL_T <- "Gov benefits"   # nominal -> deflate
COL_W <- "Ill wealth"     # nominal -> deflate
COL_P <- "Cons Deflator"  # deflator

# ---------------- LOAD & PREP -----------------
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

d <- raw %>%
  transmute(
    Q        = .data[[COL_Q]],
    Y_real   = as.numeric(.data[[COL_Y]]),
    C_real   = as.numeric(.data[[COL_C]]),
    T_nom    = as.numeric(.data[[COL_T]]),
    W_nom    = as.numeric(.data[[COL_W]]),
    P        = as.numeric(.data[[COL_P]])
  )

# Parse quarters like "Mar-00" (fallback to "Mar-2000")
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Normalize deflator if on 100=base scale (e.g., 95 -> 0.95)
if (mean(d$P, na.rm = TRUE) > 5) d$P <- d$P / 100

# Deflate ONLY transfers & wealth; build regression variables
to_real <- function(nom, p) nom / p
d <- d %>%
  mutate(
    T_real     = to_real(T_nom, P),
    W_real     = to_real(W_nom, P),
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(W_over_den)) %>%
  arrange(Date)

# ---------- helper: "nice" y-limits with small padding ----------
nice_limits <- function(vals, lower = NULL, upper = NULL, pad_mult = 0.04, digits = 2) {
  vals <- vals[is.finite(vals)]
  r <- range(vals, na.rm = TRUE); span <- max(r[2] - r[1], 1e-9); pad <- pad_mult * span
  lo <- r[1] - pad; hi <- r[2] + pad
  if (!is.null(lower)) lo <- max(lower, lo)
  if (!is.null(upper)) hi <- min(upper, hi)
  lo <- floor(lo * 10^digits) / 10^digits
  hi <- ceiling(hi * 10^digits) / 10^digits
  c(lo, hi)
}

# ---------------- Helpers for Figure 1 (baseline & break) ----------------
build_fig1_data <- function(df) {
  ols1 <- lm(ratio_data ~ W_over_den, data = df)
  df$ratio_pred <- as.numeric(predict(ols1, newdata = df))
  df <- df %>%
    mutate(post = as.integer(date >= BREAK_QTR),
           post_W = post * W_over_den)
  ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = df)
  df$ratio_pred_break <- as.numeric(predict(ols2, newdata = df))
  list(df = df, ols1 = ols1, ols2 = ols2)
}

make_fig1_plot <- function(df, title, ylims = NULL) {
  p <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = ratio_data, colour = "Data"), linewidth = 1) +
    geom_line(aes(y = ratio_pred, colour = "Predicted"),
              linewidth = 1, linetype = "dotted") +
    geom_line(aes(y = ratio_pred_break, colour = "Predicted with Trend Break"),
              linewidth = 1, linetype = "dashed") +
    scale_colour_manual(values = c("Data" = "black",
                                   "Predicted" = "#2C7FB8",
                                   "Predicted with Trend Break" = "#D95F02")) +
    labs(title = title, y = "Ratio", x = NULL, colour = "") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
      plot.background = element_blank()
    )
  if (!is.null(ylims)) p <- p + scale_y_continuous(limits = ylims, expand = expansion(mult = c(0,0)))
  p
}

# ---------------- Figure 1: full vs no-COVID side-by-side ----------------
full   <- build_fig1_data(d);         d_full <- full$df
d_nc   <- d %>% filter(date < COVID_START | date > COVID_END)
ncfit  <- build_fig1_data(d_nc);      d_nc   <- ncfit$df

ylims_full <- nice_limits(c(d_full$ratio_data, d_full$ratio_pred, d_full$ratio_pred_break))
ylims_nc   <- nice_limits(c(d_nc$ratio_data,   d_nc$ratio_pred,   d_nc$ratio_pred_break), lower = 0.8)

par(mfrow = c(1, 2))
print(make_fig1_plot(d_full, "Full Sample", ylims_full))
print(make_fig1_plot(d_nc,   "Excluding COVID (2020Q1–2021Q4)", ylims_nc))
par(mfrow = c(1, 1))  # reset

# ---------------- Figure 2: Rolling OLS (store beta & OLS SE) -----------------
# Pre/post MPC reference levels from full-sample break fit
co <- coef(full$ols2)
beta_pre  <- unname(co["W_over_den"])
delta_name <- if ("W_over_den:post" %in% names(co)) "W_over_den:post" else "post_W"
beta_post <- beta_pre + unname(co[delta_name])

seg_df <- data.frame(
  xstart = as.Date(c(min(d$Date), as.Date(as.yearqtr(BREAK_QTR)))),
  xend   = as.Date(c(as.Date(as.yearqtr(BREAK_QTR)) - 90, max(d$Date))),  # ~one quarter earlier
  beta   = c(beta_pre, beta_post)
)

# Simple rolling OLS engine (centered window)
if (nrow(d) < ROLL_WIN) stop("Need at least ", ROLL_WIN, " quarters; found: ", nrow(d))
n <- nrow(d)
rows <- vector("list", length = n - ROLL_WIN + 1L)
for (s in 1:(n - ROLL_WIN + 1L)) {
  e <- s + ROLL_WIN - 1L
  center <- s + (ROLL_WIN/2 - 1L)  # for 40: s+19
  sub <- d[s:e, , drop = FALSE]

  # Skip windows without variation or with NAs
  if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
      var(sub$W_over_den, na.rm = TRUE) <= 0) {
    rows[[s]] <- data.frame(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  fit <- try(lm(ratio_data ~ W_over_den, data = sub), silent = TRUE)
  if (inherits(fit, "try-error") || is.na(coef(fit)["W_over_den"])) {
    rows[[s]] <- data.frame(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  b  <- as.numeric(coef(fit)["W_over_den"])
  se <- tryCatch(
    summary(fit)$coef["W_over_den", "Std. Error"],
    error = function(e) NA_real_
  )
  if (!is.finite(b))  b  <- NA_real_
  if (!is.finite(se)) se <- NA_real_

  rows[[s]] <- data.frame(Date = d$Date[center], beta = b, se = se)
}

roll <- do.call(rbind, rows) %>%
  mutate(
    beta_cents = 100 * beta,
    lo = 100 * (beta - 1.96 * se),
    hi = 100 * (beta + 1.96 * se)
  )
# Import libraries -----------------------
library(readxl)
library(sandwich)
library(MatrixModels)
library(midasr)
library(sn)
library(nloptr)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(xts)
library(ggplot2)
library(moments)
library(tseries)
library(quantreg)
library(openxlsx)

### Use this code to install sn packages if unable to directly download: 

#install.packages("sn_2.1.1.tar.gz", repos = NULL)
#source("N:/CECD/10. Personal/Giulia Mantoan/Density nowcasting CECD/Code/functions/sn/R/sn")
#install.packages("sn_2.1.1.tar.gz", repos=NULL, type='source')

# Load functions -----------------------
setwd("C:/Users/344806/OneDrive - Bank of England/Desktop/density_nowcasting")
source("./functions/mix_data.R")
source("./functions/cecd_midas_q.R")
source("./functions/crps.R")
source("./functions/qs.R")
source("./functions/quant_comb_w.R")
source("./functions/quant_comb_TV_w.R")
source("./functions/merge_midas_q.R")
source("./functions/QuantilesInterpolation.R")
source("./functions/alt_fmls.R")
source("./functions/u_midas_q_pred.R")
source("./functions/MPZdens.R")
source("./functions/format_exports.R")

# Create results folder -----------------------
results_filepath <- paste0("./results/", format(today(), "%b %Y"), "/") # update this line for new results vintages
if (!dir.exists(results_filepath)) {
  dir.create(results_filepath)
}

# Load official GDP data -----------------------
gdp <- read_xlsx("./data/gdp.xlsx", sheet = "Sheet1", col_names = TRUE, 
                 col_types = c("date", "numeric"))

gdpstart_date <- c(as.numeric(format(min(gdp$date), '%Y')), as.numeric(format(min(gdp$date), '%m')) %/% 3)
gdpend_date <- c(as.numeric(format(max(gdp$date), '%Y')), as.numeric(format(max(gdp$date), '%m')) %/% 3)
gdpts <- ts(gdp$gdp, start=gdpstart_date, end=gdpend_date, frequency=4)
window(gdpts, start=c(2020, 1), end=c(2021, 4))<- NA # covid

# Load soft data -----------------------
high_freq <- read_xlsx("./data/high_frequency_stat.xlsx", sheet = "Sheet1", col_names = TRUE, 
                       col_types = c("date", rep("numeric", 8)))
high_freq_start_date <- c(as.numeric(format(min(high_freq$Date), '%Y')), as.numeric(format(min(high_freq$Date), '%m')))
high_freq_end_date <- c(as.numeric(format(max(high_freq$Date), '%Y')), as.numeric(format(max(high_freq$Date), '%m')))
high_freq_ts=ts(high_freq, start=high_freq_start_date, end=high_freq_end_date, frequency=12) 
window(high_freq_ts, start=c(2020, 1), end=c(2021, 12))<- NA
high_freq_ts=ts(high_freq_ts, start=high_freq_start_date, end=high_freq_end_date, frequency=12) 

# Set up time index -----------------------
time_index<-time(high_freq_ts)

insample_length <- round(length(time_index)[1]/3)*2
yearmon_index <- as.yearmon(time_index)
yearmon_index<-yearmon_index[insample_length:length(yearmon_index)]

years <- as.integer(format(yearmon_index, "%Y"))
months <- as.integer(format(yearmon_index, "%m"))
time_vector <- cbind(years, months)

# Load hard data -----------------------
mgdp <- read_xlsx("./data/mgdp_fe.xlsx", sheet = "Data", col_names = TRUE, 
                  col_types = c("date", "numeric"))
mgdp_start_date <- c(as.numeric(format(min(mgdp$Date), '%Y')), as.numeric(format(min(mgdp$Date), '%m')))
mgdp_end_date <- c(as.numeric(format(max(mgdp$Date), '%Y')), as.numeric(format(max(mgdp$Date), '%m')))
mgdp_ts=ts(mgdp[,2], start=mgdp_start_date, end=mgdp_end_date, frequency=12) 
window(mgdp_ts, start=c(2020, 1), end=c(2021, 12))<- NA
time_index_mgdp <-time(mgdp_ts)

post_covid<-match(c(2022),time_vector)
rest_oftime <- length(time_vector[,1])-post_covid

ind_names <- c('risk', 'uncertainty', 'housing', 'labour', 'output', 'futureoutput', 'neworders', 'retail')

# Set up quantiles -----------------------
QQ=c(0.1,0.2, 0.3, 0.4 ,0.50, 0.6, 0.7, 0.8, 0.90)
Q=length(QQ)

# Prepare empty dataframes -----------------------
q_score_k_df <- array(NA, dim = c(rest_oftime, length(names(high_freq))-1, Q)) 
q_score_u_k_df <- array(NA, dim = c(rest_oftime, length(names(high_freq))-1, Q))
y_hat_df <- array(NA, dim = c(rest_oftime, length(names(high_freq))-1, Q))
y_hat_umidas <- array(NA, dim = c(rest_oftime, length(names(high_freq))-1, Q))

q_score_mgdp <- array(NA, dim = c(rest_oftime, Q))
q_score_u_mgdp<- array(NA, dim = c(rest_oftime, Q))
q_score_qcombsurvey<- array(NA, dim = c(rest_oftime, Q))
q_score_u_qcombsurvey<- array(NA, dim = c(rest_oftime, Q))
y_hat_mgdp <- array(NA, dim = c(rest_oftime, Q))
y_u_mgdp<- array(NA, dim = c(rest_oftime, Q))
kcomb_surveys_w<- array(NA, dim = c(length(names(high_freq))-1, Q,rest_oftime))
kcomb_u_surveys_w<- array(NA, dim = c(length(names(high_freq))-1, Q,rest_oftime))
kcomb_surveys_y<- array(NA, dim = c(rest_oftime,Q))
kcomb_u_surveys_y<- array(NA, dim = c(rest_oftime,Q))

kcomb_uu_w<- array(NA, dim = c(2, Q,rest_oftime))
kcomb_uu_y <- array(NA, dim = c(rest_oftime,Q))
q_score_uu<-array(NA, dim = c(rest_oftime, 2, Q))
q_score_all <- array(NA, dim = c(rest_oftime,3))
q_score_survey <- array(NA, dim = c(rest_oftime,8))

# Set up quantile function -----------------------
theta_h0 <- function(p, dk, ...) {
  i <- (1:dk-1)/100
  pol <- p[3]*i + p[4]*i^2
  (p[1] + p[2]*i)*exp(pol)
}

# Allocate lags and performance period -----------------------
lags_xhf <- 4
lags_mgdp <- 4

decay<-8
STW <- post_covid+4 

# Run quantile MIDAS -----------------------
for (f in STW:length(time_vector[,1])) { 
  
  # Run soft data individual models
  for (k in 1:(ncol(high_freq_ts)-1)){
    
    hs_ts_clean <- window(high_freq_ts, start=start(high_freq_ts), end=time_vector[f,])
    hs_ts_clean <- hs_ts_clean[!is.na(hs_ts_clean[1:dim(hs_ts_clean)[1],k+1]), c(k+1)]
    hs_ts_clean <- ts(hs_ts_clean,  end=time_vector[f,], frequency=12)
    
    time_q <- c(as.numeric(time_vector[f,1]), (as.numeric(time_vector[f,2])%/% 3))
    
    if (time_q[2]==0){
      gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f,1])-1, 4)) 
      gdpts_clean<- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f,1])-1, 4), frequency = 4)
    } else if (time_q[2]==1){
      gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f,1]), 1)) 
      gdpts_clean<- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f,1]), 1), frequency = 4)
    } else if (time_q[2]==2){
      gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f,1]), 2)) 
      gdpts_clean<- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f,1]), 2), frequency = 4)
    } else {
      gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f,1]), 3)) 
      gdpts_clean<- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f,1]), 3), frequency = 4)
    }
    
    mix_data_d <- mix_data(hs_ts_clean, gdpts_clean, time_vector[f,])
    
    Umidas_survey <- u_midas_q_pred(mix_data_d$y_p, mix_data_d$x_p,lags_xhf, QQ)
    
    y_hat_umidas[f-STW+1,k, ]<-tail(Umidas_survey$y_hat,1)
    
    if (f==62){
      y_hat_umidas_ev <- tail(Umidas_survey$y_hat,4)[1]
    } else {
      y_hat_umidas_ev <- y_hat_umidas[f-STW-2,k, ]
    }
    
    y_out<-window(gdpts, start=mix_data_d$end_of_quarter_qev, end=mix_data_d$end_of_quarter_qev) 
    q_score_u<- crps(y_hat_umidas_ev, as.numeric(y_out), Q,2)
    q_score_u_k_df[f-STW+1,k, ] <- q_score_u
    
  }
  
  # Combine individual soft data models 
  kcomb_u_surveys_TV <- quant_comb_TV_w(q_score_u_k_df[1:(f-STW+1), , ],y_hat_umidas[f-STW+1, , ], decay, f-STW+1)
  kcomb_u_surveys_w[,,f-STW+1]<-kcomb_u_surveys_TV$wq
  kcomb_u_surveys_y[f-STW+1,]<-kcomb_u_surveys_TV$y_comb
  q_score_u_qcombsurvey[f-STW+1,]<- crps(kcomb_u_surveys_TV$y_comb, as.numeric(y_out), Q,2)
  
  # Run hard data model
  mgdp_ts_clean <- window(mgdp_ts, start=start(mgdp_ts), end=time_vector[f-1,])
  mgdp_time <- max(time(mgdp_ts_clean))
  mgdp_ts_clean <- mgdp_ts_clean[!is.na(mgdp_ts_clean)]
  mgdp_ts_clean <- ts(mgdp_ts_clean,  end=mgdp_time, frequency=12)
  
  mix_data_mgdp <- mix_data(mgdp_ts_clean, gdpts_clean, c(time_vector[f,]))
  Umidas_mgdp <- u_midas_q_pred(mix_data_mgdp$y_p, mix_data_mgdp$x_p,lags_mgdp, QQ)
  
  y_u_mgdp[f-STW+1,]<- tail(Umidas_mgdp$y_hat,1)
  
  if (f==62){
    y_u_mgdp_ev <- tail(Umidas_mgdp$y_hat,4)[1]
  } else {
    y_u_mgdp_ev <- y_u_mgdp[f-STW-2, ]
  }
  
  q_score_u_mgdp[f-STW+1,] <- crps(y_u_mgdp_ev, as.numeric(y_out), Q, 2) # evaluate how accurate each k model is in each quantile QQ
  
  # Combine soft and hard data models
  q_score_uu[1:(f-STW+1), 1, ]<-q_score_u_qcombsurvey[1:(f-STW+1),]
  q_score_uu[1:(f-STW+1), 2, ]<-q_score_u_mgdp[1:(f-STW+1),]
  y_hat_combs_uu<-rbind(kcomb_u_surveys_TV$y_comb, y_u_mgdp[f-STW+1,])
  kcomb_mgdp_uu_TV <- quant_comb_TV_w(q_score_uu[1:(f-STW+1), , ], y_hat_combs_uu, decay, f-STW+1)
  kcomb_uu_w[,,f-STW+1]<-kcomb_mgdp_uu_TV$wq
  kcomb_uu_y[f-STW+1,]<-kcomb_mgdp_uu_TV$y_comb
  
  # Check model performance
  q_score_all[f-STW+1,1] <- mean(crps(kcomb_mgdp_uu_TV$y_comb, as.numeric(y_out), Q,2))
  q_score_all[f-STW+1,2] <- mean(q_score_u_qcombsurvey[f-STW+1,])
  q_score_all[f-STW+1,3] <- mean(q_score_u_mgdp[f-STW+1,])
  
  q_score_survey[f-STW+1,1] <- mean(q_score_u_k_df[f-STW+1,1, ])
  q_score_survey[f-STW+1,2] <- mean(q_score_u_k_df[f-STW+1,2, ])
  q_score_survey[f-STW+1,3] <- mean(q_score_u_k_df[f-STW+1,3, ])
  q_score_survey[f-STW+1,4] <- mean(q_score_u_k_df[f-STW+1,4, ])
  q_score_survey[f-STW+1,5] <- mean(q_score_u_k_df[f-STW+1,5, ])
  q_score_survey[f-STW+1,6] <- mean(q_score_u_k_df[f-STW+1,6, ])
  q_score_survey[f-STW+1,7] <- mean(q_score_u_k_df[f-STW+1,7, ])
  q_score_survey[f-STW+1,8] <- mean(q_score_u_k_df[f-STW+1,8, ])
  
}

# Format dates for outputs -----------------------
month_str <- sprintf("%02d", as.numeric(time_vector[STW:length(time_vector[,1]),2]))
date_str <- paste(time_vector[STW:length(time_vector[,1]),1], month_str, "01", sep = "-")
dates <- as.Date(date_str, format = "%Y-%m-%d")
time_length <- length(time_vector[STW:length(time_vector[,1])])

# Export headline quantile results -----------------------
kcomb_uu_y_df <- format_quantile_df(kcomb_uu_y, time_length, dates)
kcomb_u_surveys_y_df <- format_quantile_df(kcomb_u_surveys_y, time_length, dates)
y_u_mgdp_df <- format_quantile_df(y_u_mgdp, time_length, dates)

dataset_names <- list('kcomb_uu_y' = kcomb_uu_y_df, 'kcomb_u_surveys_y' = kcomb_u_surveys_y_df, 'y_u_mgdp_df' = y_u_mgdp_df)
write.xlsx(dataset_names, file = paste0(results_filepath, 'quantile_results.xlsx'))

# Export individual soft data quantile results -----------------------
y_hat_umidas_list <- lapply(1:8, function(i) {
  format_quantile_df(y_hat_umidas[, i, ], time_length, dates)
})
list2env(setNames(y_hat_umidas_list, paste0("y_hat_umidas_", 1:8)), envir = .GlobalEnv)

indv_dataset_names <- list('risk' = y_hat_umidas_1, 'uncertainty' = y_hat_umidas_2, 'housing' = y_hat_umidas_3,
                           'labour' = y_hat_umidas_4, 'output' = y_hat_umidas_5, 'futureoutput' = y_hat_umidas_6,
                           'neworders' = y_hat_umidas_7, 'retail' = y_hat_umidas_8)
write.xlsx(indv_dataset_names, file = paste0(results_filepath, 'soft_quantile_results.xlsx'))

# Export CRPS results -----------------------
q_score_all_df <- format_crps_df(q_score_all, time_length, dates)
q_score_survey_df <- format_crps_df(q_score_survey, time_length, dates)

crps_dataset_names <- list('crps_results' = q_score_all_df, 'soft_crps_results' = q_score_survey_df)
write.xlsx(crps_dataset_names, file = paste0(results_filepath,'crps_results.xlsx'))

# Process weights -----------------------
weights_df <- format_weights_df(kcomb_uu_w, time_length, dates)
weights_df <- weights_df %>%
  rename_with(~ gsub("ind1", "soft_data", .x)) %>%
  rename_with(~ gsub("ind2", "hard_data", .x))

soft_weights_df <- format_weights_df(kcomb_u_surveys_w, time_length, dates)

weights_list <- list()
soft_weights_list <- list()
combined_weights <- list()
scaled_weights  <- list()

for (i in 1:length(QQ)) {
  
  prefix <- paste0("quantile", i)
  weights_list[[prefix]] <- weights_df[, grep(paste0("^", prefix), names(weights_df)), drop = FALSE]
  soft_weights_list[[prefix]] <- soft_weights_df[, grep(paste0("^", prefix), names(soft_weights_df)), drop = FALSE]
  
  combined_weights[[prefix]] <- cbind(soft_weights_list[[prefix]], weights_list[[prefix]])
  
  soft_col <- combined_weights[[prefix]][, ncol(combined_weights[[prefix]])-1]
  
  scaled_weights[[prefix]] <- sweep(combined_weights[[prefix]][,1:8], 1, soft_col, `*`)
  scaled_weights[[prefix]] <- cbind(scaled_weights[[prefix]], weights_list[[prefix]])
  
  all_weights_df <- do.call(cbind, scaled_weights)
  colnames(all_weights_df) <- gsub("^quantile[1-9]\\.", "", colnames(all_weights_df))
  
  all_weights_df <- all_weights_df %>%
    mutate(dates = dates[1:time_length]) %>%
    select(dates, everything())
  
}

# Export weights results -----------------------
dataset_names <- list('scaled_weights' = all_weights_df, 'unscaled_soft_weights' = soft_weights_df)
write.xlsx(dataset_names, paste0(results_filepath, 'weights_results.xlsx'))

# Process quantile contributions -----------------------
soft_fcast_list <- lapply(1:length(QQ), function(i) {
  df <- format_fcast_df(y_hat_umidas[, , i], time_length, dates)
  df <- df[, -1]  
  df$hard_data <- y_u_mgdp_df[[i+1]] 
  df
})

weight_slices <- lapply(0:8, function(i) {
  block_start <- i * 10 + 1
  cols <- c((block_start + 1):(block_start + 8), block_start + 10)
  all_weights_df[, cols]
})

contributions_list <- mapply(function(forecast, weight) {
  forecast * weight }, 
  soft_fcast_list, weight_slices, SIMPLIFY = FALSE)

contributions_list <- lapply(1:9, function(i) {
  format_conts(contributions_list[[i]], time_length, dates)
})

list2env(setNames(contributions_list, paste0("conts_", 1:9)), envir = .GlobalEnv)

# Export quantile contribution results -----------------------
conts_dataset_names <- list('10th' = conts_1, '20th' = conts_2, '30th' = conts_3,
                            '40th' = conts_4, 'median' = conts_5, '60th' = conts_6,
                            '70th' = conts_7, '80th' = conts_8, '90th' = conts_9)
write.xlsx(conts_dataset_names, file = paste0(results_filepath, 'contributions_results.xlsx'))


# Set parameters for density results -----------------------

# --- to run only the most recent quarters use the below 5 lines of code 
last_3_months <- tail(seq_along(time_vector[,2]), 3)
eoq_indices <- which(time_vector[,2] %in% c("3", "6", "9", "12"))
selected_f <- sort(unique(c(tail(eoq_indices,4), last_3_months)))
##num_of_dens = length(selected_f) + 1
startdens = min(selected_f)

# --- to run the past 15 quarters use the below 3 lines of code 
#startdens = length(time_vector[,1])-15
##num_of_dens = length(time_vector[,1])-startdens +1
#selected_f <- startdens:length(time_vector[,1])

num_of_dens <- length(selected_f)

skew_t_param <- array(NA, dim = c(num_of_dens, 4))
ycomb_pdf_qq <- array(NA, dim = c(num_of_dens, Q))
ycomb_pdf <- array(NA, dim = c(num_of_dens, 101))

s1=min(gdpts_clean, y_out)-3
s2=max(gdpts_clean, y_out)+3
nbins=100;
hbins=0.5;
incr=(s2-s1)/nbins;
bins=seq(from=s1, to=s2, by=incr)

below_zero<- array(NA, dim = c(num_of_dens,1))

# Run skew-t density results for full model -----------------------
for (i in seq_along(selected_f)) {
  f <- selected_f[i]
  a <- QuantilesInterpolation(kcomb_uu_y[f-STW+1, ], QQ)
  skew_t_param[i, 1] <- a[[1]]
  skew_t_param[i, 2] <- a[[2]]
  skew_t_param[i, 3] <- a[[3]]
  skew_t_param[i, 4] <- a[[4]]
  
  ycomb_pdf_qq[i,] <- sn::qst(QQ, 
                              xi=skew_t_param[i,1], 
                              omega=skew_t_param[i,2], 
                              alpha=skew_t_param[i,3], 
                              nu=skew_t_param[i,4])
  
  ycomb_pdf[i,] <- sn::dst(bins, 
                           xi=skew_t_param[i,1], 
                           omega=skew_t_param[i,2], 
                           alpha=skew_t_param[i,3], 
                           nu=skew_t_param[i,4])
  
  below_zero[i,] <- sn::psn(0, a[[1]], a[[2]], a[[3]])
  
}

ycomb_pdf_df <- as.data.frame(t(ycomb_pdf))
time_periods <- paste(month.abb[time_vector[selected_f,2]], time_vector[selected_f,1], sep = "-")
ycomb_pdf_df <- cbind(bins, ycomb_pdf_df)
colnames(ycomb_pdf_df) <- c("bins", time_periods)

# Run skew-t density results for combined soft data model -----------------------
skew_t_param_survey <- array(NA, dim = c(num_of_dens,4))
ycomb_pdf_qq_survey <- array(NA, dim = c(num_of_dens,Q))
ycomb_pdf_survey <- array(NA, dim = c(num_of_dens,101)) 

for (i in seq_along(selected_f)) {
  f <- selected_f[i]
  a <- QuantilesInterpolation(kcomb_u_surveys_y[f-STW+1,],QQ) 
  skew_t_param[i,1] <- a[[1]]
  skew_t_param[i,2] <- a[[2]]
  skew_t_param[i,3] <- a[[3]]
  skew_t_param[i,4] <- a[[4]]
  
  ycomb_pdf_qq_survey[i,] <- sn::qst(QQ, 
                                     xi=skew_t_param[i,1], 
                                     omega=skew_t_param[i,2], 
                                     alpha=skew_t_param[i,3], 
                                     nu=skew_t_param[i,4])
  
  ycomb_pdf_survey[i,] <- sn::dst(bins, 
                                  xi=skew_t_param[i,1], 
                                  omega=skew_t_param[i,2], 
                                  alpha=skew_t_param[i,3], 
                                  nu=skew_t_param[i,4])
}

ycomb_pdf_survey_df <- as.data.frame(t(ycomb_pdf_survey))
ycomb_pdf_survey_df <- cbind(bins, ycomb_pdf_survey_df)
colnames(ycomb_pdf_survey_df) <- c("bins", time_periods)

# Run skew-t density results for hard data model -----------------------
skew_t_param_mgdp <- array(NA, dim = c(num_of_dens,4))
ycomb_pdf_qq_mgdp <- array(NA, dim = c(num_of_dens,Q))
ycomb_pdf_mgdp <- array(NA, dim = c(num_of_dens,101)) 

for (i in seq_along(selected_f)) {
  f <- selected_f[i]
  a <- QuantilesInterpolation(y_u_mgdp[f-STW+1,],QQ) 
  skew_t_param[i,1] <- a[[1]]
  skew_t_param[i,2] <- a[[2]]
  skew_t_param[i,3] <- a[[3]]
  skew_t_param[i,4] <- a[[4]]
  
  ycomb_pdf_qq_mgdp[i,] <- sn::qst(QQ, 
                                   xi=skew_t_param[i,1], 
                                   omega=skew_t_param[i,2], 
                                   alpha=skew_t_param[i,3], 
                                   nu=skew_t_param[i,4])
  
  ycomb_pdf_mgdp[i,] <- sn::dst(bins, 
                                xi=skew_t_param[i,1], 
                                omega=skew_t_param[i,2], 
                                alpha=skew_t_param[i,3], 
                                nu=skew_t_param[i,4])
}

ycomb_pdf_mgdp_df <- as.data.frame(t(ycomb_pdf_mgdp))
ycomb_pdf_mgdp_df <- cbind(bins, ycomb_pdf_mgdp_df)
colnames(ycomb_pdf_mgdp_df) <- c("bins", time_periods)

dataset_names <- list('densities_all' = ycomb_pdf_df, 'densities_hf' = ycomb_pdf_survey_df, 'densities_mgdp' = ycomb_pdf_mgdp_df)
write.xlsx(dataset_names, paste0(results_filepath, 'density_results.xlsx'))


# Run density results for individual soft data models -----------------------
num_of_dens=k
skew_t_param<- array(NA, dim = c(num_of_dens,4))
ycomb_pdf_qq_mgdp <- array(NA, dim = c(num_of_dens,Q))
ycomb_pdf_hf <- array(NA, dim = c(num_of_dens,101)) 

for (kj in 1:k) {
  a <- QuantilesInterpolation(y_hat_umidas[f-STW+1,kj , ],QQ) # this takes a bit
  skew_t_param[kj,1] <- a[[1]]
  skew_t_param[kj,2] <- a[[2]]
  skew_t_param[kj,3] <- a[[3]]
  skew_t_param[kj,4] <- a[[4]]
  
  ycomb_pdf_hf[kj,] <- sn::dst(bins, 
                               xi=skew_t_param[kj,1], 
                               omega=skew_t_param[kj,2], 
                               alpha=skew_t_param[kj,3], 
                               nu=skew_t_param[kj,4])
}

ycomb_pdf_hf_df <- as.data.frame(t(ycomb_pdf_hf))
ycomb_pdf_hf_df <- cbind(bins, ycomb_pdf_hf_df)
colnames(ycomb_pdf_hf_df) <- c("bins", ind_names)

dataset_names <- list('densities_ind' = ycomb_pdf_hf_df)
write.xlsx(dataset_names, paste0(results_filepath, 'density_results_individual.xlsx'))


# Run density results for non-parametric distribution -----------------------
n_bins <- 512
num_of_dens <- length(selected_f)
ycomb_pdf_nonpar <- array(NA, dim = c(num_of_dens,n_bins))
ycomb_pdf_nonpar_surv<- array(NA, dim = c(num_of_dens,n_bins))
ycomb_pdf_nonpar_mgdp<- array(NA, dim = c(num_of_dens,n_bins))

ycomb_pdf_nonpar_bins <- array(NA, dim = c(num_of_dens,n_bins))
ycomb_pdf_nonpar_surv_bins<- array(NA, dim = c(num_of_dens,n_bins))
ycomb_pdf_nonpar_mgdp_bins<- array(NA, dim = c(num_of_dens,n_bins))

#below zero
ycomb_cdf_nonpar<- array(NA, dim = c(num_of_dens,20000))
below_zero_nonpar<-array(NA, dim = c(num_of_dens,1))

for (i in seq_along(selected_f)) {
  f <- selected_f[i]
  nonpar_comb <- MPZdens(kcomb_uu_y[f-STW+1,],QQ)
  ycomb_pdf_nonpar[i,] <- density(nonpar_comb)$y
  ycomb_pdf_nonpar_bins[i,] <- density(nonpar_comb)$x
  #below zero
  ecdf_fun<-ecdf(nonpar_comb)
  below_zero_nonpar[i,]<-ecdf_fun(0)
  
  nonpar_comb_sur <- MPZdens(kcomb_u_surveys_y[f-STW+1,],QQ)
  ycomb_pdf_nonpar_surv[i,] <- density(nonpar_comb_sur)$y
  ycomb_pdf_nonpar_surv_bins[i,] <- density(nonpar_comb_sur)$x
  
  nonpar_comb_mgdp <- MPZdens(y_u_mgdp[f-STW+1,],QQ)
  ycomb_pdf_nonpar_mgdp[i,] <- density(nonpar_comb_mgdp)$y
  ycomb_pdf_nonpar_mgdp_bins[i,] <- density(nonpar_comb_mgdp)$x
}

ycomb_pdf_nonpar_df <- as.data.frame(t(ycomb_pdf_nonpar))
colnames(ycomb_pdf_nonpar_df) <- time_periods
ycomb_pdf_nonpar_bins_df <- as.data.frame(t(ycomb_pdf_nonpar_bins))
colnames(ycomb_pdf_nonpar_bins_df) <- paste("bins", time_periods, sep = "_")
ycomb_pdf_nonpar_comb_df <- cbind(ycomb_pdf_nonpar_bins_df, ycomb_pdf_nonpar_df)
ycomb_pdf_nonpar_comb_df <- select(ycomb_pdf_nonpar_comb_df, 1,7,2,8,3,9,4,10,5,11,6,12)

ycomb_pdf_nonpar_surv_df <- as.data.frame(t(ycomb_pdf_nonpar_surv))
colnames(ycomb_pdf_nonpar_surv_df) <- time_periods
ycomb_pdf_nonpar_surv_bins_df <- as.data.frame(t(ycomb_pdf_nonpar_surv_bins))
colnames(ycomb_pdf_nonpar_surv_bins_df) <- paste("bins", time_periods, sep = "_")
ycomb_pdf_nonpar_surv_comb_df <- cbind(ycomb_pdf_nonpar_surv_bins_df, ycomb_pdf_nonpar_surv_df)
ycomb_pdf_nonpar_surv_comb_df <- select(ycomb_pdf_nonpar_surv_comb_df, 1,7,2,8,3,9,4,10,5,11,6,12)

ycomb_pdf_nonpar_mgdp_df <- as.data.frame(t(ycomb_pdf_nonpar_mgdp))
colnames(ycomb_pdf_nonpar_mgdp_df) <- time_periods
ycomb_pdf_nonpar_mgdp_bins_df <- as.data.frame(t(ycomb_pdf_nonpar_mgdp_bins))
colnames(ycomb_pdf_nonpar_mgdp_bins_df) <- paste("bins", time_periods, sep = "_")
ycomb_pdf_nonpar_mgdp_comb_df <- cbind(ycomb_pdf_nonpar_mgdp_bins_df, ycomb_pdf_nonpar_mgdp_df)
ycomb_pdf_nonpar_mgdp_comb_df <- select(ycomb_pdf_nonpar_mgdp_comb_df, 1,7,2,8,3,9,4,10,5,11,6,12)


dataset_names <- list('densities_all' = ycomb_pdf_nonpar_comb_df, 'densities_hf' = ycomb_pdf_nonpar_surv_comb_df, 'densities_mgdp' = ycomb_pdf_nonpar_mgdp_comb_df)
write.xlsx(dataset_names, paste0(results_filepath, 'nonparam_density_results.xlsx'))

# Save all results (for markdown report) -----------------------
save.image(paste0(results_filepath, "all_results.RData"))

# Build datasets: line uses finite betas; ribbon uses finite lo/hi
roll_line <- subset(roll, is.finite(beta_cents))
roll_band <- subset(roll, is.finite(lo) & is.finite(hi))

# "Nice" y-limits for rolling chart (classic paper range 2.5–3.5)
ylims_roll <- c(2.5, 3.5)

fig2 <- ggplot() +
  { if (nrow(roll_band) > 0)
      geom_ribbon(data = roll_band,
                  aes(x = Date, ymin = lo, ymax = hi),
                  inherit.aes = FALSE,
                  fill = "#FB9A99", alpha = 0.35) } +
  geom_line(data = roll_line, aes(x = Date, y = beta_cents),
            color = "#E31A1C", linewidth = 1.0, na.rm = TRUE) +
  geom_segment(data = seg_df,
               aes(x = xstart, xend = xend, y = 100*beta, yend = 100*beta),
               inherit.aes = FALSE, linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = ylims_roll, breaks = seq(2.5, 3.5, 0.2), expand = expansion(mult = c(0,0))) +
  labs(title = "Figure 2. Rolling 10-year MPC (OLS 95% CI)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.background = element_blank()
  )

print(fig2)
# ================================================================
