# ================================================================
# FED_rep.xlsx (Sheet29) -> Figure 1 (full vs no-COVID) + Figure 2
# Y & C already real; T & W deflated by Cons Deflator
# Fixed version to properly replicate Figure 2
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)        # as.yearqtr
  library(slider)     # rolling windows
  library(sandwich)   # robust vcov (Fig 1)
  library(lmtest)     # coeftest
  library(gridExtra)  # for side-by-side plots
})

# ---------------- CONFIG ----------------
XLSX_FILE   <- "FED_rep.xlsx"
SHEET       <- "Sheet29"
BREAK_QTR   <- zoo::as.yearqtr("2012 Q1")
COVID_START <- zoo::as.yearqtr("2020 Q1")
COVID_END   <- zoo::as.yearqtr("2021 Q4")

# Exact column names in the sheet
COL_Q <- "Q"              # quarter like "Mar-00"
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
    Y_real   = as.numeric(.data[[COL_Y]]),   # already real
    C_real   = as.numeric(.data[[COL_C]]),   # already real
    T_nom    = as.numeric(.data[[COL_T]]),   # nominal
    W_nom    = as.numeric(.data[[COL_W]]),   # nominal
    P        = as.numeric(.data[[COL_P]])    # deflator
  )

# Parse quarters like "Mar-00" (fallback to "Mar-2000")
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Normalize deflator if it's on a 100=base scale (e.g., 95 → 0.95)
if (mean(d$P, na.rm = TRUE) > 5) {
  message("Rescaling deflator by /100 (100=base scale detected).")
  d$P <- d$P / 100
}

# Make T & W real; build regression variables
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

# ---------------- helpers for Figure 1 ----------------
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
    theme(panel.grid = element_blank())
  if (!is.null(ylims)) p <- p + coord_cartesian(ylim = ylims)
  p
}

# ---------------- Figure 1: full sample ----------------
full <- build_fig1_data(d)
d_full <- full$df

# ---------------- Figure 1b: excluding COVID (2020Q1–2021Q4) ----------------
d_nocovid <- d %>% filter(date < COVID_START | date > COVID_END)
nocovid <- build_fig1_data(d_nocovid)
d_nc <- nocovid$df

# Identical y-limits for comparability
ylims <- range(
  d_full$ratio_data, d_full$ratio_pred, d_full$ratio_pred_break,
  d_nc$ratio_data,   d_nc$ratio_pred,   d_nc$ratio_pred_break,
  na.rm = TRUE
)

# Side-by-side panels using gridExtra
fig1a <- make_fig1_plot(d_full, "Full Sample", ylims)
fig1b <- make_fig1_plot(d_nc,   "Excluding COVID (2020Q1–2021Q4)", ylims)
grid.arrange(fig1a, fig1b, ncol = 2)

# ---------------- Figure 2: Rolling 10-year MPC with confidence bands ----------------
# Pre/post MPC from full-sample break model
bhat <- coef(full$ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

# Create trend break lines
seg_df <- data.frame(
  xstart = c(min(d$Date), as.Date(BREAK_QTR)),
  xend   = c(as.Date(BREAK_QTR) - 90, max(d$Date)), # subtract ~3 months
  beta   = c(beta_pre, beta_post)
)

# Enhanced rolling β builder with confidence intervals
build_roll_with_ci <- function(df, window_size = 40) {
  results <- list()
  n <- nrow(df)
  
  for (i in (window_size):(n)) {
    # Get 10-year (40 quarters) window
    window_data <- df[(i - window_size + 1):i, ]
    
    if (nrow(window_data) >= window_size && 
        sum(is.finite(window_data$ratio_data)) >= window_size * 0.8) {
      
      tryCatch({
        model <- lm(ratio_data ~ W_over_den, data = window_data)
        coef_est <- coef(model)["W_over_den"]
        se <- summary(model)$coefficients["W_over_den", "Std. Error"]
        
        results[[length(results) + 1]] <- data.frame(
          date = window_data$date[ceiling(nrow(window_data)/2)],
          Date = window_data$Date[ceiling(nrow(window_data)/2)],
          beta = as.numeric(coef_est),
          se = se,
          beta_cents = 100 * as.numeric(coef_est),
          lower = 100 * (as.numeric(coef_est) - 1.96 * se),
          upper = 100 * (as.numeric(coef_est) + 1.96 * se)
        )
      }, error = function(e) {
        # Skip this window if there's an error
      })
    }
  }
  
  do.call(rbind, results)
}

# Calculate rolling estimates
roll <- build_roll_with_ci(d, window_size = 40)

if (nrow(roll) == 0) {
  stop("No valid rolling window estimates could be calculated. Check your data.")
}

message("Rolling 40q window: ", nrow(roll), " estimates calculated")
message("Beta range: ", round(min(roll$beta_cents, na.rm = TRUE), 2), " to ", 
        round(max(roll$beta_cents, na.rm = TRUE), 2), " cents")

# Add recession shading (approximate dates for major recessions)
recession_data <- data.frame(
  start = as.Date(c("1990-07-01", "2001-03-01", "2007-12-01")),
  end = as.Date(c("1991-03-01", "2001-11-01", "2009-06-01"))
)

# Create Figure 2 matching the original
fig2 <- ggplot() +
  # Add recession shading
  geom_rect(data = recession_data, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray80", alpha = 0.7) +
  # Add confidence band (pink shaded area)
  geom_ribbon(data = roll, aes(x = Date, ymin = lower, ymax = upper),
              fill = "#FF6B6B", alpha = 0.3) +
  # Add main rolling estimate line (red)
  geom_line(data = roll, aes(x = Date, y = beta_cents),
            color = "#FF6B6B", linewidth = 1.2) +
  # Add trend break horizontal lines (dashed black)
  geom_segment(data = seg_df,
               aes(x = xstart, xend = xend, y = 100*beta, yend = 100*beta),
               linetype = "dashed", color = "black", linewidth = 0.8) +
  # Formatting to match original
  scale_y_continuous("Cents", 
                     limits = c(2.5, 3.5), 
                     breaks = seq(2.5, 3.5, 0.2),
                     expand = c(0, 0)) +
  scale_x_date(expand = c(0, 0), 
               limits = c(as.Date("1988-01-01"), as.Date("2022-01-01"))) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth", 
       x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  ) +
  # Add legend manually
  annotate("text", x = as.Date("2010-01-01"), y = 2.6, 
           label = "Model with Trend Break", size = 3) +
  annotate("text", x = as.Date("2010-01-01"), y = 2.55, 
           label = "Rolling Window", size = 3, color = "#FF6B6B")

print(fig2)

# Print summary statistics
cat("\nSummary of Rolling MPC Estimates:\n")
cat("1990s-early 2000s average:", round(mean(roll$beta_cents[roll$Date < as.Date("2005-01-01")], na.rm = TRUE), 2), "cents\n")
cat("2016+ average:", round(mean(roll$beta_cents[roll$Date >= as.Date("2016-01-01")], na.rm = TRUE), 2), "cents\n")
cat("Pre-trend break (", format(as.Date(BREAK_QTR), "%Y"), "):", round(100 * beta_pre, 2), "cents\n")
cat("Post-trend break:", round(100 * beta_post, 2), "cents\n")
