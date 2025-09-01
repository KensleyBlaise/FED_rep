# ================================================================
# FED_rep.xlsx (Sheet29) -> Figures 1 & 2
# Y and C already real; T and W deflated by Cons Deflator
# Display only, no gridlines
# ================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)        # as.yearqtr
  library(slider)     # rolling windows
  library(sandwich)   # robust vcov
  library(lmtest)     # coeftest
})

# ---------------- CONFIG ----------------
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
BREAK_QTR <- zoo::as.yearqtr("2012 Q1")

# Exact column names in your workbook
COL_Q <- "Q"              # quarter like "Mar-00"
COL_Y <- "Real income"    # already real
COL_C <- "Real Cons"      # already real
COL_T <- "Gov benefits"   # NOMINAL -> will deflate
COL_W <- "Ill wealth"     # NOMINAL -> will deflate
COL_P <- "Cons Deflator"  # deflator

# ---------------- LOAD -----------------
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Build tidy frame
d <- raw %>%
  transmute(
    Q        = .data[[COL_Q]],
    Y_real   = as.numeric(.data[[COL_Y]]),   # already real
    C_real   = as.numeric(.data[[COL_C]]),   # already real
    T_nom    = as.numeric(.data[[COL_T]]),   # nominal transfers
    W_nom    = as.numeric(.data[[COL_W]]),   # nominal wealth
    P        = as.numeric(.data[[COL_P]])    # deflator
  )

# Parse quarters "Mar-00" (fallback to "Mar-2000")
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Normalize deflator if it's on a 100=base scale (e.g., 95, 105)
if (mean(d$P, na.rm = TRUE) > 5) {
  message("Rescaling deflator by /100 (100=base scale detected).")
  d$P <- d$P / 100
}

# ---------------- MAKE T & W REAL ----------------
to_real <- function(nom, p) nom / p
d <- d %>%
  mutate(
    T_real = to_real(T_nom, P),
    W_real = to_real(W_nom, P),

    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(W_over_den)) %>%
  arrange(Date)

# ---------------- FIGURE 1 ----------------
# Baseline OLS
ols1 <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- as.numeric(predict(ols1, newdata = d))

# Trend break (intercept + slope shift after 2012Q1)
d <- d %>%
  mutate(post = as.integer(date >= BREAK_QTR),
         post_W = post * W_over_den)
ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = d)
d$ratio_pred_break <- as.numeric(predict(ols2, newdata = d))

fig1 <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Data"), linewidth = 1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted"),
            linewidth = 1, linetype = "dotted") +
  geom_line(aes(y = ratio_pred_break, colour = "Predicted with Trend Break"),
            linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c("Data" = "black",
                                 "Predicted" = "#2C7FB8",
                                 "Predicted with Trend Break" = "#D95F02")) +
  labs(title = "Figure 1. The consumption-to-income ratio",
       y = "Ratio", x = NULL, colour = "") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())   # no gridlines

print(fig1)

# ---------------- FIGURE 2 ----------------
# Implied MPC (β) from break model: pre vs post
bhat <- coef(ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

seg_df <- tibble::tibble(
  xstart = c(min(d$date), BREAK_QTR),
  xend   = c(BREAK_QTR - 0.25, max(d$date)),
  beta   = c(beta_pre, beta_post)
)

# Rolling 10-year (40 quarters) centered OLS with HC1 SEs
roll <- slide_index_dfr(
  .x = d, .i = d$date,
  .before = 20, .after = 19,
  .f = ~{
    if (nrow(.x) < 40)
      return(tibble::tibble(date = .x$date[ceiling(nrow(.x)/2)],
                            beta = NA_real_, se = NA_real_))
    m  <- lm(ratio_data ~ W_over_den, data = .x)
    ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC1"))
    tibble::tibble(date = .x$date[ceiling(nrow(.x)/2)],
                   beta = ct["W_over_den","Estimate"],
                   se   = ct["W_over_den","Std. Error"])
  }
)

roll <- roll %>%
  mutate(beta_cents = 100 * beta,
         lo = 100 * (beta - 1.96 * se),
         hi = 100 * (beta + 1.96 * se),
         Date = as.Date(date))

fig2 <- ggplot() +
  geom_ribbon(data = roll, aes(x = Date, ymin = lo, ymax = hi),
              fill = "#FB9A99", alpha = 0.35) +
  geom_line(data = roll, aes(x = Date, y = beta_cents),
            color = "#E31A1C", linewidth = 1.0) +
  geom_segment(data = seg_df,
               aes(x = as.Date(xstart), xend = as.Date(xend),
                   y = 100*beta, yend = 100*beta),
               linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, 0.2)) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())   # no gridlines

print(fig2)
