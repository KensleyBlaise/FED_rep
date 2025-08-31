# ================================================================
# Figures 1 & 2 — from quarterly CSV data (no gridlines)
# ================================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(zoo)        # as.yearqtr
  library(lubridate)
  library(slider)
  library(sandwich)
  library(lmtest)
})

# ------------------------ CONFIG ------------------------
XLSX_FILE <- "FED_rep.xlsx"   # your file
SHEET     <- NULL             # NULL = first sheet
DATE_COL  <- "Q"              # quarter column like "Mar-00"
BREAK_QTR <- zoo::as.yearqtr("2012 Q1")

# If auto-detect is off, set your exact column names here:
COL_C  <- "Real Cons"      # consumption (nominal)
COL_Y  <- "Real income"    # income (nominal)
COL_T  <- "Gov benefits"   # government transfers (nominal)
COL_W  <- "Ill wealth"     # wealth (nominal)
COL_P  <- "Cons Deflator"  # deflator (index ~ 0.6–0.8 or 60–80)

# ------------------------ LOAD -------------------------
raw <- read_excel(XLSX_FILE, sheet = SHEET)

# Tidy headers: trim accidental spaces; show them for sanity
names(raw) <- trimws(names(raw))
print(names(raw))

# Auto-rescale deflator if it looks like 60..150 instead of 0.60..1.50
if (!COL_P %in% names(raw)) stop("Can't find column: ", COL_P)
if (mean(as.numeric(raw[[COL_P]]), na.rm = TRUE) > 5) {
  message("Rescaling deflator by /100 (looks like 100=base scale).")
  raw[[COL_P]] <- as.numeric(raw[[COL_P]]) / 100
}

# Check required columns exist
need <- c(DATE_COL, COL_Y, COL_P, COL_C, COL_T, COL_W)
miss <- setdiff(need, names(raw))
if (length(miss)) stop("Missing columns in Excel: ", paste(miss, collapse = ", "))

# ------------------- PREPARE & DEFLATE -----------------
d <- raw |>
  transmute(
    Q        = .data[[DATE_COL]],
    Y_nom    = as.numeric(.data[[COL_Y]]),
    P        = as.numeric(.data[[COL_P]]),
    C_nom    = as.numeric(.data[[COL_C]]),
    T_nom    = as.numeric(.data[[COL_T]]),
    W_nom    = as.numeric(.data[[COL_W]])
  )

# Parse quarters like "Mar-00" (use "%b-%Y" if your sheet has "Mar-2000")
d$date <- zoo::as.yearqtr(d$Q, format = "%b-%y")
d$Date <- as.Date(d$date)

# Convert to real terms
to_real <- function(nom, p) nom / p
d <- d |>
  mutate(
    Y_real = to_real(Y_nom, P),
    C_real = to_real(C_nom, P),
    T_real = to_real(T_nom, P),
    W_real = to_real(W_nom, P)
  )

# Build variables for Equation (2)
d <- d |>
  mutate(
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) |>
  filter(is.finite(ratio_data), is.finite(W_over_den)) |>
  arrange(Date)

# ----------------------- FIGURE 1 -----------------------
# Baseline regression
ols1 <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- as.numeric(predict(ols1, newdata = d))

# Trend-break regression (intercept + slope shift after BREAK_QTR)
d <- d |>
  mutate(post = as.integer(date >= BREAK_QTR),
         post_W = post * W_over_den)
ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = d)
d$ratio_pred_break <- as.numeric(predict(ols2, newdata = d))

fig1 <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Data"), linewidth = 1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted"), linewidth = 1, linetype = "dotted") +
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

# ----------------------- FIGURE 2 -----------------------
# Implied MPC (β) from break model: pre vs post
bhat <- coef(ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

seg_df <- tibble(
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
      return(tibble(date = .x$date[ceiling(nrow(.x)/2)], beta = NA_real_, se = NA_real_))
    m  <- lm(ratio_data ~ W_over_den, data = .x)
    ct <- coeftest(m, vcov = sandwich::vcovHC(m, type = "HC1"))
    tibble(date = .x$date[ceiling(nrow(.x)/2)],
           beta = ct["W_over_den","Estimate"],
           se   = ct["W_over_den","Std. Error"])
  }
)

roll <- roll |>
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
                   y = 100 * beta, yend = 100 * beta),
               linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, 0.2)) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())   # no gridlines

print(fig2)

# ------------------- (optional) quick checks ---------------
# print(summary(ols1)); print(summary(ols2))
# head(d[, c("Date","ratio_data","ratio_pred","ratio_pred_break")])
