# ================================================================
# FED_rep.xlsx (Sheet29) -> Figure 1 (Full vs No-COVID) + Figure 2 (Rolling MPC w/ CI)
# Y & C already real; deflate only Gov benefits and Ill wealth by Cons Deflator
# Faceted Figure 1 (side-by-side) without extra packages; robust CI ribbon in Figure 2
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(zoo)        # as.yearqtr
  library(sandwich)   # NeweyWest(), vcov()
  library(lmtest)     # (not strictly needed here, but handy)
})

# ---------------- CONFIG ----------------
XLSX_FILE   <- "FED_rep.xlsx"
SHEET       <- "Sheet29"
BREAK_QTR   <- zoo::as.yearqtr("2012 Q1")
COVID_START <- zoo::as.yearqtr("2020 Q1")
COVID_END   <- zoo::as.yearqtr("2021 Q4")

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

# ---------------- Figure 1 (faceted: Full vs Excluding COVID) ----------------
# Helper to fit Eq(1) & attach predictions
fit_eq1 <- function(df) {
  ols1 <- lm(ratio_data ~ W_over_den, data = df)
  df$ratio_pred <- as.numeric(predict(ols1, newdata = df))
  df <- df %>%
    mutate(post = as.integer(date >= BREAK_QTR),
           post_W = post * W_over_den))
  ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = df)
  df$ratio_pred_break <- as.numeric(predict(ols2, newdata = df))
  df
}

d_full  <- fit_eq1(d) %>% mutate(Sample = "Full sample")
d_nc    <- d %>% filter(date < COVID_START | date > COVID_END) %>% fit_eq1() %>% mutate(Sample = "Excluding COVID (2020Q1–2021Q4)")

# Stack and pivot to long for faceting
fig1_df <- bind_rows(d_full, d_nc) %>%
  select(Sample, Date, ratio_data, ratio_pred, ratio_pred_break) %>%
  pivot_longer(cols = c(ratio_data, ratio_pred, ratio_pred_break),
               names_to = "Series", values_to = "Value") %>%
  mutate(Series = recode(Series,
                         ratio_data = "Data",
                         ratio_pred = "Predicted",
                         ratio_pred_break = "Predicted with Trend Break"),
         Series = factor(Series, levels = c("Data","Predicted","Predicted with Trend Break")),
         Sample = factor(Sample, levels = c("Full sample","Excluding COVID (2020Q1–2021Q4)")))

# Common y-limits across facets for comparability
ylims <- range(fig1_df$Value, na.rm = TRUE)

fig1 <- ggplot(fig1_df, aes(x = Date, y = Value, colour = Series, linetype = Series)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Sample, ncol = 2, scales = "fixed") +
  scale_colour_manual(values = c("Data"="black", "Predicted"="#2C7FB8", "Predicted with Trend Break"="#D95F02")) +
  scale_linetype_manual(values = c("Data"="solid", "Predicted"="dotted", "Predicted with Trend Break"="dashed")) +
  coord_cartesian(ylim = ylims) +
  labs(title = "Figure 1. The consumption-to-income ratio",
       x = NULL, y = "Ratio", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

print(fig1)

# ---------------- Figure 2: Rolling 40-quarter MPC with CI -------------------
# Pre/post MPC levels from full-sample break fit
bhat <- coef(lm(ratio_data ~ W_over_den + post + post * W_over_den, data = d_full))
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post:W_over_den"])  # interaction term name in base R uses ":" 

seg_df <- tibble::tibble(
  xstart = c(min(d$date), BREAK_QTR),
  xend   = c(BREAK_QTR - 0.25, max(d$date)),
  beta   = c(beta_pre, beta_post)
)

# Rolling 40q (centered) with Newey–West SEs; fallback to OLS SEs if needed
roll_window <- 40L
n <- nrow(d)
if (n < roll_window) stop("Need at least 40 quarters for rolling window; found: ", n)

rows <- list()
for (s in 1:(n - roll_window + 1)) {
  e <- s + roll_window - 1L
  center <- s + 19L  # centered index for 40q (20 before, 19 after)
  sub <- d[s:e, , drop = FALSE]

  # Require some variation in regressor
  if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
      var(sub$W_over_den, na.rm = TRUE) <= 0) {
    rows[[length(rows)+1]] <- tibble::tibble(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  fit <- try(lm(ratio_data ~ W_over_den, data = sub), silent = TRUE)
  if (inherits(fit, "try-error") || is.na(coef(fit)["W_over_den"])) {
    rows[[length(rows)+1]] <- tibble::tibble(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  b <- as.numeric(coef(fit)["W_over_den"])
  # Newey–West covariance (1 year of lags); fallback to classical if NW misbehaves
  V <- try(sandwich::NeweyWest(fit, lag = 4, prewhite = FALSE, adjust = TRUE), silent = TRUE)
  if (inherits(V, "try-error") || !is.matrix(V) || any(!is.finite(diag(V)))) V <- vcov(fit)
  se <- suppressWarnings(sqrt(V["W_over_den","W_over_den"]))
  if (!is.finite(se)) se <- NA_real_

  rows[[length(rows)+1]] <- tibble::tibble(Date = d$Date[center], beta = b, se = se)
}

roll <- bind_rows(rows) %>%
  mutate(beta_cents = 100 * beta,
         lo = 100 * (beta - 1.96 * se),
         hi = 100 * (beta + 1.96 * se)) %>%
  arrange(Date)

# Build datasets: line uses finite betas; ribbon uses finite lo/hi
roll_line <- roll %>% filter(is.finite(beta_cents))
roll_band <- roll %>% filter(is.finite(lo), is.finite(hi))

# Plot (CI ribbon added only when valid)
fig2 <- ggplot() +
  { if (nrow(roll_band) > 0)
      geom_ribbon(data = roll_band,
                  aes(x = Date, ymin = lo, ymax = hi),
                  inherit.aes = FALSE,
                  fill = "#FB9A99", alpha = 0.35) } +
  geom_line(data = roll_line, aes(x = Date, y = beta_cents),
            color = "#E31A1C", linewidth = 1.0, na.rm = TRUE) +
  geom_segment(data = seg_df,
               aes(x = as.Date(xstart), xend = as.Date(xend),
                   y = 100*beta, yend = 100*beta),
               inherit.aes = FALSE, linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, 0.2)) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth (Rolling 10-year MPC)",
       x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

print(fig2)
# ================================================================
