# ================================================================
# FED_rep.xlsx — Sheet29 -> Figures 1 & 2 (no gridlines)
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(zoo)
  library(slider)
  library(sandwich)
  library(lmtest)
})

# ---------------- CONFIG ----------------
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
BREAK_QTR <- zoo::as.yearqtr("2012 Q1")

# Your column names exactly as in the file
COL_Q <- "Q"
COL_Y <- "Real income"
COL_P <- "Cons Deflator"
COL_C <- "Real Cons"
COL_T <- "Gov benefits"
COL_W <- "Ill wealth"

# ---------------- LOAD -----------------
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Build tidy frame
d <- raw |>
  transmute(
    Q = .data[[COL_Q]],
    Y = as.numeric(.data[[COL_Y]]),
    P = as.numeric(.data[[COL_P]]),
    C = as.numeric(.data[[COL_C]]),
    T = as.numeric(.data[[COL_T]]),
    W = as.numeric(.data[[COL_W]])
  )

# Parse quarters "Mar-00" (or "Mar-2000")
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Rescale deflator if needed (e.g., 85 → 0.85)
if (mean(d$P, na.rm = TRUE) > 5) {
  message("Rescaling deflator by /100 (100=base scale detected).")
  d$P <- d$P / 100
}

# ---------------- REAL TERMS ------------
to_real <- function(nom, p) nom / p
d <- d |>
  mutate(
    Y_real = to_real(Y, P),
    C_real = to_real(C, P),
    T_real = to_real(T, P),
    W_real = to_real(W, P),
    den = Y_real - T_real,
    num = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) |>
  filter(is.finite(ratio_data), is.finite(W_over_den)) |>
  arrange(Date)

# ---------------- FIGURE 1 --------------
ols1 <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- predict(ols1, newdata = d)

d <- d |>
  mutate(post = as.integer(date >= BREAK_QTR),
         post_W = post * W_over_den)
ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = d)
d$ratio_pred_break <- predict(ols2, newdata = d)

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
  theme(panel.grid = element_blank())

print(fig1)

# ---------------- FIGURE 2 --------------
bhat <- coef(ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

seg_df <- tibble(
  xstart = c(min(d$date), BREAK_QTR),
  xend   = c(BREAK_QTR - 0.25, max(d$date)),
  beta   = c(beta_pre, beta_post)
)

roll <- slide_index_dfr(
  .x = d, .i = d$date,
  .before = 20, .after = 19,
  .f = ~{
    if (nrow(.x) < 40)
      return(tibble(date = .x$date[ceiling(nrow(.x)/2)], beta = NA_real_, se = NA_real_))
    m  <- lm(ratio_data ~ W_over_den, data = .x)
    ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type = "HC1"))
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
                   y = 100*beta, yend = 100*beta),
               linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = c(2.5, 3.5),
                     breaks = seq(2.5, 3.5, 0.2)) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

print(fig2)
