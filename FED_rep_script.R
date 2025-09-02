# ================================================================
# FED_rep.xlsx (Sheet29) -> Figure 1 (Full vs No-COVID) + Figure 2 (Rolling MPC w/ CI)
# Y & C already real; deflate only Gov benefits and Ill wealth by Cons Deflator
# Adds panel borders and "nice" axis bounds; COVID panel starts at 0.8
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)        # as.yearqtr
  library(sandwich)   # NeweyWest(), vcov()
  library(lmtest)
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

# ---------- helper: "nice" y-limits with small padding ----------
nice_limits <- function(vals, lower = NULL, upper = NULL, pad_mult = 0.04, digits = 2) {
  vals <- vals[is.finite(vals)]
  r <- range(vals, na.rm = TRUE)
  span <- max(r[2] - r[1], 1e-9)
  pad  <- pad_mult * span
  lo <- r[1] - pad
  hi <- r[2] + pad
  if (!is.null(lower)) lo <- max(lower, lo)
  if (!is.null(upper)) hi <- min(upper, hi)
  lo <- floor(lo * 10^digits) / 10^digits
  hi <- ceiling(hi * 10^digits) / 10^digits
  c(lo, hi)
}

# ---------------- Helpers for Figure 1 ----------------
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

# ---------------- Figure 2: Rolling 40-quarter MPC with CI -------------------
# Pre/post MPC levels from full-sample break fit
bhat <- coef(full$ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

seg_df <- data.frame(
  xstart = as.Date(c(min(d$Date), as.Date(as.yearqtr(BREAK_QTR)))),
  xend   = as.Date(c(as.Date(as.yearqtr(BREAK_QTR)) - 90, max(d$Date))),  # ~1 quarter before
  beta   = c(beta_pre, beta_post)
)

# Rolling 40q (centered) with Newey–West SEs; fallback to OLS SEs if needed
roll_window <- 40L
n <- nrow(d)
if (n < roll_window) stop("Need at least 40 quarters for rolling window; found: ", n)

rows <- vector("list", length = n - roll_window + 1L)
for (s in 1:(n - roll_window + 1L)) {
  e <- s + roll_window - 1L
  center <- s + 19L           # centered index for 40q (20 before, 19 after)
  sub <- d[s:e, , drop = FALSE]

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

  b <- as.numeric(coef(fit)["W_over_den"])
  # Newey–West covariance (1-year lag); fallback to classical
  V <- try(sandwich::NeweyWest(fit, lag = 4, prewhite = FALSE, adjust = TRUE), silent = TRUE)
  if (inherits(V, "try-error") || !is.matrix(V) || any(!is.finite(diag(V)))) V <- vcov(fit)
  se <- suppressWarnings(sqrt(V["W_over_den","W_over_den"]))
  if (!is.finite(se)) se <- NA_real_

  rows[[s]] <- data.frame(Date = d$Date[center], beta = b, se = se)
}

roll <- do.call(rbind, rows)
roll$beta_cents <- 100 * roll$beta
roll$lo <- 100 * (roll$beta - 1.96 * roll$se)
roll$hi <- 100 * (roll$beta + 1.96 * roll$se)

roll_line <- subset(roll, is.finite(beta_cents))
roll_band <- subset(roll, is.finite(lo) & is.finite(hi))

# "Nice" y-limits for rolling chart (keep your canonical 2.5–3.5 if you prefer)
ylims_roll <- nice_limits(c(roll_line$beta_cents, roll_band$lo, roll_band$hi), lower = 2.5, upper = 3.5)

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
               inherit.aes = FALSE,
               linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = ylims_roll, expand = expansion(mult = c(0,0)),
                     breaks = seq(floor(ylims_roll[1]*10)/10, ceiling(ylims_roll[2]*10)/10, by = 0.2)) +
  labs(title = "Figure 2. The Propensity to Consume out of Wealth (Rolling 10-year MPC)",
       x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.background = element_blank()
  )

print(fig2)
# ================================================================
