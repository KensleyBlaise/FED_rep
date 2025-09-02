# ================================================================
# FED_rep.xlsx (Sheet29): Fig 1 (full vs no-COVID) + Fig 2 (rolling with CI)
# Y & C already real; deflate only Gov benefits and Ill wealth
# No gridlines. Confidence band is robust (NW with fallback).
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)        # as.yearqtr
  library(sandwich)   # NeweyWest, vcov
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

# ---------------- Figure 1: full sample & no-COVID side-by-side ---------------
full   <- build_fig1_data(d)
d_full <- full$df

d_nocovid <- d %>% filter(date < COVID_START | date > COVID_END)
nocovid <- build_fig1_data(d_nocovid)
d_nc <- nocovid$df

ylims <- range(
  d_full$ratio_data, d_full$ratio_pred, d_full$ratio_pred_break,
  d_nc$ratio_data,   d_nc$ratio_pred,   d_nc$ratio_pred_break,
  na.rm = TRUE
)

par(mfrow = c(1, 2))
print(make_fig1_plot(d_full, "Full Sample", ylims))
print(make_fig1_plot(d_nc,   "Excluding COVID (2020Q1–2021Q4)", ylims))
par(mfrow = c(1, 1))  # reset

# ---------------- Figure 2: Rolling 40q regression with CI ---------------------
# Pre/post MPC from full-sample break model
bhat <- coef(full$ols2)
beta_pre  <- unname(bhat["W_over_den"])
beta_post <- beta_pre + unname(bhat["post_W"])

seg_df <- tibble::tibble(
  xstart = c(min(d$date), BREAK_QTR),
  xend   = c(BREAK_QTR - 0.25, max(d$date)),
  beta   = c(beta_pre, beta_post)
)

# Rolling 40-quarter window (centered) using a simple loop (no extra deps)
roll_window <- 40L
n <- nrow(d)
starts <- 1:(n - roll_window + 1)
centers <- starts + (roll_window %/% 2)

roll_list <- lapply(seq_along(starts), function(k) {
  s <- starts[k]; e <- s + roll_window - 1L
  .x <- d[s:e, , drop = FALSE]
  # Guard against singular fits
  fit <- try(lm(ratio_data ~ W_over_den, data = .x), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(tibble::tibble(Date = d$Date[centers[k]], beta = NA_real_, se = NA_real_))
  }
  b <- suppressWarnings(as.numeric(coef(fit)["W_over_den"]))
  # Newey–West variance; fallback to classical if needed
  V <- try(sandwich::NeweyWest(fit, lag = 4, prewhite = FALSE, adjust = TRUE), silent = TRUE)
  if (inherits(V, "try-error") || !is.matrix(V) || any(!is.finite(diag(V)))) {
    V <- vcov(fit)
  }
  se <- suppressWarnings(sqrt(V["W_over_den", "W_over_den"]))
  if (!is.finite(b))  b  <- NA_real_
  if (!is.finite(se)) se <- NA_real_
  tibble::tibble(Date = d$Date[centers[k]], beta = b, se = se)
})

roll <- dplyr::bind_rows(roll_list) %>%
  mutate(beta_cents = 100 * beta,
         lo = 100 * (beta - 1.96 * se),
         hi = 100 * (beta + 1.96 * se)) %>%
  arrange(Date)

# Build line and band datasets
roll_line <- roll %>% filter(is.finite(beta_cents))
roll_band <- roll %>% filter(is.finite(lo), is.finite(hi))

# Plot (band added only if valid rows exist)
fig2 <- ggplot() +
  geom_line(data = roll_line, aes(x = Date, y = beta_cents),
            color = "#E31A1C", linewidth = 1.0, na.rm = TRUE) +
  geom_segment(data = seg_df,
               aes(x = as.Date(xstart), xend = as.Date(xend),
                   y = 100*beta, yend = 100*beta),
               inherit.aes = FALSE,
               linetype = "dashed", color = "black", linewidth = 0.9) +
  scale_y_continuous("Cents", limits = c(2.5, 3.5), breaks = seq(2.5, 3.5, 0.2)) +
  labs(title = "Figure 2. Rolling 10-year MPC (NW 95% CI)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

if (nrow(roll_band) > 0) {
  fig2 <- fig2 +
    geom_ribbon(data = roll_band,
                aes(x = Date, ymin = lo, ymax = hi),
                inherit.aes = FALSE,
                fill = "#FB9A99", alpha = 0.35, na.rm = TRUE)
} else {
  warning("CI band skipped: no valid lo/hi rows after filtering. Line is shown.")
}

print(fig2)
# ================================================================
