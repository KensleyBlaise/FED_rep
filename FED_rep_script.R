# ================== Figure 2 — Rolling MPC (clean + robust) ==================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# ---- Config ----
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
BREAK_QTR <- as.yearqtr("2012 Q1")
ROLL_WIN  <- 40L   # 10-year centered window (set 32L for 8y, etc.)

# Your column names (exactly as in the sheet)
COL_Q <- "Q"              # e.g., "Mar-00"
COL_Y <- "Real income"    # already real
COL_C <- "Real Cons"      # already real
COL_T <- "Gov benefits"   # nominal -> deflate
COL_W <- "Ill wealth"     # nominal -> deflate
COL_P <- "Cons Deflator"  # deflator

# ---- Load & prepare ----
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
d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Deflator: if it's on a 100=base scale (e.g., 95, 105), scale to 0.xx
if (mean(d$P, na.rm = TRUE) > 5) d$P <- d$P / 100

# Deflate ONLY transfers & illiquid wealth; build regression variables
d <- d %>%
  mutate(
    T_real     = T_nom / P,
    W_real     = W_nom / P,
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(W_over_den)) %>%
  arrange(Date)

stopifnot(nrow(d) >= ROLL_WIN)

# ---- Break model to get pre/post MPC (in ratio units; we'll x100 for "cents") ----
dfb <- d %>% mutate(post = as.integer(date >= BREAK_QTR),
                    post_W = post * W_over_den)
m_break  <- lm(ratio_data ~ W_over_den + post + post_W, data = dfb)
co       <- coef(m_break)
beta_pre <- unname(co["W_over_den"])
beta_post <- beta_pre + unname(co["post_W"])

# ---- Rolling OLS (centered) — store beta and OLS SE for each window ----
n <- nrow(d)
rows <- vector("list", n - ROLL_WIN + 1L)
for (s in 1:(n - ROLL_WIN + 1L)) {
  e <- s + ROLL_WIN - 1L
  center <- s + (ROLL_WIN/2 - 1L)            # for 40q: s+19
  sub <- d[s:e, , drop = FALSE]

  # Skip if no variation or NA
  if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
      var(sub$W_over_den, na.rm = TRUE) <= 0) {
    rows[[s]] <- data.frame(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  fit <- lm(ratio_data ~ W_over_den, data = sub)
  b   <- as.numeric(coef(fit)["W_over_den"])
  se  <- summary(fit)$coef["W_over_den", "Std. Error"]

  rows[[s]] <- data.frame(Date = d$Date[center],
                          beta = ifelse(is.finite(b), b, NA_real_),
                          se   = ifelse(is.finite(se), se, NA_real_))
}
roll <- do.call(rbind, rows) %>%
  mutate(beta_cents = 100 * beta,
         lo = 100 * (beta - 1.96 * se),
         hi = 100 * (beta + 1.96 * se)) %>%
  arrange(Date)

roll_line <- dplyr::filter(roll, is.finite(beta_cents))
roll_band <- dplyr::filter(roll, is.finite(lo) & is.finite(hi))

# ---- Dynamic y-limits from the data so nothing is clipped ----
y_pre  <- 100 * beta_pre
y_post <- 100 * beta_post
vals   <- c(roll_line$beta_cents, roll_band$lo, roll_band$hi, y_pre, y_post)
vals   <- vals[is.finite(vals)]
ylims  <- range(vals)
pad    <- 0.05 * diff(ylims)
if (!is.finite(pad)) pad <- 0.1
ylims  <- c(ylims[1] - pad, ylims[2] + pad)

# ---- Build the bracket (pre/post levels), clipped to y-limits ----
x_min   <- min(d$Date); x_max <- max(d$Date)
x_break <- as.Date(as.yearqtr(BREAK_QTR))
pad_days <- 15L

hseg <- data.frame(
  xstart = c(x_min,            x_break + pad_days),
  xend   = c(x_break - pad_days, x_max),
  y      = c(y_pre, y_post)
)
hseg <- subset(hseg, is.finite(y) & y >= ylims[1] & y <= ylims[2])

vseg <- NULL
if (is.finite(y_pre) && is.finite(y_post)) {
  v0 <- max(min(y_pre, y_post), ylims[1])
  v1 <- min(max(y_pre, y_post), ylims[2])
  if (v1 > v0) vseg <- data.frame(x = x_break, y0 = v0, y1 = v1)
}

# ---- Plot (no recession shading, no secondary axis) ----
p <- ggplot() +
  { if (nrow(roll_band) > 0)
      geom_ribbon(data = roll_band,
                  aes(x = Date, ymin = lo, ymax = hi),
                  fill = "#FB9A99", alpha = 0.35) } +
  geom_line(data = roll_line,
            aes(x = Date, y = beta_cents),
            colour = "#E31A1C", linewidth = 1.0, na.rm = TRUE) +
  { if (nrow(hseg) > 0)
      geom_segment(data = hseg,
                   aes(x = xstart, xend = xend, y = y, yend = y),
                   linetype = "dotted", colour = "black", linewidth = 0.9) } +
  { if (!is.null(vseg))
      geom_segment(data = vseg,
                   aes(x = x, xend = x, y = y0, yend = y1),
                   linetype = "dotted", colour = "black", linewidth = 0.9) } +
  scale_y_continuous("Cents",
                     limits = ylims,
                     breaks = pretty(ylims, n = 6),
                     expand = expansion(mult = c(0,0))) +
  labs(title = "Figure 2. Rolling 10-year MPC (OLS 95% CI)", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.background = element_blank())

print(p)
# ======================================================================

















































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





















# ===================== UNDERLYING ANALYSIS — FACTOR SPLIT (SEPARATE BLOCK) =====================
# Assumes you already have: high_freq_ts, mgdp_ts, gdpts, time_vector, STW,
# QQ (quantiles), Q (length(QQ)), lags_xhf, decay, dates, time_length, results_filepath
# And helpers: mix_data(), u_midas_q_pred(), crps(), quant_comb_TV_w(),
#              format_quantile_df(), format_weights_df()

# Guard (in case Q wasn't defined earlier)
if (!exists("QQ") || is.null(QQ)) QQ <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
if (!exists("Q")  || is.null(Q))  Q  <- length(QQ)

# Config: variable blocks
P_vars <- c('housing','labour','output','futureoutput','neworders')   # persistent (UNDERLYING)
T_vars <- c('risk','uncertainty','retail')                            # transitory

# Standardise each column one-sided (no look-ahead)
scale_one_sided <- function(zm) {
  m <- apply(zm, 2, function(x) mean(x, na.rm = TRUE))
  s <- apply(zm, 2, function(x) sd(x,   na.rm = TRUE))
  s[s == 0 | is.na(s)] <- 1
  sweep(sweep(zm, 2, m, "-"), 2, s, "/")
}

# Storage sized like your outputs (same monthly horizon)
y_u_P2      <- array(NA, dim = c(time_length, Q))     # UNDERLYING (persistent factor) nowcasts
y_u_T2      <- array(NA, dim = c(time_length, Q))     # Transitory factor nowcasts
qsc_u_P2    <- array(NA, dim = c(time_length, Q))     # CRPS for TV weights
qsc_u_T2    <- array(NA, dim = c(time_length, Q))
kcomb_PT_w2 <- array(NA, dim = c(2, Q, time_length))  # weights: [P,T] × τ × t
kcomb_PT_y2 <- array(NA, dim = c(time_length, Q))     # factor-based headline (P+T)

# Build factors & run quantile MIDAS (mirrors your timing & NA handling)
for (f2 in STW:length(time_vector[,1])) {

  out_row <- f2 - STW + 1
  if (out_row > time_length) break

  # --- Quarterly GDP up to correct quarter end (no look-ahead) ---
  time_q <- c(as.numeric(time_vector[f2,1]), (as.numeric(time_vector[f2,2]) %/% 3))
  if (time_q[2]==0){
    gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f2,1])-1, 4))
    gdpts_clean <- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f2,1])-1, 4), frequency=4)
  } else if (time_q[2]==1){
    gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f2,1]), 1))
    gdpts_clean <- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f2,1]), 1), frequency=4)
  } else if (time_q[2]==2){
    gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f2,1]), 2))
    gdpts_clean <- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f2,1]), 2), frequency=4)
  } else {
    gdpts_n <- window(gdpts, start=start(gdpts), end=c(as.numeric(time_vector[f2,1]), 3))
    gdpts_clean <- ts(gdpts_n[!is.na(gdpts_n)], end=c(as.numeric(time_vector[f2,1]), 3), frequency=4)
  }

  # --- Monthly panels up to previous month (f2-1); COVID NAs already present ---
  hf_end   <- time_vector[f2-1, ]
  hf_full  <- window(high_freq_ts, start=start(high_freq_ts), end=hf_end)
  mgdp_fac <- window(mgdp_ts,      start=start(mgdp_ts),      end=hf_end)

  # Keep needed columns & align with mgdp; drop rows with any NA (COVID etc.)
  hf_keep <- colnames(hf_full)[colnames(hf_full) %in% c(P_vars, T_vars)]
  if (length(hf_keep) == 0) next
  hf_z   <- zoo::as.zoo(hf_full[, hf_keep, drop=FALSE])
  mgdp_z <- zoo::as.zoo(mgdp_fac); colnames(mgdp_z) <- "mgdp"
  mpanel <- na.omit(merge(hf_z, mgdp_z, all = FALSE))
  if (NROW(mpanel) < 12) next  # need ≥ 12 months

  # Standardise; drop constant cols
  mp_std <- scale_one_sided(zoo::coredata(mpanel))
  rownames(mp_std) <- zoo::index(mpanel)
  keep_cols <- apply(mp_std, 2, function(x) sd(x, na.rm=TRUE) > 0)
  mp_std <- mp_std[, keep_cols, drop=FALSE]
  if (ncol(mp_std) == 0) next

  # Define blocks (persistent includes mgdp)
  P_cols <- intersect(colnames(mp_std), c(P_vars, "mgdp"))
  T_cols <- intersect(colnames(mp_std), T_vars)
  if (length(P_cols) < 2 || length(T_cols) < 1) next

  # PCA factors (PC1 per block)
  pP <- prcomp(mp_std[, P_cols, drop=FALSE], center=FALSE, scale.=FALSE)
  pT <- prcomp(mp_std[, T_cols, drop=FALSE], center=FALSE, scale.=FALSE)
  F_P_scores <- pP$x[,1]
  F_T_scores <- pT$x[,1]

  # Align persistent factor so higher = stronger momentum (positive vs 'output' or 'mgdp')
  align_to <- if ("output" %in% P_cols) mp_std[, "output"] else mp_std[, "mgdp"]
  sgn <- suppressWarnings(sign(cor(F_P_scores, align_to, use="pairwise.complete.obs")))
  if (!is.na(sgn) && sgn < 0) F_P_scores <- -F_P_scores

  # Convert to monthly ts and trim to hf_end (no look-ahead)
  first_idx <- as.Date(rownames(mp_std)[1])
  fy <- as.numeric(format(first_idx, "%Y")); fm <- as.numeric(format(first_idx, "%m"))
  F_P_ts <- ts(as.numeric(F_P_scores), start=c(fy,fm), frequency=12)
  F_T_ts <- ts(as.numeric(F_T_scores), start=c(fy,fm), frequency=12)
  F_P_ts <- window(F_P_ts, end=hf_end)
  F_T_ts <- window(F_T_ts, end=hf_end)

  # Build MIDAS datasets and run quantile-MIDAS for each factor
  mix_P <- mix_data(F_P_ts, gdpts_clean, time_vector[f2,])
  mix_T <- mix_data(F_T_ts, gdpts_clean, time_vector[f2,])

  Umidas_P <- u_midas_q_pred(mix_P$y_p, mix_P$x_p, lags_xhf, QQ)
  Umidas_T <- u_midas_q_pred(mix_T$y_p, mix_T$x_p, lags_xhf, QQ)

  # Store latest nowcasts (per τ)
  y_u_P2[out_row, ] <- tail(Umidas_P$y_hat, 1)   # UNDERLYING nowcast (persistent)
  y_u_T2[out_row, ] <- tail(Umidas_T$y_hat, 1)   # Transitory

  # CRPS for TV weights (same evaluation timing as your pipeline)
  if ((out_row - 2) >= 1) {
    y_u_P_ev <- y_u_P2[out_row - 2, ]
    y_u_T_ev <- y_u_T2[out_row - 2, ]
  } else {
    y_u_P_ev <- tail(Umidas_P$y_hat, 4)[1]
    y_u_T_ev <- tail(Umidas_T$y_hat, 4)[1]
  }
  y_out_fac <- window(gdpts, start=mix_P$end_of_quarter_qev, end=mix_P$end_of_quarter_qev)

  qsc_u_P2[out_row, ] <- crps(y_u_P_ev, as.numeric(y_out_fac), Q, 2)
  qsc_u_T2[out_row, ] <- crps(y_u_T_ev, as.numeric(y_out_fac), Q, 2)

  # Time-varying quantile weights and factor-based headline
  q_score_PT <- array(NA, dim=c(out_row, 2, Q))
  q_score_PT[1:out_row, 1, ] <- qsc_u_P2[1:out_row, ]
  q_score_PT[1:out_row, 2, ] <- qsc_u_T2[1:out_row, ]

  y_hat_combs_PT <- rbind(y_u_P2[out_row, ], y_u_T2[out_row, ])
  kcomb_PT_TV2 <- quant_comb_TV_w(q_score_PT, y_hat_combs_PT, decay, out_row)

  kcomb_PT_w2[,,out_row] <- kcomb_PT_TV2$wq
  kcomb_PT_y2[out_row, ] <- kcomb_PT_TV2$y_comb
}

# Format & export underlying results (separate workbook)
underlying_P_df <- format_quantile_df(y_u_P2,      time_length, dates)   # UNDERLYING (persistent)
transitory_T_df <- format_quantile_df(y_u_T2,      time_length, dates)   # Transitory
headline_PT_df  <- format_quantile_df(kcomb_PT_y2, time_length, dates)   # Factor-based headline

# Weights P vs T (by quantile over time)
factor_weights_df <- format_weights_df(kcomb_PT_w2, time_length, dates)
colnames(factor_weights_df) <- sub("ind1", "P_underlying", colnames(factor_weights_df))
colnames(factor_weights_df) <- sub("ind2", "T_transitory", colnames(factor_weights_df))

# Contributions per quantile (P/T × weights)
factor_fcast_list <- lapply(seq_along(QQ), function(i) {
  data.frame(P_underlying = underlying_P_df[[i+1]],
             T_transitory = transitory_T_df[[i+1]])
})
factor_weight_slices <- lapply(seq_along(QQ), function(i) {
  wqt_arr <- kcomb_PT_w2[, i, 1:time_length, drop = FALSE]  # 2 × 1 × T
  wqt <- t(apply(wqt_arr, 3, c))                            # T × 2
  data.frame(P_underlying = wqt[,1], T_transitory = wqt[,2])
})
factor_contrib_list <- mapply(function(forecast_df, weight_df) {
  cbind(dates = dates[1:time_length], forecast_df * weight_df)
}, factor_fcast_list, factor_weight_slices, SIMPLIFY = FALSE)
names(factor_contrib_list) <- c('10th','20th','30th','40th','median','60th','70th','80th','90th')

openxlsx::write.xlsx(
  x = c(
    list(
      'underlying_P'     = underlying_P_df,
      'transitory_T'     = transitory_T_df,
      'headline_from_PT' = headline_PT_df,
      'factor_weights'   = factor_weights_df
    ),
    factor_contrib_list
  ),
  file = paste0(results_filepath, 'underlying_results.xlsx'),
  overwrite = TRUE
)
# ===================== END UNDERLYING ANALYSIS — FACTOR SPLIT =====================











