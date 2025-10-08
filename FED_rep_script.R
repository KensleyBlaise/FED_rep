# ================= Figure 2 — Rolling MPC (10, 15, 20 quarters) in a 2×2 panel =================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# ---- Config ----
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"
DATE_COL  <- "Datei t"           # column A

# Column names (exact)
COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"

# Rolling windows (quarters) — as requested
ROLL_WINS <- c(10L, 15L, 20L)

# COVID dummy window (intercept shift only)
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# Colors/fills for the three windows
col_map  <- c("10q" = "#1f78b4", "15q" = "#33a02c", "20q" = "#e31a1c")
fill_map <- c("10q" = "#a6cee3", "15q" = "#b2df8a", "20q" = "#fb9a99")

# ---- Helpers ----
pretty_limits_sym0 <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(c(-1, 1))
  rng <- range(v); pad <- 0.05 * diff(rng); if (!is.finite(pad)) pad <- 0.1
  lim <- max(abs(rng)) + pad
  c(-lim, lim)
}

# Rolling OLS → coefficient and OLS SE for chosen regressor (liq or ill)
roll_coeff <- function(df, win, target = c("liq","ill"), with_dummy = FALSE) {
  target <- match.arg(target)
  n <- nrow(df)
  if (n < win) stop("Not enough rows for window ", win, " (n = ", n, ")")
  out <- vector("list", n - win + 1L)

  for (s in 1:(n - win + 1L)) {
    e <- s + win - 1L
    center <- if (win %% 2 == 0) s + (win/2 - 1L) else s + floor(win/2)
    sub <- df[s:e, , drop = FALSE]

    fml <- if (with_dummy) {
      ratio_data ~ Wliq_den + Williq_den + Dcovid   # intercept shift only
    } else {
      ratio_data ~ Wliq_den + Williq_den
    }

    # sanity checks
    ok <- all(is.finite(sub$ratio_data), is.finite(sub$Wliq_den), is.finite(sub$Williq_den))
    if (!ok || var(sub[[ifelse(target=="liq","Wliq_den","Williq_den")]], na.rm = TRUE) <= 0) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }

    fit <- try(lm(fml, data = sub), silent = TRUE)
    if (inherits(fit, "try-error")) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }

    coef_name <- if (target == "liq") "Wliq_den" else "Williq_den"
    b  <- tryCatch(as.numeric(coef(fit)[coef_name]), error = function(e) NA_real_)
    se <- tryCatch(summary(fit)$coef[coef_name, "Std. Error"], error = function(e) NA_real_)
    out[[s]] <- data.frame(Date = df$Date[center], beta = b, se = se)
  }

  do.call(rbind, out) |>
    mutate(Window = paste0(win, "q"),
           beta_cents = 100 * beta,
           lo = 100 * (beta - 1.96 * se),
           hi = 100 * (beta + 1.96 * se)) |>
    arrange(Date)
}

make_panel <- function(df_lines, df_bands, title) {
  ylims <- pretty_limits_sym0(c(df_lines$beta_cents, df_bands$lo, df_bands$hi, 0))
  ggplot() +
    { if (nrow(df_bands) > 0)
        geom_ribbon(data = df_bands,
                    aes(x = Date, ymin = lo, ymax = hi, fill = Window),
                    alpha = 0.22, colour = NA) } +
    geom_line(data = df_lines,
              aes(x = Date, y = beta_cents, colour = Window),
              linewidth = 1.0, na.rm = TRUE) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.7) +
    scale_colour_manual(values = col_map) +
    scale_fill_manual(values = fill_map) +
    scale_y_continuous("Cents", limits = ylims,
                       breaks = pretty(ylims, n = 6),
                       expand = expansion(mult = c(0,0))) +
    labs(title = title, x = NULL, colour = "Window", fill = "Window") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.background = element_blank(),
          legend.position = "bottom")
}

# ---- Load & prepare data (ALL series already REAL) ----
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Parse "Datei t"
x  <- raw[[DATE_COL]]
yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))
if (all(is.na(yq))) {
  if (inherits(x, "Date")) yq <- as.yearqtr(x)
  else if (is.numeric(x))  yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  else                     yq <- as.yearqtr(suppressWarnings(as.Date(x)))
}
stopifnot(any(!is.na(yq)))

d <- raw %>%
  transmute(
    Date   = as.Date(yq),
    yq     = yq,
    C_real = as.numeric(.data[[COL_C]]),
    Y_real = as.numeric(.data[[COL_Y]]),
    T_real = as.numeric(.data[[COL_T]]),
    Wliq   = as.numeric(.data[[COL_WL]]),
    Williq = as.numeric(.data[[COL_WI]])
  ) %>%
  mutate(
    den        = Y_real - T_real,
    ratio_data = C_real / den,     # DEP VAR: C / (Y - T)
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den,
    Dcovid     = as.integer(yq >= COVID_START & yq <= COVID_END)
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den)) %>%
  arrange(Date)

# ---- Compute rolling series for each window ----
# No COVID dummy
roll_liq_nod <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "liq", with_dummy = FALSE)) |> dplyr::bind_rows()
roll_ill_nod <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "ill", with_dummy = FALSE)) |> dplyr::bind_rows()
# With COVID dummy (intercept shift only)
roll_liq_cov <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "liq", with_dummy = TRUE )) |> dplyr::bind_rows()
roll_ill_cov <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "ill", with_dummy = TRUE )) |> dplyr::bind_rows()

# Split lines/ribbons
L1_lines <- dplyr::filter(roll_liq_nod, is.finite(beta_cents)); L1_bands <- dplyr::filter(roll_liq_nod, is.finite(lo) & is.finite(hi))
L2_lines <- dplyr::filter(roll_ill_nod, is.finite(beta_cents)); L2_bands <- dplyr::filter(roll_ill_nod, is.finite(lo) & is.finite(hi))
L3_lines <- dplyr::filter(roll_liq_cov, is.finite(beta_cents)); L3_bands <- dplyr::filter(roll_liq_cov, is.finite(lo) & is.finite(hi))
L4_lines <- dplyr::filter(roll_ill_cov, is.finite(beta_cents)); L4_bands <- dplyr::filter(roll_ill_cov, is.finite(lo) & is.finite(hi))

# ---- Build the four ggplots ----
p1 <- make_panel(L1_lines, L1_bands, "Liquid MPC — rolling (10/15/20q, no COVID dummy)")
p2 <- make_panel(L2_lines, L2_bands, "Illiquid MPC — rolling (10/15/20q, no COVID dummy)")
p3 <- make_panel(L3_lines, L3_bands, "Liquid MPC — rolling (10/15/20q, + COVID dummy)")
p4 <- make_panel(L4_lines, L4_bands, "Illiquid MPC — rolling (10/15/20q, + COVID dummy)")

# ---- Combine in a 2×2 panel ----
if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  panel <- (p1 | p2) / (p3 | p4)
  panel <- panel + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  panel
} else {
  # fallback without extra packages
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
  vp <- function(r, c) grid::viewport(layout.pos.row = r, layout.pos.col = c)
  print(p1 + theme(legend.position = "none"), vp = vp(1,1))
  print(p2 + theme(legend.position = "none"), vp = vp(1,2))
  print(p3 + theme(legend.position = "none"), vp = vp(2,1))
  print(p4 + theme(legend.position = "none"), vp = vp(2,2))
}


















































# ===================== Figure 2 — Rolling MPC (Liquid vs Illiquid; no/with COVID dummy) =====================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# ---- Config ----
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"
DATE_COL  <- "Datei t"           # column A

# Variables (exact names)
COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"
# Optional: include mortgage spread as a control (set to TRUE/FALSE)
INCLUDE_SPREAD <- FALSE
COL_MS <- "Real Mortgage Spread"

# Rolling windows (quarters)
ROLL_WINS <- c(30L, 35L, 40L)

# COVID dummy window
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# Colors / fills (per window)
col_map  <- c("30q" = "#1f78b4", "35q" = "#33a02c", "40q" = "#e31a1c")
fill_map <- c("30q" = "#a6cee3", "35q" = "#b2df8a", "40q" = "#fb9a99")

# ---- Helpers ----
pretty_limits_sym0 <- function(v) {
  v <- v[is.finite(v)]
  if (!length(v)) return(c(-1, 1))
  rng <- range(v); pad <- 0.05 * diff(rng); if (!is.finite(pad)) pad <- 0.1
  lim <- max(abs(rng)) + pad
  c(-lim, lim)
}

# rolling OLS that returns the coefficient (and OLS SE) for a target regressor
roll_coeff <- function(df, win, target = c("liq", "ill"), with_dummy = FALSE, include_spread = FALSE) {
  target <- match.arg(target)
  n <- nrow(df)
  if (n < win) stop("Not enough rows for window ", win, " (n = ", n, ")")
  out <- vector("list", n - win + 1L)

  for (s in 1:(n - win + 1L)) {
    e <- s + win - 1L
    center <- if (win %% 2 == 0) s + (win/2 - 1L) else s + floor(win/2)
    sub <- df[s:e, , drop = FALSE]

    # build formula
    if (include_spread) {
      if (with_dummy) {
        fml <- ratio_data ~ Wliq_den + Williq_den + MSpr + Dcovid
      } else {
        fml <- ratio_data ~ Wliq_den + Williq_den + MSpr
      }
    } else {
      if (with_dummy) {
        fml <- ratio_data ~ Wliq_den + Williq_den + Dcovid
      } else {
        fml <- ratio_data ~ Wliq_den + Williq_den
      }
    }

    # sanity: finite & some variation in regressors
    ok <- all(is.finite(sub$ratio_data), is.finite(sub$Wliq_den), is.finite(sub$Williq_den))
    if (include_spread) ok <- ok && all(is.finite(sub$MSpr))
    if (!ok || var(sub[[ifelse(target=="liq","Wliq_den","Williq_den")]], na.rm = TRUE) <= 0) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }

    fit <- try(lm(fml, data = sub), silent = TRUE)
    if (inherits(fit, "try-error")) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }

    coef_name <- if (target == "liq") "Wliq_den" else "Williq_den"
    b  <- tryCatch(as.numeric(coef(fit)[coef_name]), error = function(e) NA_real_)
    se <- tryCatch(summary(fit)$coef[coef_name, "Std. Error"], error = function(e) NA_real_)
    if (!is.finite(b))  b  <- NA_real_
    if (!is.finite(se)) se <- NA_real_

    out[[s]] <- data.frame(Date = df$Date[center], beta = b, se = se)
  }

  do.call(rbind, out) |>
    mutate(Window = paste0(win, "q"),
           beta_cents = 100 * beta,
           lo = 100 * (beta - 1.96 * se),
           hi = 100 * (beta + 1.96 * se)) |>
    arrange(Date)
}

make_panel <- function(df_lines, df_bands, title) {
  # symmetric limits around 0 so the zero line is clear
  ylims <- pretty_limits_sym0(c(df_lines$beta_cents, df_bands$lo, df_bands$hi, 0))
  ggplot() +
    { if (nrow(df_bands) > 0)
        geom_ribbon(data = df_bands,
                    aes(x = Date, ymin = lo, ymax = hi, fill = Window),
                    alpha = 0.22, colour = NA) } +
    geom_line(data = df_lines,
              aes(x = Date, y = beta_cents, colour = Window),
              linewidth = 1.0, na.rm = TRUE) +
    geom_hline(yintercept = 0, colour = "black", linewidth = 0.7) +
    scale_colour_manual(values = col_map) +
    scale_fill_manual(values = fill_map) +
    scale_y_continuous("Cents",
                       limits = ylims,
                       breaks = pretty(ylims, n = 6),
                       expand = expansion(mult = c(0,0))) +
    labs(title = title, x = NULL, colour = "Window", fill = "Window") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          plot.background = element_blank(),
          legend.position = "bottom")
}

# ---- Load & prepare data (all series already REAL) ----
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Parse "Datei t" (YYYY Qq or Month-YY etc.)
x  <- raw[[DATE_COL]]
yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))
if (all(is.na(yq))) {
  if (inherits(x, "Date")) yq <- as.yearqtr(x)
  else if (is.numeric(x))  yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  else                     yq <- as.yearqtr(suppressWarnings(as.Date(x)))
}
stopifnot(any(!is.na(yq)))

d <- raw %>%
  transmute(
    Date   = as.Date(yq),
    yq     = yq,
    C_real = as.numeric(.data[[COL_C]]),
    Y_real = as.numeric(.data[[COL_Y]]),
    T_real = as.numeric(.data[[COL_T]]),
    Wliq   = as.numeric(.data[[COL_WL]]),
    Williq = as.numeric(.data[[COL_WI]]),
    MSpr   = if (INCLUDE_SPREAD) as.numeric(.data[[COL_MS]]) else NA_real_
  ) %>%
  mutate(
    den        = Y_real - T_real,
    ratio_data = C_real / den,       # DEP VAR: C / (Y - T)
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den,
    Dcovid     = as.integer(yq >= COVID_START & yq <= COVID_END)
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den)) %>%
  { if (INCLUDE_SPREAD) filter(., is.finite(MSpr)) else . } %>%
  arrange(Date)

# ---- Compute rolling series for each window ----
# A) No COVID dummy
roll_liq_nod   <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "liq", with_dummy = FALSE, include_spread = INCLUDE_SPREAD)) |> bind_rows()
roll_ill_nod   <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "ill", with_dummy = FALSE, include_spread = INCLUDE_SPREAD)) |> bind_rows()
# B) With COVID dummy (intercept shift only; no interactions)
roll_liq_cov   <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "liq", with_dummy = TRUE,  include_spread = INCLUDE_SPREAD)) |> bind_rows()
roll_ill_cov   <- lapply(ROLL_WINS, function(w) roll_coeff(d, w, target = "ill", with_dummy = TRUE,  include_spread = INCLUDE_SPREAD)) |> bind_rows()

# Split lines/ribbons
L1_lines <- roll_liq_nod %>% filter(is.finite(beta_cents)); L1_bands <- roll_liq_nod %>% filter(is.finite(lo) & is.finite(hi))
L2_lines <- roll_ill_nod %>% filter(is.finite(beta_cents)); L2_bands <- roll_ill_nod %>% filter(is.finite(lo) & is.finite(hi))
L3_lines <- roll_liq_cov %>% filter(is.finite(beta_cents)); L3_bands <- roll_liq_cov %>% filter(is.finite(lo) & is.finite(hi))
L4_lines <- roll_ill_cov %>% filter(is.finite(beta_cents)); L4_bands <- roll_ill_cov %>% filter(is.finite(lo) & is.finite(hi))

# ---- Build the four panels ----
p1 <- make_panel(L1_lines, L1_bands, "Liquid MPC — rolling (no COVID dummy)")
p2 <- make_panel(L2_lines, L2_bands, "Illiquid MPC — rolling (no COVID dummy)")
p3 <- make_panel(L3_lines, L3_bands, "Liquid MPC — rolling (+ COVID dummy)")
p4 <- make_panel(L4_lines, L4_bands, "Illiquid MPC — rolling (+ COVID dummy)")

# ---- Display as a 2×2 panel without extra packages ----
op <- par(mfrow = c(2, 2))
print(p1); print(p2); print(p3); print(p4)
par(op)



























































# ===== Nice table with stargazer (console + HTML) =====
# install.packages(c("stargazer","sandwich","lmtest"))  # run once

library(stargazer)
library(sandwich)
library(lmtest)

# Newey–West (HAC) standard errors
nw_lag <- 4  # change if you prefer (e.g., 8)
se_base  <- sqrt(diag(NeweyWest(m_base,  lag = nw_lag, prewhite = FALSE, adjust = TRUE)))
se_covid <- sqrt(diag(NeweyWest(m_covid, lag = nw_lag, prewhite = FALSE, adjust = TRUE)))

# Console table
stargazer(m_base, m_covid,
  type = "text",
  se = list(se_base, se_covid),
  dep.var.labels = "C / (Y − T)",
  column.labels = c("Baseline", "+ COVID dummy"),
  covariate.labels = c(
    "Liquid assets / (Y−T)",
    "Illiquid assets / (Y−T)",
    "Mortgage spread",
    "COVID dummy"
  ),
  omit.stat = c("f","ser"),
  digits = 3,
  notes = "HAC (Newey–West) s.e.; COVID dummy = 1 for 2020Q1–2021Q4.",
  notes.align = "l"
)

# HTML file (open reg_table.html)
stargazer(m_base, m_covid,
  type = "html", out = "reg_table.html",
  se = list(se_base, se_covid),
  dep.var.labels = "C / (Y − T)",
  column.labels = c("Baseline", "+ COVID dummy"),
  covariate.labels = c(
    "Liquid assets / (Y−T)",
    "Illiquid assets / (Y−T)",
    "Mortgage spread",
    "COVID dummy"
  ),
  omit.stat = c("f","ser"),
  digits = 3,
  notes = "HAC (Newey–West) s.e.; COVID dummy = 1 for 2020Q1–2021Q4.",
  notes.align = "l"
)

























# ===== Chart 1 — C/(Y−T) with liquid/illiquid/(spread), COVID dummy (no interactions) =====
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"
DATE_COL  <- "Datei t"   # your date/quarter column in column A

# Exact variable names
COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"
COL_MS <- "Real Mortgage Spread"

# COVID period for the dummy
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

nice_limits <- function(v, pad_mult = 0.04) {
  v <- v[is.finite(v)]; r <- range(v); s <- max(diff(r), 1e-9)
  c(r[1] - pad_mult*s, r[2] + pad_mult*s)
}

# --- Load ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Parse "Datei t" to quarterly Date
x  <- raw[[DATE_COL]]
yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))
if (all(is.na(yq))) {
  if (inherits(x, "Date")) yq <- as.yearqtr(x)
  else if (is.numeric(x))  yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  else                     yq <- as.yearqtr(suppressWarnings(as.Date(x)))
}
stopifnot(any(!is.na(yq)))
Date <- as.Date(yq)

# --- Build dataset (ALL series already real; no deflator anywhere) ---
d <- raw %>%
  transmute(
    Date,
    yq,
    C_real  = as.numeric(.data[[COL_C]]),
    Y_real  = as.numeric(.data[[COL_Y]]),
    T_real  = as.numeric(.data[[COL_T]]),
    Wliq    = as.numeric(.data[[COL_WL]]),
    Williq  = as.numeric(.data[[COL_WI]]),
    MSpr    = as.numeric(.data[[COL_MS]])
  ) %>%
  mutate(
    den        = Y_real - T_real,
    ratio_data = C_real / den,         # <-- YOUR SPEC: C / (Y - T)
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den,
    Dcovid     = as.integer(yq >= COVID_START & yq <= COVID_END)
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den), is.finite(MSpr)) %>%
  arrange(Date)

# --- OLS models (no interactions) ---
m_base  <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr, data = d)
m_covid <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr + Dcovid, data = d)

d$pred_base  <- as.numeric(predict(m_base,  newdata = d))
d$pred_covid <- as.numeric(predict(m_covid, newdata = d))

ylims <- nice_limits(c(d$ratio_data, d$pred_base, d$pred_covid))

# --- Plot: Observed vs Predicted (baseline) vs Predicted (+COVID dummy) ---
p <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data,
                colour = "Observed data", linetype = "Observed data"),
            linewidth = 1.1) +
  geom_line(aes(y = pred_base,
                colour = "Predicted (no COVID dummy)",
                linetype = "Predicted (no COVID dummy)"),
            linewidth = 1.6, lineend = "round") +
  geom_line(aes(y = pred_covid,
                colour = "Predicted (+ COVID dummy)",
                linetype = "Predicted (+ COVID dummy)"),
            linewidth = 1.2) +
  scale_colour_manual(values = c("Observed data" = "black",
                                 "Predicted (no COVID dummy)" = "#2C7FB8",
                                 "Predicted (+ COVID dummy)"  = "#D95F02")) +
  scale_linetype_manual(values = c("Observed data" = "solid",
                                   "Predicted (no COVID dummy)" = "dotted",
                                   "Predicted (+ COVID dummy)"  = "dashed")) +
  guides(colour = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) +
  scale_y_continuous(limits = ylims, expand = expansion(mult = c(0,0))) +
  labs(
    title = "Predicted vs Observed C/(Y−T)\nwith and without a COVID dummy (2020Q1–2021Q4)",
    subtitle = "Regressors: Liquid/(Y−T), Illiquid/(Y−T), Mortgage spread (level)",
    y = "Ratio", x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.background = element_blank(),
        legend.position = "bottom")

print(p)

# Optional: show regression summaries
# summary(m_base); summary(m_covid)























# ===== Regressions for Chart 1 + nice table with stargazer =====
# install.packages(c("readxl","dplyr","zoo","stargazer","sandwich","lmtest"))  # once

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(zoo)
  library(stargazer); library(sandwich); library(lmtest)
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"
DATE_COL  <- "Datei t"

COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"
COL_MS <- "Real Mortgage Spread"

COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# --- Load and parse date (quarter) ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

x <- raw[[DATE_COL]]
yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))
if (all(is.na(yq))) {
  if (inherits(x, "Date")) yq <- as.yearqtr(x)
  else if (is.numeric(x))  yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  else                     yq <- as.yearqtr(suppressWarnings(as.Date(x)))
}
stopifnot(any(!is.na(yq)))

# --- Build analysis frame (ALL variables already real) ---
d <- raw %>%
  transmute(
    Date    = as.Date(yq),
    yq      = yq,
    C_real  = as.numeric(.data[[COL_C]]),
    Y_real  = as.numeric(.data[[COL_Y]]),
    T_real  = as.numeric(.data[[COL_T]]),
    Wliq    = as.numeric(.data[[COL_WL]]),
    Williq  = as.numeric(.data[[COL_WI]]),
    MSpr    = as.numeric(.data[[COL_MS]])
  ) %>%
  mutate(
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den,
    Dcovid     = as.integer(yq >= COVID_START & yq <= COVID_END)
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den), is.finite(MSpr)) %>%
  arrange(Date)

# --- Models ---
m1 <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr, data = d)                     # baseline
m2 <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr + Dcovid, data = d)            # + COVID dummy
m3 <- lm(ratio_data ~ (Wliq_den + Williq_den + MSpr) * Dcovid, data = d)          # + interactions

# --- HAC/Newey–West standard errors (time series robust) ---
# choose a modest lag (change if you prefer)
nw_lag <- 4
se1 <- sqrt(diag(NeweyWest(m1, lag = nw_lag, prewhite = FALSE, adjust = TRUE)))
se2 <- sqrt(diag(NeweyWest(m2, lag = nw_lag, prewhite = FALSE, adjust = TRUE)))
se3 <- sqrt(diag(NeweyWest(m3, lag = nw_lag, prewhite = FALSE, adjust = TRUE)))

# --- Nice table to console ---
stargazer(m1, m2, m3,
  type = "text",
  se = list(se1, se2, se3),
  dep.var.labels = "(C − T) / (Y − T)",
  column.labels = c("Baseline", "+ COVID dummy", "+ COVID interactions"),
  covariate.labels = c(
    "Liquid assets / (Y−T)",
    "Illiquid assets / (Y−T)",
    "Mortgage spread",
    "COVID dummy",
    "Liquid × COVID",
    "Illiquid × COVID",
    "Spread × COVID"
  ),
  digits = 3, omit.stat = c("f","ser"),
  notes = "HAC (Newey–West) s.e.; COVID dummy = 1 for 2020Q1–2021Q4.",
  notes.align = "l"
)

# --- Save an HTML version too (open reg_table.html in a browser) ---
stargazer(m1, m2, m3,
  type = "html", out = "reg_table.html",
  se = list(se1, se2, se3),
  dep.var.labels = "(C − T) / (Y − T)",
  column.labels = c("Baseline", "+ COVID dummy", "+ COVID interactions"),
  covariate.labels = c(
    "Liquid assets / (Y−T)",
    "Illiquid assets / (Y−T)",
    "Mortgage spread",
    "COVID dummy",
    "Liquid × COVID",
    "Illiquid × COVID",
    "Spread × COVID"
  ),
  digits = 3, omit.stat = c("f","ser"),
  notes = "HAC (Newey–West) s.e.; COVID dummy = 1 for 2020Q1–2021Q4.",
  notes.align = "l"
)





















































# ===== Figure 1 — Predicted vs Observed with COVID dummy (Date col = "Datei t") =====
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"
DATE_COL  <- "Datei t"                # in column A

# Exact column names
COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"
COL_MS <- "Real Mortgage Spread"

# COVID period (dummy = 1 inside this window)
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# helper for tidy y-limits
nice_limits <- function(v, pad_mult = 0.04) {
  v <- v[is.finite(v)]; r <- range(v); s <- max(diff(r), 1e-9)
  c(r[1] - pad_mult*s, r[2] + pad_mult*s)
}

# --- Load ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# Parse "Datei t" into quarterly Date
x <- raw[[DATE_COL]]
yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))
if (all(is.na(yq))) {
  if (inherits(x, "Date")) yq <- as.yearqtr(x)
  else if (is.numeric(x))  yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  else                     yq <- as.yearqtr(suppressWarnings(as.Date(x)))
}
stopifnot(any(!is.na(yq)))
Date <- as.Date(yq)

# --- Build analysis frame (ALL series are already real — no deflator) ---
d <- raw %>%
  transmute(
    Date,
    yq        = yq,
    C_real    = as.numeric(.data[[COL_C]]),
    Y_real    = as.numeric(.data[[COL_Y]]),
    T_real    = as.numeric(.data[[COL_T]]),
    Wliq      = as.numeric(.data[[COL_WL]]),
    Williq    = as.numeric(.data[[COL_WI]]),
    MSpr      = as.numeric(.data[[COL_MS]])
  ) %>%
  mutate(
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den,
    Dcovid     = as.integer(yq >= COVID_START & yq <= COVID_END)   # COVID dummy
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den), is.finite(MSpr)) %>%
  arrange(Date)

# --- Two models: baseline vs + COVID dummy (intercept shift) ---
fit_base  <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr, data = d)
fit_covid <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr + Dcovid, data = d)

d$pred_base  <- as.numeric(predict(fit_base,  newdata = d))
d$pred_covid <- as.numeric(predict(fit_covid, newdata = d))

ylims <- nice_limits(c(d$ratio_data, d$pred_base, d$pred_covid))

# --- Plot: Observed vs Predicted (no dummy) vs Predicted (+ COVID dummy) ---
p <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data,
                colour = "Observed data", linetype = "Observed data"),
            linewidth = 1.1) +
  geom_line(aes(y = pred_base,
                colour = "Predicted (no COVID dummy)",
                linetype = "Predicted (no COVID dummy)"),
            linewidth = 1.6, lineend = "round") +
  geom_line(aes(y = pred_covid,
                colour = "Predicted (+ COVID dummy)",
                linetype = "Predicted (+ COVID dummy)"),
            linewidth = 1.2) +
  scale_colour_manual(
    values = c("Observed data" = "black",
               "Predicted (no COVID dummy)" = "#2C7FB8",
               "Predicted (+ COVID dummy)"  = "#D95F02")
  ) +
  scale_linetype_manual(
    values = c("Observed data" = "solid",
               "Predicted (no COVID dummy)" = "dotted",
               "Predicted (+ COVID dummy)"  = "dashed")
  ) +
  guides(colour = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) +
  scale_y_continuous(limits = ylims, expand = expansion(mult = c(0,0))) +
  labs(title = "Predicted vs Observed consumption-to-income ratio\nwith and without a COVID dummy (2020Q1–2021Q4)",
       subtitle = "Regressors: Real Liquid Assets/(Y−T), Real Illiquid Assets/(Y−T), Real Mortgage Spread",
       y = "Ratio", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.background = element_blank(),
        legend.position = "bottom")

print(p)

# Optional: quick comparison of models
# broom::glance(fit_base); broom::glance(fit_covid)
# summary(fit_base); summary(fit_covid)




























# ===== Figure 1 — Predicted vs Observed (Date column = "Datei t") =====
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "variables"

# Exact column names
DATE_COL <- "Datei t"                # << your date/quarter column in col A
COL_C  <- "Real Consumption"
COL_Y  <- "Real Income"
COL_T  <- "Real Gov Benefits"
COL_WL <- "Real Liquid Assets"
COL_WI <- "Real Illiquid Assets"
COL_MS <- "Real Mortgage Spread"

# helper for tidy y-limits
nice_limits <- function(v, pad_mult = 0.04) {
  v <- v[is.finite(v)]; r <- range(v); s <- max(diff(r), 1e-9)
  c(r[1] - pad_mult*s, r[2] + pad_mult*s)
}

# --- Load ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

# --- Parse "Datei t" into Date (try quarters first, then dates/serials) ---
x <- raw[[DATE_COL]]

yq <- suppressWarnings(as.yearqtr(x, format = "%Y Q%q"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%y"))
if (all(is.na(yq))) yq <- suppressWarnings(as.yearqtr(x, format = "%b-%Y"))

if (all(is.na(yq))) {
  # maybe it is an actual date or an Excel serial
  if (inherits(x, "Date")) {
    yq <- as.yearqtr(x)
  } else if (is.numeric(x)) {
    yq <- as.yearqtr(as.Date(x, origin = "1899-12-30"))
  } else {
    yq <- as.yearqtr(suppressWarnings(as.Date(x)))
  }
}
if (all(is.na(yq))) stop("Could not parse '", DATE_COL, "' as a date/quarter.")

Date <- as.Date(yq)

# --- Build analysis frame (ALL SERIES ARE ALREADY REAL — no deflator) ---
d <- raw %>%
  transmute(
    Date,
    C_real = as.numeric(.data[[COL_C]]),
    Y_real = as.numeric(.data[[COL_Y]]),
    T_real = as.numeric(.data[[COL_T]]),
    Wliq   = as.numeric(.data[[COL_WL]]),
    Williq = as.numeric(.data[[COL_WI]]),
    MSpr   = as.numeric(.data[[COL_MS]])
  ) %>%
  mutate(
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    Wliq_den   = Wliq / den,
    Williq_den = Williq / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(Wliq_den), is.finite(Williq_den), is.finite(MSpr)) %>%
  arrange(Date)

# --- OLS & prediction ---
fit <- lm(ratio_data ~ Wliq_den + Williq_den + MSpr, data = d)
d$ratio_pred <- as.numeric(predict(fit, newdata = d))
ylims <- nice_limits(c(d$ratio_data, d$ratio_pred))

# --- Plot ---
p <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Observed data", linetype = "Observed data"),
            linewidth = 1.1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted", linetype = "Predicted"),
            linewidth = 1.8, lineend = "round") +
  scale_colour_manual(values = c("Observed data" = "black", "Predicted" = "#2C7FB8"),
                      breaks = c("Predicted","Observed data"),
                      labels = c("Predicted","Observed data")) +
  scale_linetype_manual(values = c("Observed data" = "solid", "Predicted" = "dotted"),
                        breaks = c("Predicted","Observed data"),
                        labels = c("Predicted","Observed data")) +
  guides(colour = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) +
  scale_y_continuous(limits = ylims, expand = expansion(mult = c(0,0))) +
  labs(title = "Predicted vs Observed consumption-to-income ratio (3 covariates)",
       y = "Ratio", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.background = element_blank(),
        legend.position = "bottom")

print(p)

# Optional: see coefficients
# summary(fit)
















































# ================================================================
# Reproduce Figure 1 (with a structural break at COVID = 2020Q1)
# and Figure 2 (rolling MPC with 30/35/40-quarter windows).
# - Uses your deflator AS-IS (no rescaling)
# - C and Y already real; only T and W are deflated by P
# - No recession shading
# - Clean border; minimal grid
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
BREAK_QTR   <- zoo::as.yearqtr("2020 Q1")  # structural break AT COVID
ROLL_WINS   <- c(30L, 35L, 40L)            # windows for Figure 2

# Exact column names in your sheet
COL_Q <- "Q"              # e.g. "Mar-00"
COL_Y <- "Real income"    # already real
COL_C <- "Real Cons"      # already real
COL_T <- "Gov benefits"   # nominal -> deflate
COL_W <- "Ill wealth"     # nominal -> deflate
COL_P <- "Cons Deflator"  # deflator (AS-IS)

# ---------- helper: "nice" y-limits ----------
nice_limits <- function(vals, pad_mult = 0.05) {
  vals <- vals[is.finite(vals)]
  r <- range(vals, na.rm = TRUE)
  span <- max(r[2] - r[1], 1e-9)
  pad <- pad_mult * span
  c(r[1] - pad, r[2] + pad)
}

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
    P        = as.numeric(.data[[COL_P]])     # USED AS-IS (no /100)
  )

# Parse quarters like "Mar-00" (fallback to "Mar-2000")
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Deflate ONLY transfers & wealth; build regression variables
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

# ---------------- Figure 1: Data vs Predicted + Predicted with COVID break ----------------
# No-break model
ols_base <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- as.numeric(predict(ols_base, newdata = d))

# Break model at COVID (allows intercept + slope shift)
d$post   <- as.integer(d$date >= BREAK_QTR)
d$post_W <- d$post * d$W_over_den
ols_break <- lm(ratio_data ~ W_over_den + post + post_W, data = d)
d$ratio_pred_break <- as.numeric(predict(ols_break, newdata = d))

ylims_f1 <- nice_limits(c(d$ratio_data, d$ratio_pred, d$ratio_pred_break))

fig1 <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Observed data", linetype = "Observed data"),
            linewidth = 1.1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted", linetype = "Predicted"),
            linewidth = 1.4, lineend = "round") +
  geom_line(aes(y = ratio_pred_break, colour = "Predicted (break at 2020Q1)",
                linetype = "Predicted (break at 2020Q1)"),
            linewidth = 1.2) +
  scale_colour_manual(values = c("Observed data" = "black",
                                 "Predicted" = "#2C7FB8",
                                 "Predicted (break at 2020Q1)" = "#D95F02")) +
  scale_linetype_manual(values = c("Observed data" = "solid",
                                   "Predicted" = "dotted",
                                   "Predicted (break at 2020Q1)" = "dashed")) +
  scale_y_continuous(limits = ylims_f1, expand = expansion(mult = c(0,0))) +
  labs(title = "Figure 1. Consumption-to-income ratio (COVID break at 2020Q1)",
       y = "Ratio", x = NULL, colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.background = element_blank(),
        legend.position = "bottom")

print(fig1)

# ---------------- Figure 2: Rolling MPC (30/35/40) + break benchmarks at COVID -----------
# Pre-/post-break MPC levels (from the break model)
co <- coef(ols_break)
beta_pre  <- unname(co["W_over_den"])
beta_post <- beta_pre + unname(co["post_W"])
y_pre  <- 100 * beta_pre
y_post <- 100 * beta_post

# Rolling OLS helper
roll_ols <- function(df, win) {
  n <- nrow(df)
  if (n < win) stop("Not enough rows for window = ", win, " (n = ", n, ")")
  out <- vector("list", n - win + 1L)
  for (s in 1:(n - win + 1L)) {
    e <- s + win - 1L
    center <- if (win %% 2 == 0) s + (win/2 - 1L) else s + floor(win/2)
    sub <- df[s:e, , drop = FALSE]
    if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
        var(sub$W_over_den, na.rm = TRUE) <= 0) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }
    fit <- lm(ratio_data ~ W_over_den, data = sub)
    b   <- as.numeric(coef(fit)["W_over_den"])
    se  <- summary(fit)$coef["W_over_den", "Std. Error"]
    out[[s]] <- data.frame(Date = df$Date[center], beta = b, se = se)
  }
  do.call(rbind, out) %>%
    mutate(Window = paste0(win, "q"),
           beta_cents = 100*beta,
           lo = 100*(beta - 1.96*se),
           hi = 100*(beta + 1.96*se)) %>%
    arrange(Date)
}

roll_list <- lapply(ROLL_WINS, function(w) roll_ols(d, w))
roll_all  <- dplyr::bind_rows(roll_list)

lines_df <- roll_all %>% filter(is.finite(beta_cents))
band_df  <- roll_all %>% filter(is.finite(lo) & is.finite(hi))

# y-limits include rolling bands AND break levels
vals <- c(lines_df$beta_cents, band_df$lo, band_df$hi, y_pre, y_post)
vals <- vals[is.finite(vals)]
ylims <- nice_limits(vals, pad_mult = 0.06)

# Bracket at COVID break (clipped to plotting range)
x_min   <- min(d$Date);  x_max <- max(d$Date)
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

# Colors/fills for windows
col_map  <- c("30q" = "#1f78b4", "35q" = "#33a02c", "40q" = "#e31a1c")
fill_map <- c("30q" = "#a6cee3", "35q" = "#b2df8a", "40q" = "#fb9a99")

fig2 <- ggplot() +
  { if (nrow(band_df) > 0)
      geom_ribbon(data = band_df,
                  aes(x = Date, ymin = lo, ymax = hi, fill = Window),
                  alpha = 0.22, colour = NA) } +
  geom_line(data = lines_df,
            aes(x = Date, y = beta_cents, colour = Window),
            linewidth = 1.0, na.rm = TRUE) +
  { if (nrow(hseg) > 0)
      geom_segment(data = hseg,
                   aes(x = xstart, xend = xend, y = y, yend = y),
                   linetype = "dotted", colour = "black", linewidth = 0.9) } +
  { if (!is.null(vseg))
      geom_segment(data = vseg,
                   aes(x = x, xend = x, y = y0, yend = y1),
                   linetype = "dotted", colour = "black", linewidth = 0.9) } +
  scale_colour_manual(values = col_map) +
  scale_fill_manual(values = fill_map) +
  scale_y_continuous("Cents", limits = ylims,
                     breaks = pretty(ylims, n = 6),
                     expand = expansion(mult = c(0,0))) +
  labs(title = "Figure 2. Rolling MPC — 30q vs 35q vs 40q (break at 2020Q1)",
       x = NULL, colour = "Window", fill = "Window") +
  theme_minimal(base_size = 12) +
  theme(panel.grid   = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        plot.background = element_blank(),
        legend.position = "bottom")

print(fig2)
# ================================================================




































# ==== Rolling MPC with multiple windows (30, 35, 40), single plot, overlapping CIs ====
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
ROLL_WINS <- c(30L, 35L, 40L)   # windows to compare (in quarters)

# Use your column names exactly
COL_Q <- "Q"
COL_Y <- "Real income"     # already real
COL_C <- "Real Cons"       # already real
COL_T <- "Gov benefits"    # nominal -> deflate
COL_W <- "Ill wealth"      # nominal -> deflate
COL_P <- "Cons Deflator"   # deflator (USE AS-IS — NO RESCALE)

# Remove COVID period (as requested in earlier steps)
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# --- Load & prep ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

d <- raw %>%
  transmute(
    Q        = .data[[COL_Q]],
    Y_real   = as.numeric(.data[[COL_Y]]),
    C_real   = as.numeric(.data[[COL_C]]),
    T_nom    = as.numeric(.data[[COL_T]]),
    W_nom    = as.numeric(.data[[COL_W]]),
    P        = as.numeric(.data[[COL_P]])   # used AS-IS (no /100)
  )

# Parse quarters like "Mar-00" (fallback "Mar-2000")
d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Remove COVID period
d <- d %>% filter(date < COVID_START | date > COVID_END)

# Build regression variables (deflate ONLY transfers & illiquid wealth)
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

stopifnot(nrow(d) >= max(ROLL_WINS))

# --- Rolling OLS helper: returns a data.frame with beta & OLS SE for a given window ---
roll_ols <- function(df, win) {
  n <- nrow(df)
  out <- vector("list", n - win + 1L)
  for (s in 1:(n - win + 1L)) {
    e <- s + win - 1L
    # center index (works for even/odd windows)
    center <- if (win %% 2 == 0) s + (win/2 - 1L) else s + floor(win/2)
    sub <- df[s:e, , drop = FALSE]

    if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
        var(sub$W_over_den, na.rm = TRUE) <= 0) {
      out[[s]] <- data.frame(Date = df$Date[center], beta = NA_real_, se = NA_real_)
      next
    }
    fit <- lm(ratio_data ~ W_over_den, data = sub)
    b   <- as.numeric(coef(fit)["W_over_den"])
    se  <- summary(fit)$coef["W_over_den", "Std. Error"]
    out[[s]] <- data.frame(Date = df$Date[center], beta = b, se = se)
  }
  do.call(rbind, out) %>%
    mutate(
      Window      = paste0(win, "q"),
      beta_cents  = 100 * beta,
      lo          = 100 * (beta - 1.96 * se),
      hi          = 100 * (beta + 1.96 * se)
    ) %>%
    arrange(Date)
}

# --- Compute rolling series for each window and bind ---
roll_list <- lapply(ROLL_WINS, function(w) roll_ols(d, w))
roll_all  <- dplyr::bind_rows(roll_list)

# Split for plotting
roll_lines <- roll_all %>% filter(is.finite(beta_cents))
roll_bands <- roll_all %>% filter(is.finite(lo) & is.finite(hi))

# Dynamic symmetric y-limits around zero (so 0 line is always visible)
vals <- c(roll_lines$beta_cents, roll_bands$lo, roll_bands$hi, 0)
vals <- vals[is.finite(vals)]
rng  <- range(vals)
pad  <- 0.05 * diff(rng); if (!is.finite(pad)) pad <- 0.1
lim  <- max(abs(rng)) + pad
ylims <- c(-lim, lim)

# Colors/fills for the three windows
col_map  <- c("30q" = "#1f78b4", "35q" = "#33a02c", "40q" = "#e31a1c")
fill_map <- c("30q" = "#a6cee3", "35q" = "#b2df8a", "40q" = "#fb9a99")

# --- Plot: three rolling lines + overlapping CI ribbons on the same axes ---
p <- ggplot() +
  { if (nrow(roll_bands) > 0)
      geom_ribbon(data = roll_bands,
                  aes(x = Date, ymin = lo, ymax = hi, fill = Window),
                  alpha = 0.25, colour = NA) } +
  geom_line(data = roll_lines,
            aes(x = Date, y = beta_cents, colour = Window),
            linewidth = 1.0, na.rm = TRUE) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.7) +
  scale_colour_manual(values = col_map) +
  scale_fill_manual(values = fill_map) +
  scale_y_continuous("Cents",
                     limits = ylims,
                     breaks = pretty(ylims, n = 6),
                     expand = expansion(mult = c(0,0))) +
  labs(title = "Rolling MPC out of Wealth — 30q vs 35q vs 40q (OLS, 95% CI, COVID removed)",
       x = NULL, colour = "Window", fill = "Window") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        plot.background = element_blank(),
        legend.position = "bottom")

print(p)



























ggplot() +
  geom_line(data = subset(df_obs, Quarter >= as.yearqtr("2021 Q1")),
            aes(x = Quarter, y = Growth), colour = "black") +
  geom_point(data = df_fc, aes(x = Quarter, y = Forecast),
             colour = "red", size = 3) +
  geom_line(data = df_fc, aes(x = Quarter, y = Forecast),
            colour = "red", linetype = "dotted") +
  labs(title = "Mortgage Approvals Growth (QoQ)",
       x = "Quarter", y = "Growth rate (%)") +
  theme_minimal()

fc_vals  <- as.numeric(fcst$mean)       # the two predicted values
fc_time  <- time(fcst$mean)             # their corresponding quarters

# Put them into a small data.frame for ggplot
df_fc <- data.frame(
  Quarter = as.yearqtr(fc_time),
  Forecast = fc_vals
)


autoplot(g_ts) +
  geom_point(data = df_fc, aes(x = Quarter, y = Forecast),
             colour = "red", shape = 16, size = 3) +
  geom_line(data = df_fc, aes(x = Quarter, y = Forecast),
            colour = "red", linetype = "dotted") +
  labs(title = "Mortgage Approvals Growth (QoQ)",
       y = "Growth rate (%)", x = "Quarter") +
  theme_minimal()




suppressPackageStartupMessages({
  library(forecast)   # auto.arima, forecast, tsCV
})

# ---- Quarterly series (frequency = 4) ----
# If it's already a ts with freq=4, this will keep it; otherwise convert.
mortgage_ts <- if (is.ts(mortgage_Approval) && frequency(mortgage_Approval) == 4) {
  mortgage_Approval
} else {
  ts(mortgage_Approval, frequency = 4)
}

# ---- Fit on full sample ----
fit <- auto.arima(mortgage_ts)

# ---- Next 1- and 2-step forecasts from the final fit ----
fc <- forecast(fit, h = 2)
fc_1step <- fc$mean[1]
fc_2step <- fc$mean[2]

# ---- Rolling CV RMSE for h = 1 and h = 2 (re-fits auto.arima each step) ----
cv_errors <- tsCV(
  mortgage_ts,
  forecastfunction = function(y, h) forecast(auto.arima(y), h = h)$mean,
  h = 2
)
rmse_h1 <- sqrt(mean(cv_errors[, 1]^2, na.rm = TRUE))
rmse_h2 <- sqrt(mean(cv_errors[, 2]^2, na.rm = TRUE))

# ---- Print results ----
cat("Selected model:", capture.output(fit)[1], "\n\n")
cat("1-step-ahead forecast: ", round(fc_1step, 3), "\n")
cat("2-step-ahead forecast: ", round(fc_2step, 3), "\n\n")
cat("RMSE (h = 1): ", round(rmse_h1, 4), "\n")
cat("RMSE (h = 2): ", round(rmse_h2, 4), "\n")






# ================================================================
# PART B: Scenario Forecasts
# ================================================================

# Function to create scenario forecasts
scenario_forecast <- function(y, h, scenario = c("trend", "constant", "reversal")) {
  fit <- auto.arima(y)
  fc <- forecast(fit, h = h)
  
  # extract last observed value and forecasted trend
  last_val <- tail(y, 1)
  fc_vals <- fc$mean
  
  if (scenario == "trend") {
    # Keep auto.arima forecast
    return(fc_vals)
    
  } else if (scenario == "constant") {
    # Assume it stays flat at last observed level
    return(rep(last_val, h))
    
  } else if (scenario == "reversal") {
    # Take the auto.arima trend but flip the deviation around last value
    return(last_val - (fc_vals - last_val))
  }
}

# --- Rolling CV errors for each scenario ---
cv_scenario <- function(scenario, h) {
  cv_err <- tsCV(
    mortgage_ts,
    forecastfunction = function(y, h) scenario_forecast(y, h, scenario = scenario),
    h = h
  )
  sqrt(colMeans(cv_err^2, na.rm = TRUE)) # RMSE for each horizon
}

# Compute RMSEs for each scenario
rmse_trend    <- cv_scenario("trend", h = 2)
rmse_constant <- cv_scenario("constant", h = 2)
rmse_reversal <- cv_scenario("reversal", h = 2)

# ---- Display results ----
cat("RMSE (Trend scenario)    h=1:", round(rmse_trend[1], 4), 
    " h=2:", round(rmse_trend[2], 4), "\n")
cat("RMSE (Constant scenario) h=1:", round(rmse_constant[1], 4), 
    " h=2:", round(rmse_constant[2], 4), "\n")
cat("RMSE (Reversal scenario) h=1:", round(rmse_reversal[1], 4), 
    " h=2:", round(rmse_reversal[2], 4), "\n")















# ============ Rolling MPC (30-quarter window, COVID removed, P as-is) ============
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(zoo)
})

# ---- Config ----
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
ROLL_WIN  <- 30L  # <-- 30 observations (quarters), centered window

# Column names in your sheet (exact)
COL_Q <- "Q"
COL_Y <- "Real income"    # already real
COL_C <- "Real Cons"      # already real
COL_T <- "Gov benefits"   # nominal -> deflate
COL_W <- "Ill wealth"     # nominal -> deflate
COL_P <- "Cons Deflator"  # deflator (USED AS-IS — NO RESCALE)

# ---- Load ----
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

# Parse quarter labels like "Mar-00" (fallback "Mar-2000")
d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# ---- Remove COVID period ----
COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")
d <- d %>% filter(date < COVID_START | date > COVID_END)

# ---- Build regression variables (deflator P used AS-IS) ----
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

# ---- Rolling OLS (centered 30q): store beta & OLS SE ----
n <- nrow(d)
rows <- vector("list", n - ROLL_WIN + 1L)

for (s in 1:(n - ROLL_WIN + 1L)) {
  e <- s + ROLL_WIN - 1L
  center <- s + (ROLL_WIN/2 - 1L)          # for 30q: s + 14
  sub <- d[s:e, , drop = FALSE]

  # skip windows with no variation or NAs
  if (!all(is.finite(sub$ratio_data)) || !all(is.finite(sub$W_over_den)) ||
      var(sub$W_over_den, na.rm = TRUE) <= 0) {
    rows[[s]] <- data.frame(Date = d$Date[center], beta = NA_real_, se = NA_real_)
    next
  }

  fit <- lm(ratio_data ~ W_over_den, data = sub)
  b   <- as.numeric(coef(fit)["W_over_den"])
  se  <- summary(fit)$coef["W_over_den", "Std. Error"]

  rows[[s]] <- data.frame(
    Date = d$Date[center],
    beta = ifelse(is.finite(b),  b,  NA_real_),
    se   = ifelse(is.finite(se), se, NA_real_)
  )
}

roll <- do.call(rbind, rows) %>%
  mutate(
    beta_cents = 100 * beta,
    lo = 100 * (beta - 1.96 * se),
    hi = 100 * (beta + 1.96 * se)
  ) %>%
  arrange(Date)

roll_line <- dplyr::filter(roll, is.finite(beta_cents))
roll_band <- dplyr::filter(roll, is.finite(lo) & is.finite(hi))

# ---- Symmetric y-limits around zero (always show 0 line) ----
vals <- c(roll_line$beta_cents, roll_band$lo, roll_band$hi, 0)
vals <- vals[is.finite(vals)]
rng  <- range(vals); pad <- 0.05 * diff(rng); if (!is.finite(pad)) pad <- 0.1
lim  <- max(abs(rng)) + pad
ylims <- c(-lim, lim)

# ---- Plot: rolling MPC with 95% CI band, zero line, border ----
p <- ggplot() +
  { if (nrow(roll_band) > 0)
      geom_ribbon(data = roll_band,
                  aes(x = Date, ymin = lo, ymax = hi),
                  fill = "#FB9A99", alpha = 0.35) } +
  geom_line(data = roll_line,
            aes(x = Date, y = beta_cents),
            colour = "#E31A1C", linewidth = 1.05, na.rm = TRUE) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.7) +
  scale_y_continuous("Cents",
                     limits = ylims,
                     breaks = pretty(ylims, n = 6),
                     expand = expansion(mult = c(0,0))) +
  labs(title = "Rolling 30-quarter MPC out of Wealth (OLS, 95% CI) — COVID removed",
       x = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        plot.background = element_blank())

print(p)












# ===================== Figure 1 — Predicted vs Observed (COVID removed) =====================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)     # as.yearqtr
})

# --- Config ---
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"

# Exact column names in your sheet
COL_Q <- "Q"               # e.g., "Mar-00"
COL_Y <- "Real income"     # already real
COL_C <- "Real Cons"       # already real
COL_T <- "Gov benefits"    # nominal -> deflate
COL_W <- "Ill wealth"      # nominal -> deflate
COL_P <- "Cons Deflator"   # deflator (use AS-IS; NO rescale)

COVID_START <- as.yearqtr("2020 Q1")
COVID_END   <- as.yearqtr("2021 Q4")

# helper for tidy y-limits
nice_limits <- function(vals, pad_mult = 0.04, digits = 3) {
  vals <- vals[is.finite(vals)]
  r <- range(vals, na.rm = TRUE)
  span <- max(r[2] - r[1], 1e-9)
  pad <- pad_mult * span
  lo <- floor((r[1] - pad) * 10^digits) / 10^digits
  hi <- ceiling((r[2] + pad) * 10^digits) / 10^digits
  c(lo, hi)
}

# --- Load & prep ---
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

d <- raw %>%
  transmute(
    Q        = .data[[COL_Q]],
    Y_real   = as.numeric(.data[[COL_Y]]),
    C_real   = as.numeric(.data[[COL_C]]),
    T_nom    = as.numeric(.data[[COL_T]]),
    W_nom    = as.numeric(.data[[COL_W]]),
    P        = as.numeric(.data[[COL_P]])  # used AS-IS (no /100)
  )

# parse quarters like "Mar-00" (fallback "Mar-2000")
d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# remove COVID period
d <- d %>% filter(date < COVID_START | date > COVID_END)

# deflate ONLY transfers & illiquid wealth; build regression vars
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

# --- Fit simple OLS and get Predicted (no break) ---
ols1 <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- as.numeric(predict(ols1, newdata = d))

ylims_f1 <- nice_limits(c(d$ratio_data, d$ratio_pred))

# --- Plot: Predicted (dotted, bigger dots) vs Observed data ---
fig1 <- ggplot(d, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Data",      linetype = "Data"),      linewidth = 1.1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted", linetype = "Predicted"),
            linewidth = 1.8, lineend = "round") +   # thicker -> larger dotted "dots"
  scale_colour_manual(
    values = c("Data" = "black", "Predicted" = "#2C7FB8"),
    breaks = c("Predicted", "Data"),
    labels = c("Predicted", "Observed data")
  ) +
  scale_linetype_manual(
    values = c("Data" = "solid", "Predicted" = "dotted"),
    breaks = c("Predicted", "Data"),
    labels = c("Predicted", "Observed data")
  ) +
  guides(colour = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL)) +
  scale_y_continuous(limits = ylims_f1, expand = expansion(mult = c(0,0))) +
  labs(title = "Figure 1. Predicted vs Observed consumption-to-income ratio (COVID removed)",
       y = "Ratio", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    plot.background = element_blank(),
    legend.position = "bottom"
  )

print(fig1)



















































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











