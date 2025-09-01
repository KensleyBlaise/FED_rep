# ================================================================
# FED_rep.xlsx (Sheet29) -> Figures 1 & 1b (Full sample vs No-COVID)
# Y and C already real; T and W deflated by Cons Deflator
# ================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)
  library(gridExtra)   # to arrange plots side by side
  library(slider)
  library(sandwich)
  library(lmtest)
})

# ---------------- CONFIG ----------------
XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
BREAK_QTR <- zoo::as.yearqtr("2012 Q1")

# Column names
COL_Q <- "Q"
COL_Y <- "Real income"
COL_C <- "Real Cons"
COL_T <- "Gov benefits"
COL_W <- "Ill wealth"
COL_P <- "Cons Deflator"

# ---------------- LOAD -----------------
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

d <- raw %>%
  transmute(
    Q       = .data[[COL_Q]],
    Y_real  = as.numeric(.data[[COL_Y]]),
    C_real  = as.numeric(.data[[COL_C]]),
    T_nom   = as.numeric(.data[[COL_T]]),
    W_nom   = as.numeric(.data[[COL_W]]),
    P       = as.numeric(.data[[COL_P]])
  )

# Parse quarters
d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

# Normalize deflator if needed
if (mean(d$P, na.rm = TRUE) > 5) {
  message("Rescaling deflator by /100 (100=base scale detected).")
  d$P <- d$P / 100
}

# Deflate only T and W
d <- d %>%
  mutate(
    T_real = T_nom / P,
    W_real = W_nom / P,
    den        = Y_real - T_real,
    num        = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(W_over_den)) %>%
  arrange(Date)

# ---------------- FIGURE 1 (FULL SAMPLE) ----------------
ols1_full <- lm(ratio_data ~ W_over_den, data = d)
d$ratio_pred <- predict(ols1_full, newdata = d)

d_full <- d %>%
  mutate(post = as.integer(date >= BREAK_QTR),
         post_W = post * W_over_den)
ols2_full <- lm(ratio_data ~ W_over_den + post + post_W, data = d_full)
d_full$ratio_pred_break <- predict(ols2_full, newdata = d_full)

fig1 <- ggplot(d_full, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Data"), linewidth = 1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted"), linewidth = 1, linetype = "dotted") +
  geom_line(aes(y = ratio_pred_break, colour = "Predicted with Trend Break"),
            linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c("Data" = "black",
                                 "Predicted" = "#2C7FB8",
                                 "Predicted with Trend Break" = "#D95F02")) +
  labs(title = "Full Sample", y = "Ratio", x = NULL, colour = "") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ---------------- FIGURE 1b (NO COVID: remove 2020Q1–2021Q4) ----------------
d_nocovid <- d %>% filter(date < zoo::as.yearqtr("2020 Q1") | date > zoo::as.yearqtr("2021 Q4"))

ols1_nc <- lm(ratio_data ~ W_over_den, data = d_nocovid)
d_nocovid$ratio_pred <- predict(ols1_nc, newdata = d_nocovid)

d_nc <- d_nocovid %>%
  mutate(post = as.integer(date >= BREAK_QTR),
         post_W = post * W_over_den)
ols2_nc <- lm(ratio_data ~ W_over_den + post + post_W, data = d_nc)
d_nc$ratio_pred_break <- predict(ols2_nc, newdata = d_nc)

fig1b <- ggplot(d_nc, aes(x = Date)) +
  geom_line(aes(y = ratio_data, colour = "Data"), linewidth = 1) +
  geom_line(aes(y = ratio_pred, colour = "Predicted"), linewidth = 1, linetype = "dotted") +
  geom_line(aes(y = ratio_pred_break, colour = "Predicted with Trend Break"),
            linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c("Data" = "black",
                                 "Predicted" = "#2C7FB8",
                                 "Predicted with Trend Break" = "#D95F02")) +
  labs(title = "Excluding COVID (2020–2021)", y = "Ratio", x = NULL, colour = "") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# ---------------- SHOW SIDE BY SIDE ----------------
gridExtra::grid.arrange(fig1, fig1b, ncol = 2)
