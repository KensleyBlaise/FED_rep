# ================================================================
# Two panels: Full sample vs No-COVID (no gridExtra needed)
# ================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(zoo)
  library(sandwich)
  library(lmtest)
})

XLSX_FILE <- "FED_rep.xlsx"
SHEET     <- "Sheet29"
BREAK_QTR <- zoo::as.yearqtr("2012 Q1")

# ---------------- LOAD -----------------
raw <- read_excel(XLSX_FILE, sheet = SHEET, skip = 0)
names(raw) <- trimws(names(raw))

d <- raw %>%
  transmute(
    Q       = Q,
    Y_real  = as.numeric(`Real income`),
    C_real  = as.numeric(`Real Cons`),
    T_nom   = as.numeric(`Gov benefits`),
    W_nom   = as.numeric(`Ill wealth`),
    P       = as.numeric(`Cons Deflator`)
  )

d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%y"))
if (any(is.na(d$date))) d$date <- suppressWarnings(zoo::as.yearqtr(d$Q, format = "%b-%Y"))
d$Date <- as.Date(d$date)

if (mean(d$P, na.rm = TRUE) > 5) d$P <- d$P / 100

# Deflate transfers + wealth
d <- d %>%
  mutate(
    T_real = T_nom / P,
    W_real = W_nom / P,
    den = Y_real - T_real,
    num = C_real - T_real,
    ratio_data = num / den,
    W_over_den = W_real / den
  ) %>%
  filter(is.finite(ratio_data), is.finite(W_over_den)) %>%
  arrange(Date)

# ---------------- FUNCTION TO BUILD A PLOT -----------------
make_fig1 <- function(data, title){
  ols1 <- lm(ratio_data ~ W_over_den, data = data)
  data$ratio_pred <- predict(ols1, newdata = data)

  data <- data %>%
    mutate(post = as.integer(date >= BREAK_QTR),
           post_W = post * W_over_den)
  ols2 <- lm(ratio_data ~ W_over_den + post + post_W, data = data)
  data$ratio_pred_break <- predict(ols2, newdata = data)

  ggplot(data, aes(x = Date)) +
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
}

# ---------------- BUILD TWO DATASETS -----------------
d_full    <- d
d_nocovid <- d %>% filter(date < zoo::as.yearqtr("2020 Q1") | date > zoo::as.yearqtr("2021 Q4"))

# ---------------- DRAW SIDE BY SIDE -----------------
# Open a 1x2 plotting panel
par(mfrow = c(1, 2))

print(make_fig1(d_full, "Full Sample"))
print(make_fig1(d_nocovid, "Excluding COVID (2020–2021)"))
