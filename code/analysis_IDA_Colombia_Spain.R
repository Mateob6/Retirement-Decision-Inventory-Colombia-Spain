# =============================================================================
# Psychometric Validation of the Retirement Decision Inventory (IDA)
# in Colombia and Spain: Confirmatory Factor Analysis,
# Measurement Invariance, and Reliability
# =============================================================================
#
# Authors: Díaz-Bambula, F., Londoño-Moreno, A.M., Belalcázar, M.,
#          Rubio, L., Dumitrache, C.G., Gabardo-Martins, L.M.,
#          & França, L.H.F.P.
#
# Description:
#   This script replicates all psychometric analyses reported in the
#   manuscript submitted to the European Journal of Psychological
#   Assessment (EJPA). Analyses include:
#     1. CFA with three competing models (total sample, N = 1037)
#     2. Standardized factor loadings and factor correlations (best model)
#     3. Measurement invariance across countries (Colombia vs. Spain)
#     4. Internal consistency (Cronbach's alpha and McDonald's omega)
#
# Data:
#   COLOMBIA_ESPANHA_TOTAL.xlsx — N = 1037 (Colombia n = 415, Spain n = 622)
#   Items: EDA1–EDA20 (Likert scale, treated as ordinal)
#   Grouping variable: "País" ("Colômbia" / "Espanha")
#
# Software:
#   R (>= 4.0), lavaan (>= 0.6-12), semTools, readxl, psych
#   Estimator: WLSMV (robust for ordinal data)
#
# License: CC-BY 4.0
# =============================================================================


# ---- 0. SETUP: PACKAGES AND DATA -------------------------------------------

# Required packages
library(readxl)     # Read Excel files
library(lavaan)     # Confirmatory factor analysis (CFA)
library(semTools)   # Measurement invariance and composite reliability
library(psych)      # Cronbach's alpha and descriptive statistics

# Load data
# NOTE: Set working directory to the repository root before running,
#       or adjust the path below.
data_path <- file.path("..", "data", "COLOMBIA_ESPANHA_TOTAL.xlsx")
dat <- readxl::read_xlsx(data_path)

# Verify dimensions
cat("=== Dataset dimensions ===\n")
cat("N =", nrow(dat), " | Variables =", ncol(dat), "\n\n")

# Frequencies by country
cat("=== Distribution by country ===\n")
print(table(dat$País))
cat("\n")

# IDA items (13 items from the final model)
items_all <- paste0("EDA", 1:20)
items_final <- c("EDA1", "EDA3", "EDA4", "EDA5",   # ANT
                  "EDA7", "EDA8", "EDA9",             # APO
                  "EDA12", "EDA13", "EDA14",           # PONT
                  "EDA17", "EDA18", "EDA19")           # CON

# Verify that items exist in the data
stopifnot(all(items_final %in% names(dat)))

# Convert items to ordered factors (required by lavaan with WLSMV)
for (item in items_final) {
  dat[[item]] <- ordered(dat[[item]])
}

cat("=== Response levels (example: EDA1) ===\n")
print(levels(dat$EDA1))
cat("\n")


# ---- 1. CFA: THREE COMPETING MODELS (TOTAL SAMPLE, N = 1037) ---------------
# Confirmatory factor analysis: three competing models
# WLSMV estimator with ordinal items

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: CONFIRMATORY FACTOR ANALYSIS — COMPETING MODELS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- Model 1: Two correlated factors ---
#   APOS (retiring) = ANT + APO items
#   CONT (continuing) = PONT + CON items
model_2f <- '
  APOS =~ EDA1 + EDA3 + EDA4 + EDA5 + EDA7 + EDA8 + EDA9
  CONT =~ EDA12 + EDA13 + EDA14 + EDA17 + EDA18 + EDA19
'

fit_2f <- lavaan::cfa(
  model    = model_2f,
  data     = dat,
  ordered  = TRUE,
  estimator = "WLSMV"
)

cat("--- Model 1: Two correlated factors ---\n")
summary(fit_2f, fit.measures = TRUE, standardized = TRUE)
cat("\n")

# --- Model 2: Four correlated factors (no correlated residuals) ---
model_4f <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
'

fit_4f <- lavaan::cfa(
  model    = model_4f,
  data     = dat,
  ordered  = TRUE,
  estimator = "WLSMV"
)

cat("--- Model 2: Four correlated factors ---\n")
summary(fit_4f, fit.measures = TRUE, standardized = TRUE)
cat("\n")

# --- Model 3: Four correlated factors + correlated residuals ---
# Correlated residuals based on modification indices:
#   EDA4 ~~ EDA5 (both from ANT, similar content)
#   EDA12 ~~ EDA14 (both from PONT, similar content)
model_4f_cr <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19

  # Correlated residuals
  EDA4  ~~ EDA5
  EDA12 ~~ EDA14
'

fit_4f_cr <- lavaan::cfa(
  model    = model_4f_cr,
  data     = dat,
  ordered  = TRUE,
  estimator = "WLSMV"
)

cat("--- Model 3: Four correlated factors + correlated residuals ---\n")
summary(fit_4f_cr, fit.measures = TRUE, standardized = TRUE)
cat("\n")


# ---- 1b. FIT INDICES COMPARISON TABLE --------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("FIT INDICES COMPARISON TABLE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

extract_fit <- function(fit_object, model_name) {
  fm <- fitMeasures(fit_object, c(
    "chisq.scaled", "df.scaled", "pvalue.scaled",
    "cfi.scaled", "tli.scaled",
    "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "srmr"
  ))
  data.frame(
    Model       = model_name,
    chi_sq      = round(fm["chisq.scaled"], 2),
    df          = fm["df.scaled"],
    p           = round(fm["pvalue.scaled"], 4),
    CFI         = round(fm["cfi.scaled"], 3),
    TLI         = round(fm["tli.scaled"], 3),
    RMSEA       = round(fm["rmsea.scaled"], 3),
    RMSEA_lower = round(fm["rmsea.ci.lower.scaled"], 3),
    RMSEA_upper = round(fm["rmsea.ci.upper.scaled"], 3),
    SRMR        = round(fm["srmr"], 3),
    row.names   = NULL
  )
}

fit_table <- rbind(
  extract_fit(fit_2f,    "(i) Two correlated factors"),
  extract_fit(fit_4f,    "(ii) Four correlated factors"),
  extract_fit(fit_4f_cr, "(iii) Four factors + corr. residuals")
)

print(fit_table, right = FALSE)
cat("\n")


# ---- 2. STANDARDIZED FACTOR LOADINGS (MODEL 3) -----------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: STANDARDIZED FACTOR LOADINGS (MODEL 3)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Extract standardized loadings
std_loadings <- standardizedSolution(fit_4f_cr)
loadings_only <- std_loadings[std_loadings$op == "=~", ]

cat("--- Standardized loadings (Std.all) ---\n")
print(loadings_only[, c("lhs", "rhs", "est.std", "se", "z", "pvalue",
                         "ci.lower", "ci.upper")],
      row.names = FALSE, digits = 3)
cat("\n")

cat("Loading range:",
    round(min(loadings_only$est.std), 2), "to",
    round(max(loadings_only$est.std), 2), "\n")
cat("Mean loading:",
    round(mean(loadings_only$est.std), 2), "\n\n")


# ---- 3. FACTOR CORRELATIONS (MODEL 3) --------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: FACTOR CORRELATIONS (MODEL 3)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

factor_cors <- std_loadings[std_loadings$op == "~~" &
                              std_loadings$lhs != std_loadings$rhs &
                              std_loadings$lhs %in% c("ANT", "APO", "PONT", "CON") &
                              std_loadings$rhs %in% c("ANT", "APO", "PONT", "CON"), ]

cat("--- Factor correlations (Std.all) ---\n")
print(factor_cors[, c("lhs", "rhs", "est.std", "se", "z", "pvalue")],
      row.names = FALSE, digits = 3)
cat("\n")


# ---- 4. MEASUREMENT INVARIANCE (MGCFA) ACROSS COUNTRIES --------------------
# Multigroup measurement invariance between Colombia and Spain
# Using Model 3 (4 factors + correlated residuals)

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: MEASUREMENT INVARIANCE ACROSS COUNTRIES\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- 4a. Configural invariance ---
# Same factorial structure in both groups
fit_configural <- lavaan::cfa(
  model     = model_4f_cr,
  data      = dat,
  group     = "País",
  ordered   = TRUE,
  estimator = "WLSMV"
)

cat("--- Configural invariance ---\n")
summary(fit_configural, fit.measures = TRUE, standardized = TRUE)
cat("\n")

# --- 4b. Metric (weak) invariance ---
# Equal factor loadings across groups
fit_metric <- lavaan::cfa(
  model       = model_4f_cr,
  data        = dat,
  group       = "País",
  ordered     = TRUE,
  estimator   = "WLSMV",
  group.equal = c("loadings")
)

cat("--- Metric invariance ---\n")
summary(fit_metric, fit.measures = TRUE, standardized = TRUE)
cat("\n")

# --- 4c. Scalar (strong) invariance ---
# Equal factor loadings and thresholds across groups
fit_scalar <- lavaan::cfa(
  model       = model_4f_cr,
  data        = dat,
  group       = "País",
  ordered     = TRUE,
  estimator   = "WLSMV",
  group.equal = c("loadings", "thresholds")
)

cat("--- Scalar invariance ---\n")
summary(fit_scalar, fit.measures = TRUE, standardized = TRUE)
cat("\n")

# --- 4d. Invariance comparison table ---

cat("--- Invariance model comparison ---\n\n")

# Robust chi-square difference test
cat(">> Metric vs. Configural:\n")
print(lavTestLRT(fit_configural, fit_metric))
cat("\n")

cat(">> Scalar vs. Metric:\n")
print(lavTestLRT(fit_metric, fit_scalar))
cat("\n")

# Fit indices for invariance models
inv_table <- rbind(
  extract_fit(fit_configural, "Configural"),
  extract_fit(fit_metric,     "Metric"),
  extract_fit(fit_scalar,     "Scalar")
)

cat("--- Fit indices for invariance models ---\n")
print(inv_table, right = FALSE)
cat("\n")

# Compute delta CFI
cfi_vals <- inv_table$CFI
cat("Delta CFI (Metric - Configural):", round(cfi_vals[1] - cfi_vals[2], 3), "\n")
cat("Delta CFI (Scalar - Metric):",     round(cfi_vals[2] - cfi_vals[3], 3), "\n")
cat("Criterion: Delta CFI < .010 supports invariance (Cheung & Rensvold, 2002)\n\n")


# ---- 5. INTERNAL CONSISTENCY (RELIABILITY) ----------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: INTERNAL CONSISTENCY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- 5a. Reliability from the CFA model (semTools) ---
# McDonald's omega from the CFA model
cat("--- Reliability from CFA Model 3 (total sample) ---\n")
rel_total <- semTools::reliability(fit_4f_cr)
print(rel_total)
cat("\n")

# --- 5b. Cronbach's alpha by factor (total sample) ---
cat("--- Cronbach's alpha by factor (total sample, N = 1037) ---\n\n")

# Convert items to numeric for psych::alpha
dat_num <- dat
for (item in items_final) {
  dat_num[[item]] <- as.numeric(dat[[item]])
}

alpha_ANT  <- psych::alpha(dat_num[, c("EDA1", "EDA3", "EDA4", "EDA5")])
alpha_APO  <- psych::alpha(dat_num[, c("EDA7", "EDA8", "EDA9")])
alpha_PONT <- psych::alpha(dat_num[, c("EDA12", "EDA13", "EDA14")])
alpha_CON  <- psych::alpha(dat_num[, c("EDA17", "EDA18", "EDA19")])
alpha_TOTAL <- psych::alpha(dat_num[, items_final])

cat("ANT  (EDA1, EDA3, EDA4, EDA5): alpha =",
    round(alpha_ANT$total$raw_alpha, 3), "\n")
cat("APO  (EDA7, EDA8, EDA9):       alpha =",
    round(alpha_APO$total$raw_alpha, 3), "\n")
cat("PONT (EDA12, EDA13, EDA14):    alpha =",
    round(alpha_PONT$total$raw_alpha, 3), "\n")
cat("CON  (EDA17, EDA18, EDA19):    alpha =",
    round(alpha_CON$total$raw_alpha, 3), "\n")
cat("Total scale (13 items):        alpha =",
    round(alpha_TOTAL$total$raw_alpha, 3), "\n\n")

# --- 5c. Reliability by country ---

compute_alpha_by_country <- function(data, country_name, items_list) {
  sub <- data[data$País == country_name, ]
  sub_num <- sub
  for (item in items_final) {
    sub_num[[item]] <- as.numeric(sub[[item]])
  }
  cat(paste0("  ", country_name, " (n = ", nrow(sub), "):\n"))

  a_ANT  <- psych::alpha(sub_num[, c("EDA1", "EDA3", "EDA4", "EDA5")],
                          check.keys = FALSE)
  a_APO  <- psych::alpha(sub_num[, c("EDA7", "EDA8", "EDA9")],
                          check.keys = FALSE)
  a_PONT <- psych::alpha(sub_num[, c("EDA12", "EDA13", "EDA14")],
                          check.keys = FALSE)
  a_CON  <- psych::alpha(sub_num[, c("EDA17", "EDA18", "EDA19")],
                          check.keys = FALSE)

  cat("    ANT  alpha =", round(a_ANT$total$raw_alpha, 3), "\n")
  cat("    APO  alpha =", round(a_APO$total$raw_alpha, 3), "\n")
  cat("    PONT alpha =", round(a_PONT$total$raw_alpha, 3), "\n")
  cat("    CON  alpha =", round(a_CON$total$raw_alpha, 3), "\n")
}

cat("--- Cronbach's alpha by country ---\n\n")
compute_alpha_by_country(dat, "Colômbia", items_final)
cat("\n")
compute_alpha_by_country(dat, "Espanha", items_final)
cat("\n")

# --- 5d. McDonald's omega by country (from MGCFA) ---
cat("--- McDonald's omega by country (from MGCFA configural model) ---\n\n")

cat("  Colombia:\n")
rel_configural <- semTools::reliability(fit_configural)
print(rel_configural)
cat("\n")


# ---- 6. MODIFICATION INDICES (MODEL 2, FOR REFERENCE) ----------------------
# Modification indices from the four-factor model without correlated
# residuals, to justify adding correlated residuals in Model 3

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 6: MODIFICATION INDICES (MODEL 2)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

mi_4f <- modificationindices(fit_4f, sort. = TRUE, minimum.value = 10)
cat("--- Top modification indices (MI > 10) ---\n")
print(head(mi_4f, 20), row.names = FALSE)
cat("\n")


# ---- 7. DESCRIPTIVE STATISTICS ----------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 7: DESCRIPTIVE STATISTICS (13 ITEMS)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Use numeric data
desc_stats <- psych::describe(dat_num[, items_final])
print(desc_stats[, c("n", "mean", "sd", "median", "min", "max",
                      "skew", "kurtosis")],
      digits = 2)
cat("\n")

# Descriptives by country
cat("--- Descriptives: Colombia ---\n")
desc_col <- psych::describe(
  dat_num[dat_num$País == "Colômbia", items_final]
)
print(desc_col[, c("n", "mean", "sd", "median", "skew", "kurtosis")],
      digits = 2)
cat("\n")

cat("--- Descriptives: Spain ---\n")
desc_esp <- psych::describe(
  dat_num[dat_num$País == "Espanha", items_final]
)
print(desc_esp[, c("n", "mean", "sd", "median", "skew", "kurtosis")],
      digits = 2)
cat("\n")


# ---- 8. SESSION INFO --------------------------------------------------------

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SESSION INFO\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

sessionInfo()

cat("\n\n--- Script completed successfully ---\n")
