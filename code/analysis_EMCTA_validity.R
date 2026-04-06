# =============================================================================
# Convergent and Discriminant Validity: IDA × EMCTA-r
# Colombia (n = 415) and Spain (n = 622)
# =============================================================================
#
# Purpose:
#   Complementary analyses requested by EJPA: convergent and discriminant
#   validity of the IDA using the EMCTA-r (Brief Scale of Reasons to Continue
#   Working in Retirement) as an external measure.
#
# Analyses:
#   1. CFA of EMCTA-r by country (21 items, 7 factors)
#   2. CFA of IDA by country (13 items, 4 factors — with and without
#      correlated residuals)
#   3. Internal consistency of both instruments by country (α and ω)
#   4. Latent factor correlations IDA ↔ EMCTA-r by country
#      (joint 11-factor model)
#
# References:
#   - Souza, A.P. & França, L.H.F.P. (2020). EMCTA original (44 items).
#     Ciencias Psicológicas, 14(2), e-2256.
#   - Carvalho, F.C.P., França, L.H.F.P., & Gabardo-Martins, L.M.D. (2024).
#     EMCTA-r (21 items). Psicologia: Teoria e Prática, 26(3), ePTPPA15620.
#
# Software: R (>= 4.0), lavaan (>= 0.6-12), semTools, readxl, psych
# Estimator: WLSMV (ordinal items)
# License: CC-BY 4.0
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------

library(readxl)
library(lavaan)
library(semTools)
library(psych)

# Load data
data_path <- file.path("..", "02_datos", "COLOMBIA_ESPANHA_TOTAL.xlsx")
dat <- readxl::read_xlsx(data_path)

cat("=== Dataset ===\n")
cat("N =", nrow(dat), "\n")
print(table(dat$País))
cat("\n")

# Split by country
dat_col <- dat[dat$País == "Colômbia", ]
dat_esp <- dat[dat$País == "Espanha", ]
cat("Colombia n =", nrow(dat_col), " | Spain n =", nrow(dat_esp), "\n\n")

# ---- Item definitions ----

# EMCTA-r: 21 items, 7 factors (Carvalho, França & Gabardo-Martins, 2024)
items_emcta <- paste0("EMCTA", 1:21)

# IDA: 13 items, 4 factors
items_ida <- c("EDA1", "EDA3", "EDA4", "EDA5",    # ANT
               "EDA7", "EDA8", "EDA9",              # APO
               "EDA12", "EDA13", "EDA14",            # PONT
               "EDA17", "EDA18", "EDA19")            # CON

all_items <- c(items_emcta, items_ida)
stopifnot(all(all_items %in% names(dat)))

# Convert to ordered factors
for (item in all_items) {
  dat[[item]]     <- ordered(dat[[item]])
  dat_col[[item]] <- ordered(dat_col[[item]])
  dat_esp[[item]] <- ordered(dat_esp[[item]])
}

# ---- Item-to-factor mapping table (for Larissa's validation) ----

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("EMCTA-r ITEM-TO-FACTOR MAPPING\n")
cat("(Based on Carvalho, França & Gabardo-Martins, 2024, Table 1)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

emcta_mapping <- data.frame(
  Factor = c(
    rep("F1: Situação financeira", 3),
    rep("F2: Condições físicas", 3),
    rep("F3: Condições de trabalho", 3),
    rep("F4: Importância do trabalho", 3),
    rep("F5: Relacionamento no trabalho", 3),
    rep("F6: Relacionamento com a organização", 3),
    rep("F7: Desenvolvimento intelectual", 3)
  ),
  Item = paste0("EMCTA", 1:21),
  Content = c(
    # F1: Financial situation
    "Para sustentar meu padrão de vida",
    "Para manter os benefícios que recebo do trabalho",
    "Para poupar alguma renda para o futuro",
    # F2: Physical conditions
    "Porque me sinto saudável fisicamente",
    "Para manter minha memória em bom funcionamento",
    "Para me sentir produtivo",
    # F3: Working conditions
    "Porque tenho autonomia sobre o meu trabalho",
    "Porque tenho equilíbrio entre vida pessoal e profissional",
    "Por ter liberdade para determinar prioridades no trabalho",
    # F4: Importance of work
    "Porque quando convivo com pessoas no trabalho não penso em outros problemas",
    "Para continuar participando de treinamentos/capacitações",
    "Porque tenho mais interesse no trabalho do que em realizar outras atividades",
    # F5: Relationships at work
    "Para compartilhar a minha experiência com os outros",
    "Para ensinar algo aos mais jovens",
    "Para contribuir para futuras gerações",
    # F6: Relationship with organization
    "Porque convivo com diversas pessoas no meu trabalho",
    "Porque o ambiente de trabalho é agradável",
    "Porque me sinto descontraído quando convivo com as pessoas do trabalho",
    # F7: Intellectual development
    "Por ser um trabalho interessante",
    "Por ser um trabalho desafiador",
    "Porque tenho condições de utilizar meus conhecimentos"
  )
)

print(emcta_mapping, right = FALSE, row.names = FALSE)
cat("\n")


# ---- Helper: extract fit indices ----

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

# Helper: extract standardized loadings
print_loadings <- function(fit, label) {
  std <- standardizedSolution(fit)
  loadings <- std[std$op == "=~", ]
  cat(paste0("--- ", label, " ---\n"))
  print(loadings[, c("lhs", "rhs", "est.std", "se", "z", "pvalue")],
        row.names = FALSE, digits = 3)
  cat(sprintf("  Loading range: %.3f to %.3f (M = %.3f)\n\n",
              min(loadings$est.std), max(loadings$est.std),
              mean(loadings$est.std)))
}


# =============================================================================
# SECTION 1: CFA OF EMCTA-r BY COUNTRY
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: CFA OF EMCTA-r BY COUNTRY (21 items, 7 factors)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

model_emcta <- '
  SIT_FIN  =~ EMCTA1  + EMCTA2  + EMCTA3
  COND_FIS =~ EMCTA4  + EMCTA5  + EMCTA6
  COND_TRA =~ EMCTA7  + EMCTA8  + EMCTA9
  IMP_TRA  =~ EMCTA10 + EMCTA11 + EMCTA12
  REL_TRA  =~ EMCTA13 + EMCTA14 + EMCTA15
  REL_ORG  =~ EMCTA16 + EMCTA17 + EMCTA18
  DES_INT  =~ EMCTA19 + EMCTA20 + EMCTA21
'

# CFA Colombia
fit_emcta_col <- cfa(
  model     = model_emcta,
  data      = dat_col,
  ordered   = TRUE,
  estimator = "WLSMV"
)

# CFA Spain
fit_emcta_esp <- cfa(
  model     = model_emcta,
  data      = dat_esp,
  ordered   = TRUE,
  estimator = "WLSMV"
)

# Fit comparison table
emcta_fit_table <- rbind(
  extract_fit(fit_emcta_col, "EMCTA-r Colombia (n=415)"),
  extract_fit(fit_emcta_esp, "EMCTA-r Spain (n=622)")
)

cat("--- EMCTA-r: Fit indices by country ---\n")
print(emcta_fit_table, right = FALSE)
cat("\n")

# Standardized loadings
print_loadings(fit_emcta_col, "EMCTA-r Loadings: Colombia")
print_loadings(fit_emcta_esp, "EMCTA-r Loadings: Spain")


# =============================================================================
# SECTION 2: CFA OF IDA BY COUNTRY
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: CFA OF IDA BY COUNTRY (13 items, 4 factors)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Model A: Without correlated residuals (Brazil-Portugal original)
model_ida_a <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
'

# Model B: With correlated residuals (current manuscript)
model_ida_b <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
  EDA4  ~~ EDA5
  EDA12 ~~ EDA14
'

# Fit all four combinations
fit_ida_a_col <- cfa(model_ida_a, data = dat_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_a_esp <- cfa(model_ida_a, data = dat_esp, ordered = TRUE, estimator = "WLSMV")
fit_ida_b_col <- cfa(model_ida_b, data = dat_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_b_esp <- cfa(model_ida_b, data = dat_esp, ordered = TRUE, estimator = "WLSMV")

ida_fit_table <- rbind(
  extract_fit(fit_ida_a_col, "IDA Model A (no corr. res.) — Colombia"),
  extract_fit(fit_ida_a_esp, "IDA Model A (no corr. res.) — Spain"),
  extract_fit(fit_ida_b_col, "IDA Model B (corr. res.) — Colombia"),
  extract_fit(fit_ida_b_esp, "IDA Model B (corr. res.) — Spain")
)

cat("--- IDA: Fit indices by country and model ---\n")
print(ida_fit_table, right = FALSE)
cat("\n")

# Loadings for Model A (clean model)
print_loadings(fit_ida_a_col, "IDA Model A Loadings: Colombia")
print_loadings(fit_ida_a_esp, "IDA Model A Loadings: Spain")


# =============================================================================
# SECTION 3: INTERNAL CONSISTENCY (α AND ω) BY COUNTRY
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: INTERNAL CONSISTENCY BY COUNTRY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# --- 3a. EMCTA-r reliability ---

# ω from CFA models
cat("--- EMCTA-r: McDonald's omega from CFA ---\n\n")
cat("Colombia:\n")
rel_emcta_col <- semTools::reliability(fit_emcta_col)
print(round(rel_emcta_col, 3))
cat("\nSpain:\n")
rel_emcta_esp <- semTools::reliability(fit_emcta_esp)
print(round(rel_emcta_esp, 3))
cat("\n")

# α from raw data
compute_alpha <- function(data, items_list, label) {
  dat_num <- data
  for (item in items_list) {
    dat_num[[item]] <- as.numeric(data[[item]])
  }
  cat(paste0("--- ", label, " ---\n"))

  emcta_factors <- list(
    SIT_FIN  = paste0("EMCTA", 1:3),
    COND_FIS = paste0("EMCTA", 4:6),
    COND_TRA = paste0("EMCTA", 7:9),
    IMP_TRA  = paste0("EMCTA", 10:12),
    REL_TRA  = paste0("EMCTA", 13:15),
    REL_ORG  = paste0("EMCTA", 16:18),
    DES_INT  = paste0("EMCTA", 19:21)
  )

  for (fname in names(emcta_factors)) {
    fitems <- emcta_factors[[fname]]
    a <- psych::alpha(dat_num[, fitems], check.keys = FALSE)
    cat(sprintf("  %s (%s): alpha = %.3f\n",
                fname, paste(fitems, collapse = ", "),
                a$total$raw_alpha))
  }
  cat("\n")
}

compute_alpha(dat_col, items_emcta, "EMCTA-r Cronbach's alpha: Colombia (n=415)")
compute_alpha(dat_esp, items_emcta, "EMCTA-r Cronbach's alpha: Spain (n=622)")

# --- 3b. IDA reliability ---

cat("--- IDA: McDonald's omega from CFA (Model A, no corr. res.) ---\n\n")
cat("Colombia:\n")
rel_ida_col <- semTools::reliability(fit_ida_a_col)
print(round(rel_ida_col, 3))
cat("\nSpain:\n")
rel_ida_esp <- semTools::reliability(fit_ida_a_esp)
print(round(rel_ida_esp, 3))
cat("\n")

compute_alpha_ida <- function(data, label) {
  dat_num <- data
  for (item in items_ida) {
    dat_num[[item]] <- as.numeric(data[[item]])
  }
  cat(paste0("--- ", label, " ---\n"))

  ida_factors <- list(
    ANT  = c("EDA1", "EDA3", "EDA4", "EDA5"),
    APO  = c("EDA7", "EDA8", "EDA9"),
    PONT = c("EDA12", "EDA13", "EDA14"),
    CON  = c("EDA17", "EDA18", "EDA19")
  )

  for (fname in names(ida_factors)) {
    fitems <- ida_factors[[fname]]
    a <- psych::alpha(dat_num[, fitems], check.keys = FALSE)
    cat(sprintf("  %s (%s): alpha = %.3f\n",
                fname, paste(fitems, collapse = ", "),
                a$total$raw_alpha))
  }
  cat("\n")
}

compute_alpha_ida(dat_col, "IDA Cronbach's alpha: Colombia (n=415)")
compute_alpha_ida(dat_esp, "IDA Cronbach's alpha: Spain (n=622)")


# =============================================================================
# SECTION 4: LATENT CORRELATIONS IDA ↔ EMCTA-r BY COUNTRY
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: LATENT CORRELATIONS IDA x EMCTA-r (11-factor model)\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Joint model: 4 IDA factors + 7 EMCTA-r factors = 11 factors
model_joint <- '
  # IDA (4 factors)
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19

  # EMCTA-r (7 factors)
  SIT_FIN  =~ EMCTA1  + EMCTA2  + EMCTA3
  COND_FIS =~ EMCTA4  + EMCTA5  + EMCTA6
  COND_TRA =~ EMCTA7  + EMCTA8  + EMCTA9
  IMP_TRA  =~ EMCTA10 + EMCTA11 + EMCTA12
  REL_TRA  =~ EMCTA13 + EMCTA14 + EMCTA15
  REL_ORG  =~ EMCTA16 + EMCTA17 + EMCTA18
  DES_INT  =~ EMCTA19 + EMCTA20 + EMCTA21
'

# Fit joint model by country
cat("Fitting joint 11-factor model...\n\n")

fit_joint_col <- cfa(
  model     = model_joint,
  data      = dat_col,
  ordered   = TRUE,
  estimator = "WLSMV"
)

fit_joint_esp <- cfa(
  model     = model_joint,
  data      = dat_esp,
  ordered   = TRUE,
  estimator = "WLSMV"
)

# Fit indices
joint_fit_table <- rbind(
  extract_fit(fit_joint_col, "Joint 11-factor — Colombia"),
  extract_fit(fit_joint_esp, "Joint 11-factor — Spain")
)

cat("--- Joint model: Fit indices ---\n")
print(joint_fit_table, right = FALSE)
cat("\n")

# ---- Extract IDA ↔ EMCTA correlations ----

extract_cross_correlations <- function(fit, label) {
  std <- standardizedSolution(fit)

  ida_factors   <- c("ANT", "APO", "PONT", "CON")
  emcta_factors <- c("SIT_FIN", "COND_FIS", "COND_TRA", "IMP_TRA",
                      "REL_TRA", "REL_ORG", "DES_INT")

  # Factor correlations (~~, lhs != rhs)
  cors <- std[std$op == "~~" & std$lhs != std$rhs, ]

  # Filter to cross-instrument correlations only
  cross <- cors[
    (cors$lhs %in% ida_factors & cors$rhs %in% emcta_factors) |
    (cors$lhs %in% emcta_factors & cors$rhs %in% ida_factors),
  ]

  cat(paste0("--- ", label, " ---\n"))

  # Build correlation matrix
  cor_matrix <- matrix(NA, nrow = length(ida_factors),
                        ncol = length(emcta_factors))
  rownames(cor_matrix) <- ida_factors
  colnames(cor_matrix) <- emcta_factors

  sig_matrix <- matrix("", nrow = length(ida_factors),
                         ncol = length(emcta_factors))
  rownames(sig_matrix) <- ida_factors
  colnames(sig_matrix) <- emcta_factors

  for (i in seq_len(nrow(cross))) {
    lhs <- cross$lhs[i]
    rhs <- cross$rhs[i]
    r   <- cross$est.std[i]
    p   <- cross$pvalue[i]

    # Determine which is IDA and which is EMCTA
    if (lhs %in% ida_factors & rhs %in% emcta_factors) {
      row_name <- lhs
      col_name <- rhs
    } else {
      row_name <- rhs
      col_name <- lhs
    }

    cor_matrix[row_name, col_name] <- r
    sig <- ifelse(p < 0.001, "***",
           ifelse(p < 0.01,  "**",
           ifelse(p < 0.05,  "*", "ns")))
    sig_matrix[row_name, col_name] <- sig
  }

  # Print formatted table
  cat("\nLatent correlations (rows = IDA, cols = EMCTA-r):\n\n")
  formatted <- matrix(
    paste0(sprintf("%.3f", cor_matrix), sig_matrix),
    nrow = nrow(cor_matrix),
    dimnames = dimnames(cor_matrix)
  )
  formatted[is.na(cor_matrix)] <- "  —  "
  print(noquote(formatted))
  cat("\nNote: * p < .05, ** p < .01, *** p < .001, ns = not significant\n\n")

  # Return raw data for further use
  invisible(list(correlations = cor_matrix, significance = sig_matrix,
                 raw = cross))
}

cors_col <- extract_cross_correlations(fit_joint_col,
              "Latent correlations IDA × EMCTA-r: COLOMBIA (n=415)")
cors_esp <- extract_cross_correlations(fit_joint_esp,
              "Latent correlations IDA × EMCTA-r: SPAIN (n=622)")

# ---- Also print IDA inter-factor correlations by country ----

cat("--- IDA inter-factor correlations by country ---\n\n")

print_ida_cors <- function(fit, label) {
  std <- standardizedSolution(fit)
  ida_f <- c("ANT", "APO", "PONT", "CON")
  cors <- std[std$op == "~~" & std$lhs != std$rhs &
              std$lhs %in% ida_f & std$rhs %in% ida_f, ]
  cat(paste0(label, ":\n"))
  print(cors[, c("lhs", "rhs", "est.std", "pvalue")],
        row.names = FALSE, digits = 3)
  cat("\n")
}

print_ida_cors(fit_joint_col, "Colombia")
print_ida_cors(fit_joint_esp, "Spain")


# =============================================================================
# SECTION 5: SUMMARY — HYPOTHESES vs. RESULTS
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: SUMMARY OF HYPOTHESES vs. RESULTS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("H1a/H1b: IDA has 4 factors and 13 items in each country\n")
cat("  → See Section 2 (CFA IDA by country)\n\n")

cat("H2a/H2b: IDA factors show adequate internal consistency\n")
cat("  → See Section 3 (alpha and omega by country)\n\n")

cat("H3a/H3b: CON and PONT positively correlate with EMCTA-r factors\n")
cat("  → Colombia:\n")
if (exists("cors_col")) {
  cat("    CON  mean r =", round(mean(cors_col$correlations["CON", ], na.rm = TRUE), 3), "\n")
  cat("    PONT mean r =", round(mean(cors_col$correlations["PONT", ], na.rm = TRUE), 3), "\n")
}
cat("  → Spain:\n")
if (exists("cors_esp")) {
  cat("    CON  mean r =", round(mean(cors_esp$correlations["CON", ], na.rm = TRUE), 3), "\n")
  cat("    PONT mean r =", round(mean(cors_esp$correlations["PONT", ], na.rm = TRUE), 3), "\n")
}
cat("\n")

cat("H4a/H4b: ANT and APO negatively correlate with EMCTA-r factors\n")
cat("  → Colombia:\n")
if (exists("cors_col")) {
  cat("    ANT  mean r =", round(mean(cors_col$correlations["ANT", ], na.rm = TRUE), 3), "\n")
  cat("    APO  mean r =", round(mean(cors_col$correlations["APO", ], na.rm = TRUE), 3), "\n")
}
cat("  → Spain:\n")
if (exists("cors_esp")) {
  cat("    ANT  mean r =", round(mean(cors_esp$correlations["ANT", ], na.rm = TRUE), 3), "\n")
  cat("    APO  mean r =", round(mean(cors_esp$correlations["APO", ], na.rm = TRUE), 3), "\n")
}
cat("\n")


# =============================================================================
# SECTION 6: SESSION INFO
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SESSION INFO\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

sessionInfo()

cat("\n\n--- Script completed successfully ---\n")
