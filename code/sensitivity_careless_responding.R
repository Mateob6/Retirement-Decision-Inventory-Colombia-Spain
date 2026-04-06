# =============================================================================
# Sensitivity Analysis: Careless Responding Detection and Filtered Re-analysis
# =============================================================================
#
# Purpose:
#   Detect and exclude careless responders (straight-lining) in both countries,
#   then re-run CFA and validity analyses to assess robustness of results.
#
# Criteria for exclusion (applied equally to both countries):
#   - Longstring >= 20 on EMCTA items (20+ consecutive identical responses
#     out of 21 items = straight-lining)
#   - Longstring >= 10 on IDA items (10+ consecutive identical responses
#     out of 13 items)
#   - Combined: flagged on EITHER instrument
#
# References:
#   - Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in
#     survey data. Psychological Methods, 17(3), 437-455.
#   - Curran, P. G. (2016). Methods for the detection of carelessly invalid
#     responses in survey data. Journal of Experimental Social Psychology, 66,
#     4-19.
#
# License: CC-BY 4.0
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------

library(readxl)
library(lavaan)
library(semTools)
library(psych)

data_path <- file.path("..", "02_datos", "COLOMBIA_ESPANHA_TOTAL.xlsx")
dat <- readxl::read_xlsx(data_path)
dat$id <- 1:nrow(dat)

items_emcta <- paste0("EMCTA", 1:21)
items_ida <- c("EDA1", "EDA3", "EDA4", "EDA5",
               "EDA7", "EDA8", "EDA9",
               "EDA12", "EDA13", "EDA14",
               "EDA17", "EDA18", "EDA19")

# Convert to numeric for diagnostics
dat_num <- dat
for (item in c(items_emcta, items_ida)) {
  dat_num[[item]] <- as.numeric(dat[[item]])
}


# =============================================================================
# SECTION 1: CARELESS RESPONDING DETECTION
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 1: CARELESS RESPONDING DETECTION\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# ---- Longstring function ----
longstring <- function(row) {
  r <- as.numeric(row)
  r <- r[!is.na(r)]
  if (length(r) == 0) return(0)
  max(rle(r)$lengths)
}

# ---- Compute indicators ----
dat_num$ls_emcta <- apply(dat_num[, items_emcta], 1, longstring)
dat_num$ls_ida   <- apply(dat_num[, items_ida], 1, longstring)
dat_num$sd_emcta <- apply(dat_num[, items_emcta], 1, sd, na.rm = TRUE)
dat_num$sd_ida   <- apply(dat_num[, items_ida], 1, sd, na.rm = TRUE)

# ---- Flagging criteria ----
# EMCTA: 20+ of 21 items identical = straight-lining
# IDA: 10+ of 13 items identical = straight-lining
dat_num$flag_emcta <- dat_num$ls_emcta >= 20
dat_num$flag_ida   <- dat_num$ls_ida >= 10
dat_num$flag_any   <- dat_num$flag_emcta | dat_num$flag_ida

# ---- Report by country ----
for (pais in c("Colômbia", "Espanha")) {
  sub <- dat_num[dat_num$País == pais, ]
  n <- nrow(sub)
  label <- ifelse(pais == "Colômbia", "COLOMBIA", "SPAIN")

  cat(sprintf("--- %s (n = %d) ---\n\n", label, n))

  # EMCTA longstring distribution
  cat("  EMCTA longstring distribution:\n")
  cat(sprintf("    Mean = %.1f, Median = %.0f, Max = %d\n",
              mean(sub$ls_emcta), median(sub$ls_emcta), max(sub$ls_emcta)))
  cat(sprintf("    >= 15: %d (%.1f%%)\n", sum(sub$ls_emcta >= 15),
              100 * mean(sub$ls_emcta >= 15)))
  cat(sprintf("    >= 20: %d (%.1f%%)\n", sum(sub$ls_emcta >= 20),
              100 * mean(sub$ls_emcta >= 20)))
  cat(sprintf("    = 21 (all identical): %d (%.1f%%)\n\n",
              sum(sub$ls_emcta == 21), 100 * mean(sub$ls_emcta == 21)))

  # IDA longstring distribution
  cat("  IDA longstring distribution:\n")
  cat(sprintf("    Mean = %.1f, Median = %.0f, Max = %d\n",
              mean(sub$ls_ida), median(sub$ls_ida), max(sub$ls_ida)))
  cat(sprintf("    >= 10: %d (%.1f%%)\n", sum(sub$ls_ida >= 10),
              100 * mean(sub$ls_ida >= 10)))
  cat(sprintf("    = 13 (all identical): %d (%.1f%%)\n\n",
              sum(sub$ls_ida == 13), 100 * mean(sub$ls_ida == 13)))

  # Intra-individual SD
  cat("  Intra-individual SD (EMCTA):\n")
  cat(sprintf("    SD = 0 (perfectly flat): %d\n", sum(sub$sd_emcta == 0)))
  cat(sprintf("    SD < 0.3: %d\n\n", sum(sub$sd_emcta < 0.3)))

  # Flags
  cat("  Flagged cases:\n")
  cat(sprintf("    EMCTA straight-lining (ls >= 20): %d\n", sum(sub$flag_emcta)))
  cat(sprintf("    IDA straight-lining (ls >= 10):   %d\n", sum(sub$flag_ida)))
  cat(sprintf("    Either instrument:                %d (%.1f%%)\n\n",
              sum(sub$flag_any), 100 * mean(sub$flag_any)))
}

# ---- Summary ----
n_flag_col <- sum(dat_num$País == "Colômbia" & dat_num$flag_any)
n_flag_esp <- sum(dat_num$País == "Espanha" & dat_num$flag_any)
n_clean_col <- sum(dat_num$País == "Colômbia" & !dat_num$flag_any)
n_clean_esp <- sum(dat_num$País == "Espanha" & !dat_num$flag_any)

cat("=== EXCLUSION SUMMARY ===\n\n")
cat(sprintf("  Colombia: %d excluded, %d retained (was %d)\n",
            n_flag_col, n_clean_col, n_flag_col + n_clean_col))
cat(sprintf("  Spain:    %d excluded, %d retained (was %d)\n",
            n_flag_esp, n_clean_esp, n_flag_esp + n_clean_esp))
cat(sprintf("  Total:    %d excluded, %d retained (was %d)\n\n",
            n_flag_col + n_flag_esp,
            n_clean_col + n_clean_esp,
            nrow(dat_num)))


# =============================================================================
# SECTION 2: FILTERED DATA PREPARATION
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 2: FILTERED DATA — RE-ANALYSIS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# Create filtered dataset
dat_clean <- dat[!dat_num$flag_any, ]
dat_clean_col <- dat_clean[dat_clean$País == "Colômbia", ]
dat_clean_esp <- dat_clean[dat_clean$País == "Espanha", ]

# Convert to ordered factors
for (item in c(items_emcta, items_ida)) {
  dat_clean[[item]]     <- ordered(dat_clean[[item]])
  dat_clean_col[[item]] <- ordered(dat_clean_col[[item]])
  dat_clean_esp[[item]] <- ordered(dat_clean_esp[[item]])
}

cat(sprintf("Filtered sample: Colombia n = %d, Spain n = %d, Total N = %d\n\n",
            nrow(dat_clean_col), nrow(dat_clean_esp), nrow(dat_clean)))


# ---- Helper functions ----

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
    CFI         = round(fm["cfi.scaled"], 3),
    TLI         = round(fm["tli.scaled"], 3),
    RMSEA       = round(fm["rmsea.scaled"], 3),
    RMSEA_lower = round(fm["rmsea.ci.lower.scaled"], 3),
    RMSEA_upper = round(fm["rmsea.ci.upper.scaled"], 3),
    SRMR        = round(fm["srmr"], 3),
    row.names   = NULL
  )
}


# =============================================================================
# SECTION 3: CFA EMCTA-r — ORIGINAL vs. FILTERED
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 3: CFA EMCTA-r — ORIGINAL vs. FILTERED\n")
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

# Original samples (for comparison — re-fit here)
dat_orig_col <- dat[dat$País == "Colômbia", ]
dat_orig_esp <- dat[dat$País == "Espanha", ]
for (item in c(items_emcta, items_ida)) {
  dat_orig_col[[item]] <- ordered(dat_orig_col[[item]])
  dat_orig_esp[[item]] <- ordered(dat_orig_esp[[item]])
}

fit_emcta_orig_col <- cfa(model_emcta, dat_orig_col, ordered = TRUE, estimator = "WLSMV")
fit_emcta_orig_esp <- cfa(model_emcta, dat_orig_esp, ordered = TRUE, estimator = "WLSMV")
fit_emcta_filt_col <- cfa(model_emcta, dat_clean_col, ordered = TRUE, estimator = "WLSMV")
fit_emcta_filt_esp <- cfa(model_emcta, dat_clean_esp, ordered = TRUE, estimator = "WLSMV")

emcta_comparison <- rbind(
  extract_fit(fit_emcta_orig_col, "EMCTA Colombia ORIGINAL (n=415)"),
  extract_fit(fit_emcta_filt_col, sprintf("EMCTA Colombia FILTERED (n=%d)", nrow(dat_clean_col))),
  extract_fit(fit_emcta_orig_esp, "EMCTA Spain ORIGINAL (n=622)"),
  extract_fit(fit_emcta_filt_esp, sprintf("EMCTA Spain FILTERED (n=%d)", nrow(dat_clean_esp)))
)

cat("--- EMCTA-r: Original vs. Filtered fit indices ---\n")
print(emcta_comparison, right = FALSE)
cat("\n")

# Delta
cat("EMCTA Colombia RMSEA change:",
    round(emcta_comparison$RMSEA[1] - emcta_comparison$RMSEA[2], 3), "\n")
cat("EMCTA Spain RMSEA change:",
    round(emcta_comparison$RMSEA[3] - emcta_comparison$RMSEA[4], 3), "\n\n")


# =============================================================================
# SECTION 4: CFA IDA — ORIGINAL vs. FILTERED
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 4: CFA IDA — ORIGINAL vs. FILTERED\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

model_ida_a <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
'

model_ida_b <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
  EDA4  ~~ EDA5
  EDA12 ~~ EDA14
'

# Model A (no correlated residuals)
fit_ida_a_orig_col <- cfa(model_ida_a, dat_orig_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_a_orig_esp <- cfa(model_ida_a, dat_orig_esp, ordered = TRUE, estimator = "WLSMV")
fit_ida_a_filt_col <- cfa(model_ida_a, dat_clean_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_a_filt_esp <- cfa(model_ida_a, dat_clean_esp, ordered = TRUE, estimator = "WLSMV")

# Model B (with correlated residuals)
fit_ida_b_orig_col <- cfa(model_ida_b, dat_orig_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_b_orig_esp <- cfa(model_ida_b, dat_orig_esp, ordered = TRUE, estimator = "WLSMV")
fit_ida_b_filt_col <- cfa(model_ida_b, dat_clean_col, ordered = TRUE, estimator = "WLSMV")
fit_ida_b_filt_esp <- cfa(model_ida_b, dat_clean_esp, ordered = TRUE, estimator = "WLSMV")

cat("--- IDA Model A (no correlated residuals): Original vs. Filtered ---\n")
ida_a_comparison <- rbind(
  extract_fit(fit_ida_a_orig_col, "IDA-A Colombia ORIGINAL (n=415)"),
  extract_fit(fit_ida_a_filt_col, sprintf("IDA-A Colombia FILTERED (n=%d)", nrow(dat_clean_col))),
  extract_fit(fit_ida_a_orig_esp, "IDA-A Spain ORIGINAL (n=622)"),
  extract_fit(fit_ida_a_filt_esp, sprintf("IDA-A Spain FILTERED (n=%d)", nrow(dat_clean_esp)))
)
print(ida_a_comparison, right = FALSE)
cat("\n")

cat("IDA-A Colombia RMSEA change:",
    round(ida_a_comparison$RMSEA[1] - ida_a_comparison$RMSEA[2], 3), "\n")
cat("IDA-A Spain RMSEA change:",
    round(ida_a_comparison$RMSEA[3] - ida_a_comparison$RMSEA[4], 3), "\n\n")

cat("--- IDA Model B (with correlated residuals): Original vs. Filtered ---\n")
ida_b_comparison <- rbind(
  extract_fit(fit_ida_b_orig_col, "IDA-B Colombia ORIGINAL (n=415)"),
  extract_fit(fit_ida_b_filt_col, sprintf("IDA-B Colombia FILTERED (n=%d)", nrow(dat_clean_col))),
  extract_fit(fit_ida_b_orig_esp, "IDA-B Spain ORIGINAL (n=622)"),
  extract_fit(fit_ida_b_filt_esp, sprintf("IDA-B Spain FILTERED (n=%d)", nrow(dat_clean_esp)))
)
print(ida_b_comparison, right = FALSE)
cat("\n")

cat("IDA-B Colombia RMSEA change:",
    round(ida_b_comparison$RMSEA[1] - ida_b_comparison$RMSEA[2], 3), "\n")
cat("IDA-B Spain RMSEA change:",
    round(ida_b_comparison$RMSEA[3] - ida_b_comparison$RMSEA[4], 3), "\n\n")


# =============================================================================
# SECTION 5: LATENT CORRELATIONS IDA × EMCTA-r — FILTERED SAMPLE
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 5: LATENT CORRELATIONS IDA × EMCTA-r — FILTERED\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

model_joint <- '
  ANT  =~ EDA1 + EDA3 + EDA4 + EDA5
  APO  =~ EDA7 + EDA8 + EDA9
  PONT =~ EDA12 + EDA13 + EDA14
  CON  =~ EDA17 + EDA18 + EDA19
  SIT_FIN  =~ EMCTA1  + EMCTA2  + EMCTA3
  COND_FIS =~ EMCTA4  + EMCTA5  + EMCTA6
  COND_TRA =~ EMCTA7  + EMCTA8  + EMCTA9
  IMP_TRA  =~ EMCTA10 + EMCTA11 + EMCTA12
  REL_TRA  =~ EMCTA13 + EMCTA14 + EMCTA15
  REL_ORG  =~ EMCTA16 + EMCTA17 + EMCTA18
  DES_INT  =~ EMCTA19 + EMCTA20 + EMCTA21
'

fit_joint_filt_col <- cfa(model_joint, dat_clean_col, ordered = TRUE, estimator = "WLSMV")
fit_joint_filt_esp <- cfa(model_joint, dat_clean_esp, ordered = TRUE, estimator = "WLSMV")

# Also re-fit original for comparison
fit_joint_orig_col <- cfa(model_joint, dat_orig_col, ordered = TRUE, estimator = "WLSMV")
fit_joint_orig_esp <- cfa(model_joint, dat_orig_esp, ordered = TRUE, estimator = "WLSMV")

joint_comparison <- rbind(
  extract_fit(fit_joint_orig_col, "Joint Colombia ORIGINAL (n=415)"),
  extract_fit(fit_joint_filt_col, sprintf("Joint Colombia FILTERED (n=%d)", nrow(dat_clean_col))),
  extract_fit(fit_joint_orig_esp, "Joint Spain ORIGINAL (n=622)"),
  extract_fit(fit_joint_filt_esp, sprintf("Joint Spain FILTERED (n=%d)", nrow(dat_clean_esp)))
)

cat("--- Joint 11-factor model: Original vs. Filtered ---\n")
print(joint_comparison, right = FALSE)
cat("\n")

# ---- Extract cross-correlations ----

extract_cross_cors <- function(fit, label) {
  std <- standardizedSolution(fit)
  ida_f   <- c("ANT", "APO", "PONT", "CON")
  emcta_f <- c("SIT_FIN", "COND_FIS", "COND_TRA", "IMP_TRA",
                "REL_TRA", "REL_ORG", "DES_INT")

  cors <- std[std$op == "~~" & std$lhs != std$rhs, ]
  cross <- cors[
    (cors$lhs %in% ida_f & cors$rhs %in% emcta_f) |
    (cors$lhs %in% emcta_f & cors$rhs %in% ida_f),
  ]

  cor_matrix <- matrix(NA, nrow = 4, ncol = 7,
                        dimnames = list(ida_f, emcta_f))
  sig_matrix <- matrix("", nrow = 4, ncol = 7,
                         dimnames = list(ida_f, emcta_f))

  for (i in seq_len(nrow(cross))) {
    lhs <- cross$lhs[i]; rhs <- cross$rhs[i]
    r <- cross$est.std[i]; p <- cross$pvalue[i]
    row_name <- ifelse(lhs %in% ida_f, lhs, rhs)
    col_name <- ifelse(lhs %in% emcta_f, lhs, rhs)
    cor_matrix[row_name, col_name] <- r
    sig_matrix[row_name, col_name] <- ifelse(p < .001, "***",
                                      ifelse(p < .01, "**",
                                      ifelse(p < .05, "*", "ns")))
  }

  cat(paste0("--- ", label, " ---\n\n"))
  formatted <- matrix(paste0(sprintf("%.3f", cor_matrix), sig_matrix),
                       nrow = 4, dimnames = dimnames(cor_matrix))
  formatted[is.na(cor_matrix)] <- "  —  "
  print(noquote(formatted))
  cat("\n* p<.05, ** p<.01, *** p<.001, ns = not significant\n\n")
  invisible(cor_matrix)
}

cat("--- FILTERED SAMPLE ---\n\n")
cors_filt_col <- extract_cross_cors(fit_joint_filt_col,
  sprintf("Colombia FILTERED (n=%d)", nrow(dat_clean_col)))
cors_filt_esp <- extract_cross_cors(fit_joint_filt_esp,
  sprintf("Spain FILTERED (n=%d)", nrow(dat_clean_esp)))

cat("--- ORIGINAL SAMPLE (for comparison) ---\n\n")
cors_orig_col <- extract_cross_cors(fit_joint_orig_col,
  "Colombia ORIGINAL (n=415)")
cors_orig_esp <- extract_cross_cors(fit_joint_orig_esp,
  "Spain ORIGINAL (n=622)")


# =============================================================================
# SECTION 6: IDA INTER-FACTOR CORRELATIONS — FILTERED
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 6: IDA INTER-FACTOR CORRELATIONS — FILTERED vs. ORIGINAL\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

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

print_ida_cors(fit_joint_orig_col, "Colombia ORIGINAL")
print_ida_cors(fit_joint_filt_col, sprintf("Colombia FILTERED (n=%d)", nrow(dat_clean_col)))
print_ida_cors(fit_joint_orig_esp, "Spain ORIGINAL")
print_ida_cors(fit_joint_filt_esp, sprintf("Spain FILTERED (n=%d)", nrow(dat_clean_esp)))


# =============================================================================
# SECTION 7: SUMMARY
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SECTION 7: SENSITIVITY ANALYSIS SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Exclusion criterion: Longstring >= 20 on EMCTA (21 items) OR\n")
cat("                     Longstring >= 10 on IDA (13 items)\n")
cat("Applied equally to both countries.\n\n")

cat(sprintf("Excluded: Colombia %d/%d (%.1f%%), Spain %d/%d (%.1f%%)\n",
            n_flag_col, n_flag_col + n_clean_col,
            100 * n_flag_col / (n_flag_col + n_clean_col),
            n_flag_esp, n_flag_esp + n_clean_esp,
            100 * n_flag_esp / (n_flag_esp + n_clean_esp)))
cat(sprintf("Retained: Colombia n = %d, Spain n = %d\n\n",
            n_clean_col, n_clean_esp))

cat("Key RMSEA comparisons (Model A IDA, no corr. residuals):\n")
cat(sprintf("  Colombia: %.3f → %.3f (Δ = %.3f)\n",
            ida_a_comparison$RMSEA[1], ida_a_comparison$RMSEA[2],
            ida_a_comparison$RMSEA[1] - ida_a_comparison$RMSEA[2]))
cat(sprintf("  Spain:    %.3f → %.3f (Δ = %.3f)\n\n",
            ida_a_comparison$RMSEA[3], ida_a_comparison$RMSEA[4],
            ida_a_comparison$RMSEA[3] - ida_a_comparison$RMSEA[4]))

cat("Conclusion: If cross-cultural pattern (independence of exit/stay factors\n")
cat("in Colombia vs. bipolar structure in Spain) is preserved after filtering,\n")
cat("results are robust to careless responding.\n\n")


# =============================================================================
# SESSION INFO
# =============================================================================

cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SESSION INFO\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")
sessionInfo()
cat("\n\n--- Script completed successfully ---\n")
