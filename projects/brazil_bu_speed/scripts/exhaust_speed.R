# Speed-dependent exhaust emissions -------------------------------------------
# For every vehicle category and pollutant, the CETESB emission factor is
# scaled by the EMEP/EEA speed curve (ef_cetesb_speed) and evaluated with the
# fast OpenMP kernel emis_speed().  Pollutants without an EMEP speed curve fall
# back to the constant emission factor and the Fortran emis() path.
#
# Required objects (see main.R): language, metadata, tfs, veh, net, lkm, s,
# speed, pol, scale, year, verbose.

suppressWarnings(file.remove("emi/EXHAUST_SPEED_DF.csv"))

source("config/ef_speed_mapping.R", encoding = "UTF-8", local = TRUE)

H <- ncol(speed)
S <- nrow(net)
nt <- ifelse(check_nt() == 1, 1, check_nt() / 2)

# weekly profile per category: repeat the daily tfs over 7 days ---------------
tfs_week_list <- lapply(metadata$vehicles, function(v) {
  tv <- as.numeric(tfs[[v]])
  if (length(tv) == 24) as.numeric(matrix(rep(tv, 7), nrow = 24, ncol = 7))
  else tv
})
names(tfs_week_list) <- metadata$vehicles

# zero accumulators per pollutant --------------------------------------------
street_pol <- lapply(pol, function(x) matrix(0, nrow = S, ncol = H))
names(street_pol) <- pol
df_all <- list()

speed_ready <- function(veh, pol) {
  m <- ef_speed_mapping[ef_speed_mapping$veh == veh, , drop = FALSE]
  if (nrow(m) == 0) return(FALSE)
  if (m$engine[1] == "ldv") pol %in% pol_speed_ldv else pol %in% pol_speed_hdv
}

# Build the scaled speed EF or return NULL when it is not usable (e.g. the
# EMEP curve is zero at the driving-cycle speed, as for pre-Euro PM)
build_speed_ef <- function(veh_i, pj, A) {
  lef <- tryCatch(
    ef_cetesb_speed(veh_i, pj, year = year, agemax = A, scale = scale,
                    sppm = as.numeric(s[[veh_i]])[1]),
    error = function(e) NULL
  )
  if (is.null(lef)) return(NULL)
  if (any(!is.finite(attr(lef, "programs")$kk_age))) return(NULL)
  lef
}

# Constant emission factor path (falls back to Fortran emis)
constant_emis <- function(x, ef, prof, A) {
  arr <- emis(veh = x, lkm = lkm, ef = ef, profile = prof,
              agemax = A, simplify = TRUE, fortran = TRUE, nt = nt)
  list(streets = apply(arr, c(1, 3), sum), veh = apply(arr, c(2, 3), sum))
}

switch(
  language,
  "portuguese" = cat("\nEstimando emissões com velocidade\n"),
  "english" = cat("\nEstimating speed-dependent emissions\n"),
  "spanish" = cat("\nEstimando emisiones con velocidad\n")
)

for (i in seq_along(metadata$vehicles)) {
  veh_i <- metadata$vehicles[i]
  cat("\n", veh_i, "\n")
  x <- readRDS(paste0("veh/", veh_i, ".rds"))
  x <- remove_units(x)
  A <- ncol(x)
  prof <- tfs_week_list[[veh_i]]

  for (j in seq_along(pol)) {
    pj <- pol[j]
    cat(pj, " ")

    if (pj == "SO2") {
      lef <- build_speed_ef(veh_i, "FC", A)
      if (!is.null(lef)) {
        E <- emis_speed(veh = x, lkm = lkm, ef = lef, speed = speed,
                        profile = prof, agemax = A, by_age = TRUE, nt = nt)
        k <- as.numeric(s[[veh_i]])[1] * 2 * 1e-6
        streets <- E$streets * k
        vehmat <- E$veh * k
        rm(E, lef)
      } else {
        ef <- as.numeric(ef_cetesb(p = "FC", veh = veh_i, year = year,
                                   agemax = A, scale = scale)) *
          as.numeric(s[[veh_i]])[1] * 2 * 1e-6
        ce <- constant_emis(x, ef, prof, A)
        streets <- ce$streets; vehmat <- ce$veh
        rm(ce, ef)
      }
    } else if (speed_ready(veh_i, pj)) {
      lef <- build_speed_ef(veh_i, pj, A)
      if (!is.null(lef)) {
        E <- emis_speed(veh = x, lkm = lkm, ef = lef, speed = speed,
                        profile = prof, agemax = A, by_age = TRUE, nt = nt)
        streets <- E$streets
        vehmat <- E$veh
        rm(E, lef)
      } else {
        ef <- as.numeric(ef_cetesb(p = pj, veh = veh_i, year = year,
                                   agemax = A, scale = scale))
        ce <- constant_emis(x, ef, prof, A)
        streets <- ce$streets; vehmat <- ce$veh
        rm(ce, ef)
      }
    } else {
      ef <- as.numeric(ef_cetesb(p = pj, veh = veh_i, year = year,
                                 agemax = A, scale = scale))
      ce <- constant_emis(x, ef, prof, A)
      streets <- ce$streets
      vehmat <- ce$veh
      rm(ce, ef)
    }

    street_pol[[pj]] <- street_pol[[pj]] + streets

    df_all[[length(df_all) + 1]] <- data.table::data.table(
      veh = veh_i,
      size = metadata$size[i],
      fuel = metadata$fuel[i],
      type_emi = "Exhaust",
      pollutant = pj,
      age = rep(seq_len(A), times = H),
      hour = rep(seq_len(H), each = A),
      g = as.numeric(vehmat)
    )
    suppressWarnings(rm(streets, vehmat))
  }
  rm(x)
  gc(FALSE)
}

# save results ----------------------------------------------------------------
for (pj in pol) {
  saveRDS(street_pol[[pj]], paste0("emi/EXHAUST_SPEED_", pj, ".rds"))
}

df_out <- data.table::rbindlist(df_all)
data.table::fwrite(df_out, "emi/EXHAUST_SPEED_DF.csv")

# totals
tot <- df_out[, .(g = sum(g, na.rm = TRUE)), by = pollutant]
print(tot)

switch(
  language,
  "portuguese" = message("\nArquivos em: /emi/EXHAUST_SPEED_*\n"),
  "english" = message("\nFiles in: /emi/EXHAUST_SPEED_*\n"),
  "spanish" = message("\nArchivos en: /emi/EXHAUST_SPEED_*\n")
)

suppressWarnings(rm(i, j, pj, veh_i, x, prof, df_all, df_out, tot, S, H, A, nt))
invisible(gc())
