#' Fast speed-dependent bottom-up emission inventory
#'
#' @description \code{\link{emis_speed}} estimates hourly emissions per street
#' using speed-dependent emission factors obtained with
#' \code{\link{ef_ldv_scaled}} or \code{\link{ef_hdv_scaled}}. The EMEP/EEA
#' equations are evaluated in a compiled OpenMP kernel, which is orders of
#' magnitude faster than looping over hours and ages in R.
#'
#' Emissions are computed as
#' \deqn{E(s,h) = lkm(s) * \sum_a veh(s,a) * profile(h) * EF_a(speed(s,h))}
#' and the result is returned aggregated by street and hour, and optionally by
#' age and hour.
#'
#' @param veh numeric matrix or data.frame of vehicle flows, rows are streets
#' and columns are ages (as in \code{\link{emis}}).
#' @param lkm numeric vector with the length of each link in km.
#' @param ef a list returned by \code{\link{ef_ldv_scaled}} or
#' \code{\link{ef_hdv_scaled}} (it must carry the compiled programs attribute).
#' @param speed data.frame or matrix of speeds (km/h), rows are streets and
#' columns are hours (flattened hours x days).
#' @param profile temporal profile, matrix (24 x 7) or vector. If missing, a
#' profile of ones is used.
#' @param agemax integer; number of ages to use.
#' @param by_age logical; return the per-age hourly totals (sum over streets).
#' @param nt integer; number of OpenMP threads.
#' @param verbose logical.
#' @return A list with \code{streets} (streets x hours) and, when
#' \code{by_age = TRUE}, \code{veh} (ages x hours).
#' @export
emis_speed <- function(veh,
                       lkm,
                       ef,
                       speed,
                       profile,
                       agemax = ncol(veh),
                       by_age = TRUE,
                       nt = ifelse(check_nt() == 1, 1, check_nt() / 2),
                       verbose = FALSE) {
  prog <- if (inherits(ef, "speed_programs")) ef else attr(ef, "programs")
  if (is.null(prog)) {
    stop(
      "`ef` must come from ef_ldv_scaled() or ef_hdv_scaled() and carry ",
      "compiled speed programs.\n",
      "Recreate the emission factors with the current version of vein."
    )
  }
  if (any(!is.finite(prog$kk_age))) {
    stop(
      "Some scaled emission factors are not finite (e.g. the EMEP curve is ",
      "zero at the driving-cycle speed). Use a constant emission factor for ",
      "those categories/pollutants instead."
    )
  }
  if (inherits(veh, "sf")) veh <- sf::st_set_geometry(veh, NULL)
  veh <- remove_units(veh)
  veh <- as.matrix(veh)
  lkm <- as.numeric(remove_units(lkm))
  speed <- remove_units(speed)
  speed <- as.matrix(speed)

  S <- nrow(veh)
  A <- ncol(veh)
  H <- ncol(speed)

  if (S != nrow(speed)) stop("nrow(veh) must equal nrow(speed)")
  if (length(lkm) != S) stop("length(lkm) must equal nrow(veh)")

  agemax <- min(agemax, A, prog$n)
  if (agemax < A) {
    veh <- veh[, seq_len(agemax), drop = FALSE]
    A <- agemax
  }
  if (prog$n > A) {
    prog <- subset_speed_programs(prog, seq_len(A))
  }

  if (missing(profile) || is.null(profile)) {
    profilev <- rep(1, H)
  } else {
    profilev <- as.numeric(unlist(profile))
  }
  if (length(profilev) != H) {
    stop("length(profile) must equal ncol(speed)")
  }

  if (verbose) {
    message(
      "emis_speed: ", S, " streets x ", A, " ages x ", H, " hours,",
      " using ", nt, " threads"
    )
  }

  res <- .Call("emis_speed_engine",
    as.numeric(speed),
    as.numeric(veh),
    as.numeric(lkm),
    as.numeric(profilev),
    prog$code, prog$clen, prog$consts, prog$soff, prog$cofs,
    prog$x, prog$minv, prog$maxv, prog$gid, prog$kk_age,
    as.integer(S), as.integer(A), as.integer(prog$G), as.integer(H),
    as.integer(nt), as.integer(if (isTRUE(by_age)) 1L else 0L),
    PACKAGE = "vein"
  )

  streets <- matrix(res$streets, nrow = S, ncol = H)
  colnames(streets) <- paste0("h", seq_len(H))
  out <- list(streets = streets)
  if (isTRUE(by_age)) {
    v <- matrix(res$veh, nrow = A, ncol = H)
    rownames(v) <- seq_len(A)
    colnames(v) <- paste0("h", seq_len(H))
    out$veh <- v
  }
  out
}

# keep the first `idx` programs of a bundle
subset_speed_programs <- function(prog, idx) {
  speed_programs(prog$progs[idx])
}
