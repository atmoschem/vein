# Fast speed-dependent emission factors ---------------------------------------
#
# The EMEP/EEA emission factor equations used by ef_ldv_speed() and
# ef_hdv_speed() are strings evaluated with eval(parse()).  Building a large
# hourly inventory this way is prohibitively slow because the equation is
# interpreted once per (street, hour, age).  Here the equations are compiled
# once to a small RPN bytecode (see src/e_speed.c) so millions of evaluations
# run in a tight OpenMP loop.

# Opcodes must match src/e_speed.c
.OP_CONST <- 1L
.PUSH_A <- 10L
.PUSH_B <- 11L
.PUSH_C <- 12L
.PUSH_D <- 13L
.PUSH_E <- 14L
.PUSH_F <- 15L
.PUSH_X <- 16L
.PUSH_V <- 17L
.OP_ADD <- 20L
.OP_SUB <- 21L
.OP_MUL <- 22L
.OP_DIV <- 23L
.OP_POW <- 24L
.FN_EXP <- 25L
.FN_LOG <- 26L
.CMP_LT <- 30L
.CMP_LE <- 31L
.CMP_GT <- 32L
.CMP_GE <- 33L
.CMP_EQ <- 34L
.CMP_NE <- 35L
.OP_AND <- 36L
.OP_OR <- 37L
.IFELSE <- 40L
.OP_NEG <- 41L

.vein_prog_cache <- new.env(parent = emptyenv())

# Safe scalar coefficient extraction (some EF tables lack `f`)
.coef1 <- function(row, nm) {
  z <- row[[nm]]
  if (is.null(z) || length(z) == 0) return(0)
  as.numeric(z)[1]
}
.coef6 <- function(row) {
  c(.coef1(row, "a"), .coef1(row, "b"), .coef1(row, "c"),
    .coef1(row, "d"), .coef1(row, "e"), .coef1(row, "f"))
}

# Compile an emission-factor expression string to RPN bytecode
compile_ef_expr <- function(Y) {
  if (is.na(Y) || !nzchar(trimws(Y))) {
    return(list(code = c(.OP_CONST, 0L), consts = 0))
  }
  key <- Y
  if (!is.null(.vein_prog_cache[[key]])) {
    return(.vein_prog_cache[[key]])
  }
  e <- parse(text = Y)[[1]]
  code <- integer()
  consts <- numeric()
  rec <- function(x) {
    if (is.numeric(x)) {
      consts <<- c(consts, x)
      code <<- c(code, .OP_CONST, length(consts) - 1L)
    } else if (is.name(x)) {
      nm <- as.character(x)
      code <<- c(code, switch(nm,
        a = .PUSH_A, b = .PUSH_B, c = .PUSH_C, d = .PUSH_D,
        e = .PUSH_E, f = .PUSH_F, V = .PUSH_V, x = .PUSH_X,
        stop(paste("ef_program: unknown variable", nm))
      ))
    } else if (is.call(x)) {
      op <- as.character(x[[1]])
      if (op == "(") {
        rec(x[[2]])
      } else if ((op == "-" || op == "+") && length(x) == 2) {
        rec(x[[2]])
        if (op == "-") code <<- c(code, .OP_NEG)
      } else if (op %in% c("+", "-", "*", "/", "^")) {
        rec(x[[2]]); rec(x[[3]])
        code <<- c(code, switch(op,
          "+" = .OP_ADD, "-" = .OP_SUB, "*" = .OP_MUL,
          "/" = .OP_DIV, "^" = .OP_POW
        ))
      } else if (op %in% c("<", "<=", ">", ">=", "==", "!=")) {
        rec(x[[2]]); rec(x[[3]])
        code <<- c(code, switch(op,
          "<" = .CMP_LT, "<=" = .CMP_LE, ">" = .CMP_GT,
          ">=" = .CMP_GE, "==" = .CMP_EQ, "!=" = .CMP_NE
        ))
      } else if (op == "&") {
        rec(x[[2]]); rec(x[[3]]); code <<- c(code, .OP_AND)
      } else if (op == "|") {
        rec(x[[2]]); rec(x[[3]]); code <<- c(code, .OP_OR)
      } else if (op == "exp") {
        rec(x[[2]]); code <<- c(code, .FN_EXP)
      } else if (op == "log") {
        rec(x[[2]]); code <<- c(code, .FN_LOG)
      } else if (op == "ifelse") {
        rec(x[[2]]); rec(x[[3]]); rec(x[[4]]); code <<- c(code, .IFELSE)
      } else {
        stop(paste("ef_program: unknown operator", op))
      }
    } else {
      stop("ef_program: unsupported expression node")
    }
  }
  rec(e)
  out <- list(code = as.integer(code), consts = as.numeric(consts))
  assign(key, out, envir = .vein_prog_cache)
  out
}

# Fuel correction factor by euro standard (same as internal lala)
fcorr_factor <- function(eu, fcorr = rep(1, 8)) {
  idx <- match(as.character(eu), c("PRE", "I", "II", "III", "IV", "V", "VI", "VIc"))
  idx[is.na(idx)] <- 8L
  fcorr[idx]
}

# Build one LDV program (one age)
ef_ldv_program <- function(v, t = "4S", cc, f, eu, p, x = 0, k = 1,
                           fcorr = rep(1, 8)) {
  if (v == "LCV" && any(eu %in% "V")) {
    v <- "PC"
    cc <- ">2000"
  }
  eu <- as.character(eu)
  df <- sysdata$ldv
  row <- df[
    df$VEH == v & df$TYPE == t & df$CC == cc & df$FUEL == f &
      df$EURO == eu & df$POLLUTANT == p,
  ]
  if (nrow(row) == 0) stop(paste("ef_ldv_program: no EF for", v, t, cc, f, eu, p))
  row <- row[1, ]
  prg <- compile_ef_expr(as.character(row$Y))
  list(
    code = prg$code, consts = prg$consts,
    coef = .coef6(row),
    x = as.numeric(x),
    minv = as.numeric(row$MINV), maxv = as.numeric(row$MAXV),
    k = as.numeric(k) * fcorr_factor(eu, fcorr)
  )
}

# Build one HDV program (one age)
ef_hdv_program <- function(v, t, g, eu, gr = 0, l = 0.5, p, x = 0, k = 1,
                           fcorr = rep(1, 8)) {
  p_cri <- as.character(unique(sysdata$hdv_criteria$POLLUTANT))
  p_ghg <- as.character(unique(sysdata$hdv_ghg$POLLUTANT))
  if (p %in% p_cri) {
    df <- sysdata$hdv_criteria
  } else if (p %in% p_ghg) {
    df <- sysdata$hdv_ghg
  } else {
    stop(paste("ef_hdv_program: pollutant", p, "not found"))
  }
  eu <- as.character(eu)
  row <- df[
    df$VEH == v & df$TYPE == t & df$GW == g & df$EURO == eu &
      df$GRA == gr & df$LOAD == l & df$POLLUTANT == p,
  ]
  if (nrow(row) == 0) stop(paste("ef_hdv_program: no EF for", v, t, g, eu, gr, l, p))
  row <- row[1, ]
  prg <- compile_ef_expr(as.character(row$Y))
  list(
    code = prg$code, consts = prg$consts,
    coef = .coef6(row),
    x = as.numeric(x),
    minv = as.numeric(row$MINV), maxv = as.numeric(row$MAXV),
    k = as.numeric(k) * fcorr_factor(eu, fcorr)
  )
}

# Build all LDV programs for a set of euro standards in one table lookup
ef_ldv_programs <- function(v, t = "4S", cc, f, eu, p, x = 0, k = 1,
                            fcorr = rep(1, 8)) {
  eu <- as.character(eu)
  n <- length(eu)
  kk <- rep(as.numeric(k), length.out = n) * fcorr_factor(eu, fcorr)
  df <- sysdata$ldv
  # LCV euro V is taken from PC >2000 (issue #204), applied per age as in
  # ef_ldv_speed()
  remap <- v == "LCV" & (eu %in% "V")
  tab <- df[
    df$VEH == v & df$TYPE == t & df$CC == cc & df$FUEL == f &
      df$POLLUTANT == p,
  ]
  if (nrow(tab) == 0) stop(paste("ef_ldv_programs: no EF for", v, t, cc, f, p))
  tab_pc <- NULL
  if (any(remap)) {
    tab_pc <- df[
      df$VEH == "PC" & df$TYPE == t & df$CC == ">2000" & df$FUEL == f &
        df$POLLUTANT == p,
    ]
    if (nrow(tab_pc) == 0) stop("ef_ldv_programs: no PC >2000 EF for LCV euro V")
  }
  lapply(seq_len(n), function(i) {
    tb <- if (remap[i]) tab_pc else tab
    row <- tb[tb$EURO == eu[i], , drop = FALSE]
    if (nrow(row) == 0) {
      stop(paste("ef_ldv_programs: no EF for", v, t, cc, f, eu[i], p))
    }
    row <- row[1, ]
    prg <- compile_ef_expr(as.character(row$Y))
    list(
      code = prg$code, consts = prg$consts,
      coef = .coef6(row), x = as.numeric(x),
      minv = as.numeric(row$MINV), maxv = as.numeric(row$MAXV), k = kk[i]
    )
  })
}

# Build all HDV programs for a set of euro standards in one table lookup
ef_hdv_programs <- function(v, t, g, eu, gr = 0, l = 0.5, p, x = 0, k = 1,
                            fcorr = rep(1, 8)) {
  p_cri <- as.character(unique(sysdata$hdv_criteria$POLLUTANT))
  p_ghg <- as.character(unique(sysdata$hdv_ghg$POLLUTANT))
  if (p %in% p_cri) {
    df <- sysdata$hdv_criteria
  } else if (p %in% p_ghg) {
    df <- sysdata$hdv_ghg
  } else {
    stop(paste("ef_hdv_programs: pollutant", p, "not found"))
  }
  eu <- as.character(eu)
  n <- length(eu)
  kk <- rep(as.numeric(k), length.out = n) * fcorr_factor(eu, fcorr)
  tab <- df[
    df$VEH == v & df$TYPE == t & df$GW == g & df$GRA == gr & df$LOAD == l &
      df$POLLUTANT == p,
  ]
  if (nrow(tab) == 0) stop(paste("ef_hdv_programs: no EF for", v, t, g, gr, l, p))
  lapply(seq_len(n), function(i) {
    row <- tab[tab$EURO == eu[i], , drop = FALSE]
    if (nrow(row) == 0) {
      stop(paste("ef_hdv_programs: no EF for", v, t, g, eu[i], gr, l, p))
    }
    row <- row[1, ]
    prg <- compile_ef_expr(as.character(row$Y))
    list(
      code = prg$code, consts = prg$consts,
      coef = .coef6(row), x = as.numeric(x),
      minv = as.numeric(row$MINV), maxv = as.numeric(row$MAXV), k = kk[i]
    )
  })
}

# Concatenate a list of programs into the flat structure read by the C engine.
# Ages sharing the same equation are grouped so it is evaluated only once.
speed_programs <- function(progs) {
  n <- length(progs)
  sig <- vapply(progs, function(p) paste(
    paste(p$code, collapse = ","),
    paste(sprintf("%.17g", p$consts), collapse = ","),
    paste(sprintf("%.17g", p$coef), collapse = ","),
    sprintf("%.17g", p$minv), sprintf("%.17g", p$maxv),
    sep = "|"
  ), character(1))
  usig <- unique(sig)
  gid <- match(sig, usig) - 1L
  gprogs <- progs[match(usig, sig)]
  for (g in seq_along(gprogs)) gprogs[[g]]$k <- 1
  G <- length(gprogs)
  clen <- vapply(gprogs, function(p) length(p$code), integer(1))
  ncon <- vapply(gprogs, function(p) length(p$consts), integer(1))
  structure(list(
    n = n,
    G = G,
    gid = as.integer(gid),
    kk_age = as.numeric(vapply(progs, function(p) p$k, numeric(1))),
    code = as.integer(unlist(lapply(gprogs, `[[`, "code"), use.names = FALSE)),
    clen = as.integer(clen),
    consts = as.numeric(unlist(lapply(gprogs, `[[`, "consts"), use.names = FALSE)),
    soff = as.integer(cumsum(c(0L, ncon))[seq_len(G)]),
    cofs = as.numeric(unlist(lapply(gprogs, `[[`, "coef"), use.names = FALSE)),
    x = as.numeric(vapply(gprogs, function(p) p$x, numeric(1))),
    minv = as.numeric(vapply(gprogs, function(p) p$minv, numeric(1))),
    maxv = as.numeric(vapply(gprogs, function(p) p$maxv, numeric(1))),
    progs = progs
  ), class = "speed_programs")
}

# Evaluate a single program at a numeric speed (vector) in R -- used to scale
ef_eval_program <- function(prg, speed) {
  v <- pmin(pmax(as.numeric(speed), prg$minv), prg$maxv)
  # reuse the compiled bytecode through the C engine with a single age
  sp <- matrix(v, ncol = 1)
  veh <- matrix(1, nrow = length(v), ncol = 1)
  lkm <- rep(1, length(v))
  pack <- speed_programs(list(prg))
  res <- .Call("emis_speed_engine",
    as.numeric(sp), as.numeric(veh), as.numeric(lkm), 1,
    pack$code, pack$clen, pack$consts, pack$soff, pack$cofs,
    pack$x, pack$minv, pack$maxv, pack$gid, pack$kk_age,
    as.integer(length(v)), 1L, pack$G, 1L, 1L, 0L,
    PACKAGE = "vein"
  )
  res$streets
}
