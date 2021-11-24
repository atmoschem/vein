for (i in seq_along(metadata$vehicles)) {
  if (identical(names(veh)[i + 1], metadata$vehicles[i]) == FALSE) {
    switch(language,
      "portuguese" = cat(
        "Nomes incompativeis:\n",
        "metadata:", metadata$vehicles[i], "\n",
        "`veh`:", names(veh)[i + 1], "\n"
      ),
      "english" = cat(
        "Incomptaible names:\n",
        "metadata:", metadata$vehicles[i], "\n",
        "`veh`:", names(veh)[i + 1], "\n"
      ),
      "spanish" = cat(
        "Nombres incomptatibles:\n",
        "metadata:", metadata$vehicles[i], "\n",
        "`veh`:", names(veh)[i + 1], "\n"
      )
    )
    stop()
  }
}


# apagando arquivos
switch(language,
  "portuguese" = message("Apagando veh/*.rds\n"),
  "english" = message("Deleting veh/*.rds\n"),
  "spanish" = message("Borrando veh/*.rds\n")
)

arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

# plotting
switch(language,
  "portuguese" = cat("Plotando fluxos\n"),
  "english" = cat("Plotting traffic flows\n"),
  "spanish" = cat("Plotando flujos\n")
)

# identicar nomes de grupos
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

# PC ####
# apply survival functions
for (i in seq_along(metadata$vehicles)) {
  veh[[metadata$vehicles[i]]] <- age(
    x = veh[[metadata$vehicles[i]]],
    type = metadata$survival[i],
    a = metadata$survival_param_a[i],
    b = metadata$survival_param_b[i]
  )
}
#
# calculate proportion in PC
#

kPC_G <- sum(veh$PC_G) / sum(veh[, n_PC])
kPC_E <- sum(veh$PC_E) / sum(veh[, n_PC])
kPC_FG <- sum(veh$PC_FG) / sum(veh[, n_PC])
kPC_FE <- sum(veh$PC_FE) / sum(veh[, n_PC])
kPC <- c(kPC_G, kPC_E, kPC_FG, kPC_FE)
l_PC <- list()

cat(paste0(
  "PC_G  is ", round(kPC_G, 3), " of PC\n",
  "PC_E  is ", round(kPC_E, 3), " of PC\n",
  "PC_FG is ", round(kPC_FG, 3), " of PC\n",
  "PC_FE is ", round(kPC_FE, 3), " of PC\n",
  "sum: ", kPC_G + kPC_E + kPC_FG + kPC_FE, "\n\n"
))

# fuel
kf <- c(k_G, k_E, k_G, k_E)

for (i in seq_along(n_PC)) {
  x <- veh[[n_PC[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  l_PC[[i]] <- unlist(x)
}
dfpc <- as.data.frame(do.call("cbind", l_PC))
names(dfpc) <- n_PC

# LCV ####
#
# calculate proportion in LCV
#

kLCV_G <- sum(veh$LCV_G) / sum(veh[, n_LCV])
kLCV_E <- sum(veh$LCV_E) / sum(veh[, n_LCV])
kLCV_FG <- sum(veh$LCV_FG) / sum(veh[, n_LCV])
kLCV_FE <- sum(veh$LCV_FE) / sum(veh[, n_LCV])
kLCV_D <- sum(veh$LCV_D) / sum(veh[, n_LCV])
kLCV <- c(kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D)
l_LCV <- list()

cat(paste0(
  "LCV_G  é ", round(kLCV_G, 3), " de LCV\n",
  "LCV_E  é ", round(kLCV_E, 3), " de LCV\n",
  "LCV_FG é ", round(kLCV_FG, 3), " de LCV\n",
  "LCV_FE é ", round(kLCV_FE, 3), " de LCV\n",
  "LCV_D  é ", round(kLCV_D, 3), " de LCV\n\n",
  "sum: ", kLCV_G + kLCV_FG + kLCV_FE + kLCV_E + kLCV_D, "\n"
))

# fuel
kf <- c(k_G, k_E, k_G, k_E, k_D)

for (i in seq_along(n_LCV)) {
  x <- veh[[n_LCV[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
  l_LCV[[i]] <- unlist(x)
}
dflcv <- as.data.frame(do.call("cbind", l_LCV))
names(dflcv) <- n_LCV

# TRUCKS ####
#
# calculate proportion in TRUCKS
#

kTRUCKS_SL_D <- sum(veh$TRUCKS_SL_D) / sum(veh[, n_TRUCKS])
kTRUCKS_L_D <- sum(veh$TRUCKS_L_D) / sum(veh[, n_TRUCKS])
kTRUCKS_M_D <- sum(veh$TRUCKS_M_D) / sum(veh[, n_TRUCKS])
kTRUCKS_SH_D <- sum(veh$TRUCKS_SH_D) / sum(veh[, n_TRUCKS])
kTRUCKS_H_D <- sum(veh$TRUCKS_H_D) / sum(veh[, n_TRUCKS])
kTRUCKS <- c(kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D)
l_TRUCKS <- list()

cat(paste0(
  "TRUCKS_G  é ", round(kTRUCKS_SL_D, 3), " de TRUCKS\n",
  "TRUCKS_FG é ", round(kTRUCKS_L_D, 3), " de TRUCKS\n",
  "TRUCKS_FE é ", round(kTRUCKS_M_D, 3), " de TRUCKS\n",
  "TRUCKS_E  é ", round(kTRUCKS_SH_D, 3), " de TRUCKS\n",
  "TRUCKS_D  é ", round(kTRUCKS_H_D, 3), " de TRUCKS\n\n",
  "sum: ", kTRUCKS_SL_D + kTRUCKS_L_D + kTRUCKS_M_D + kTRUCKS_SH_D + kTRUCKS_H_D, "\n"
))

# fuel
kf <- rep(k_D, 5)

for (i in seq_along(n_TRUCKS)) {
  x <- veh[[n_TRUCKS[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
  l_TRUCKS[[i]] <- unlist(x)
}
dft <- as.data.frame(do.call("cbind", l_TRUCKS))
names(dft) <- n_TRUCKS

# BUS ####
#
# calculate proportion in BUS
#

kBUS_URBAN_D <- sum(veh$BUS_URBAN_D) / sum(veh[, n_BUS])
kBUS_MICRO_D <- sum(veh$BUS_MICRO_D) / sum(veh[, n_BUS])
kBUS_COACH_D <- sum(veh$BUS_COACH_D) / sum(veh[, n_BUS])
kBUS <- c(kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D)
l_BUS <- list()

cat(paste0(
  "BUS_URBAN_D  é ", round(kBUS_URBAN_D, 3), " de BUS\n",
  "BUS_MICRO_D  é ", round(kBUS_MICRO_D, 3), " de BUS\n",
  "BUS_URBAN_D  é ", round(kBUS_COACH_D, 3), " de BUS\n",
  "sum: ", kBUS_URBAN_D + kBUS_MICRO_D + kBUS_COACH_D, "\n\n"
))

# fuel
kf <- rep(k_D, 3)

for (i in seq_along(n_BUS)) {
  x <- veh[[n_BUS[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
  l_BUS[[i]] <- unlist(x)
}
dfb <- as.data.frame(do.call("cbind", l_BUS))
names(dfb) <- n_BUS

# MC ####
#
# calculate proportion in MC
#

kMC_150_G <- sum(veh$MC_150_G) / sum(veh[, n_MC])
kMC_150_500_G <- sum(veh$MC_150_500_G) / sum(veh[, n_MC])
kMC_500_G <- sum(veh$MC_500_G) / sum(veh[, n_MC])
kMC_150_FG <- sum(veh$MC_150_FG) / sum(veh[, n_MC])
kMC_150_500_FG <- sum(veh$MC_150_500_FG) / sum(veh[, n_MC])
kMC_500_FG <- sum(veh$MC_500_FG) / sum(veh[, n_MC])
kMC_150_FE <- sum(veh$MC_150_FE) / sum(veh[, n_MC])
kMC_150_500_FE <- sum(veh$MC_150_500_FE) / sum(veh[, n_MC])
kMC_500_FE <- sum(veh$MC_500_FE) / sum(veh[, n_MC])
kMC <- c(
  kMC_150_G, kMC_150_500_G, kMC_500_G,
  kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
  kMC_150_FE, kMC_150_500_FE, kMC_500_FE
)
l_MC <- list()

cat(paste0(
  "MC_150_G  é ", round(kMC_150_G, 3), " de MC\n",
  "MC_150_500_G  é ", round(kMC_150_500_G, 3), " de MC\n",
  "MC_500_G  é ", round(kMC_500_G, 3), " de MC\n",
  "MC_150_FG  é ", round(kMC_150_FG, 3), " de MC\n",
  "MC_150_500_FG  é ", round(kMC_150_500_FG, 3), " de MC\n",
  "MC_500_FG  é ", round(kMC_500_FG, 3), " de MC\n",
  "MC_150_FE  é ", round(kMC_150_FE, 3), " de MC\n",
  "MC_150_500_FE  é ", round(kMC_150_500_FE, 3), " de MC\n",
  "MC_500_FE  é ", round(kMC_500_FE, 3), " de MC\n",
  "sum: ",
  kMC_150_G + kMC_150_500_G + kMC_500_G +
    kMC_150_FG + kMC_150_500_FG + kMC_500_FG +
    kMC_150_FE + kMC_150_500_FE + kMC_500_FE, "\n\n"
))

# fuel
kf <- c(rep(k_G, 6), rep(k_E, 3))

for (i in seq_along(n_MC)) {
  x <- veh[[n_MC[i]]] * kf[i]
  x <- Vehicles(matrix(x, ncol = length(x)))
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
  l_MC[[i]] <- unlist(x)
}
dfm <- as.data.frame(do.call("cbind", l_MC))
names(dfm) <- n_MC

df <- cbind(dfpc, dflcv, dft, dfb, dfm)

# plots ####
switch(language,
  "portuguese" = cat("Plotando frota \n"),
  "english" = cat("Plotting fleet \n"),
  "spanish" = cat("Plotando flota \n")
)

for (i in seq_along(n_veh)) {
  df_x <- df[, n_veh[[i]]]
  png(
    paste0(
      "images/VEH_",
      names(n_veh)[i],
      ".png"
    ),
    2000, 1500, "px",
    res = 300
  )
  colplot(
    df = df_x,
    cols = n_veh[[i]],
    xlab = "Age",
    ylab = "veh",
    main = paste0("Veh", names(n_veh)[i]),
    type = "l",
    pch = NULL,
    lwd = 1,
    theme = theme,
    spl = 8
  )
  dev.off()
}



# ggplot2
f <- list.files(path = "veh", pattern = ".rds", full.names = T)
na <- list.files(path = "veh", pattern = ".rds")
na <- gsub(pattern = ".rds", replacement = "", x = na)
ff <- unlist(lapply(seq_along(f), function(i) {
  sum(readRDS(f[i]), na.rm = T)
}))
df <- data.frame(
  per = ff,
  Categoria = na
)
df <- df[df$Categoria != "fleet_age", ]
df$per <- df$per / sum(df$per) * 100
df$Categoria <- factor(df$Categoria, levels = metadata$vehicles)
p <- ggplot(df, aes(x = Categoria, y = per, fill = per)) +
  geom_bar(stat = "identity", col = "black") +
  labs(
    y = "[%]",
    title = "Vehicular composition [%]"
  ) +
  ggplot2::scale_fill_gradientn("[%]", colours = cptcity::cpt(rev = T)) +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(limits = rev(metadata$vehicles)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

png(
  filename = paste0("images/TRAFFIC.png"),
  width = 2500, height = 2500, units = "px", pointsize = 12,
  bg = "white", res = 300
)
print(p)
dev.off()

switch(language,
  "portuguese" = message("\nArquivos em:"),
  "english" = message("\nFiles in:"),
  "spanish" = message("\nArchivos en:")
)

message("veh/*\n")

switch(language,
  "portuguese" = message("\nFiguras em"),
  "english" = message("\nFigures in"),
  "spanish" = message("\nFiguras en")
)
message("/images")

switch(language,
  "portuguese" = message("Limpando..."),
  "english" = message("Cleaning..."),
  "spanish" = message("Limpiando...")
)

suppressWarnings(rm(
  kPC, kPC_G, kPC_E, kPC_FG, kPC_FE,
  kLCV, kLCV_G, kLCV_E, kLCV_FG, kLCV_FE, kLCV_D,
  kTRUCKS, kTRUCKS_SL_D, kTRUCKS_L_D, kTRUCKS_M_D, kTRUCKS_SH_D, kTRUCKS_H_D,
  kBUS, kBUS_URBAN_D, kBUS_MICRO_D, kBUS_COACH_D,
  kMC, kMC_150_G, kMC_150_500_G, kMC_500_G,
  kMC_150_FG, kMC_150_500_FG, kMC_500_FG,
  kMC_150_FE, kMC_150_500_FE, kMC_500_FE,
  l_PC, l_LCV, l_TRUCKS, l_BUS, l_MC,
  i, arquivos, cores, df, f, ff,
  n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC,
  na, nveh, p, tit, tit2, veiculos, verbose, x, kf,
  k_G, k_D, k_E,
  metadata, net, veh, year
))
invisible(gc())