
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
n_TAXI <- nveh[grep(pattern = "TAXI", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

n_PC <- c(n_PC, n_TAXI)
# PC and TAXI ####
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
lapply(seq_along(n_PC), function(i) {
  df <- veh[, n_PC]
  sum(df[i]) / sum(df, na.rm = T)
}) -> l_pc

names(l_pc) <- n_PC
lapply(seq_along(l_pc), function(i) {
  cat(names(l_pc)[i], " is ", l_pc[[i]], " of PC\n")
}) -> lx
cat("sum: ", sum(unlist(l_pc)), "\n\n")
l_PC <- list()
n_PC
# fuel
kf <- list(k_G,
           k_G,
           k_G,
           k_G, 
           k_D,
           k_D,
           k_D,
           k_D,
            1, 1, 
           k_G, 
           1)
# necesitamos repetir cada k 12 veces y luego el numero de provincias
# loop en net
# order, regionX12 then regionx12
for (i in seq_along(n_PC)) {
  
  do.call("rbind", 
          lapply(1:(length(net[[id_region]])*12),
                 function(kk){veh[[n_PC[i]]]
    }))-> x
  x <- Vehicles(x*kf[[i]])
  saveRDS(x, paste0("veh/", n_PC[i], ".rds"))
  x$month <- 1:12
  l_PC[[i]] <- x
}
# dfpc <- as.data.frame(do.call("cbind", l_PC))
# names(dfpc) <- n_PC

# LCV ####
#
# calculate proportion in LCV
#
lapply(seq_along(n_LCV), function(i) {
  df <- veh[, n_LCV]
  sum(df[i]) / sum(df, na.rm = T)
}) -> l_lcv

names(l_lcv) <- n_LCV
lapply(seq_along(l_lcv), function(i) {
  cat(names(l_lcv)[i], " is ", l_lcv[[i]], " of LCV\n")
}) -> lx
cat("sum: ", sum(unlist(l_lcv)), "\n\n")
l_LCV <- list()

# fuel
kf <- list(k_G,
           k_G, 
           k_G, 
           k_D,
           k_D,
           k_D,
           1, 1)

for (i in seq_along(n_LCV)) {
  do.call("rbind", 
          lapply(1:(length(net[[id_region]])*12),
                 function(kk){veh[[n_LCV[i]]]
          }))-> x
  x <- Vehicles(x*kf[[i]])
  saveRDS(x, paste0("veh/", n_LCV[i], ".rds"))
  x$month <- 1:12
  l_LCV[[i]] <- x
}
# dflcv <- as.data.frame(do.call("cbind", l_LCV))
# names(dflcv) <- n_LCV

# TRUCKS ####
#
# calculate proportion in TRUCKS
#
lapply(seq_along(n_TRUCKS), function(i) {
  df <- veh[, n_TRUCKS]
  sum(df[i]) / sum(df, na.rm = T)
}) -> l_trucks

names(l_trucks) <- n_TRUCKS
lapply(seq_along(l_trucks), function(i) {
  cat(names(l_trucks)[i], " is ", l_trucks[[i]], " of TRUCKS\n")
}) -> lx
cat("sum: ", sum(unlist(l_trucks)), "\n\n")
l_TRUCKS <- list()

# fuel
kf <- list(
  k_D, k_D, k_D,
  k_D, k_D, k_D,
  k_D, k_D, k_D,
  k_G, k_G, k_G,
  k_G, k_G, k_G,
  k_G, k_G, k_G,
  k_D, k_D, k_D,
  k_D, k_D, k_D,
  1)

for (i in seq_along(n_TRUCKS)) {
  do.call("rbind", 
          lapply(1:(length(net[[id_region]])*12),
                 function(kk){veh[[n_TRUCKS[i]]]
          }))-> x
  x <- Vehicles(x*kf[[i]])
  saveRDS(x, paste0("veh/", n_TRUCKS[i], ".rds"))
  x$month <- 1:12
  l_TRUCKS[[i]] <- x
}
# dft <- as.data.frame(do.call("cbind", l_TRUCKS))
# names(dft) <- n_TRUCKS

# BUS ####
#
# calculate proportion in BUS
#
lapply(seq_along(n_BUS), function(i) {
  df <- veh[, n_BUS]
  sum(df[i]) / sum(df, na.rm = T)
}) -> l_bus

names(l_bus) <- n_BUS
lapply(seq_along(l_bus), function(i) {
  cat(names(l_bus)[i], " is ", l_bus[[i]], " of BUS\n")
}) -> lx
cat("sum: ", sum(unlist(l_bus)), "\n\n")
l_BUS <- list()

# fuel
kf <- list(
  k_D, k_D, k_D,
  k_G, k_G, k_G,
  k_D, k_D, 
  k_G, k_G,
  1, 1)


for (i in seq_along(n_BUS)) {
  do.call("rbind", 
          lapply(1:(length(net[[id_region]])*12),
                 function(kk){veh[[n_BUS[i]]]
          }))-> x
  x <- Vehicles(x*kf[[i]])
  saveRDS(x, paste0("veh/", n_BUS[i], ".rds"))
  x$month <- 1:12
  l_BUS[[i]] <- x
}
# dfb <- as.data.frame(do.call("cbind", l_BUS))
# names(dfb) <- n_BUS

# MC ####
#
# calculate proportion in MC
#
lapply(seq_along(n_MC), function(i) {
  df <- veh[, n_MC]
  sum(df[i]) / sum(df, na.rm = T)
}) -> l_mc

names(l_mc) <- n_MC
lapply(seq_along(l_mc), function(i) {
  cat(names(l_mc)[i], " is ", l_mc[[i]], " of MC\n")
}) -> lx
cat("sum: ", sum(unlist(l_mc)), "\n\n")
l_MC <- list()


# fuel
kf <- list(
  k_G, k_G,
  k_G, k_G,
  1)


for (i in seq_along(n_MC)) {
  do.call("rbind", 
          lapply(1:(length(net[[id_region]])*12),
                 function(kk){veh[[n_MC[i]]]
          }))-> x
  x <- Vehicles(x*kf[[i]])
  saveRDS(x, paste0("veh/", n_MC[i], ".rds"))
  x$month <- 1:12
  l_MC[[i]] <- x
}
# m <- as.data.frame(do.call("cbind", l_MC))
# names(dfm) <- n_MC

lapply(list(l_PC, l_LCV, l_TRUCKS, l_BUS, l_MC), rbindlist) -> df
# 
# # plots ####
# switch(language,
#   "portuguese" = cat("Plotando frota \n"),
#   "english" = cat("Plotting fleet \n"),
#   "spanish" = cat("Plotando flota \n")
# )
# 
# n_veh <- list(n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC)
# 
# for (i in seq_along(n_veh)) {
#   df_x <- df[, n_veh[[i]]]
#   png(
#     paste0(
#       "images/VEH_",
#       names(n_veh)[i],
#       ".png"
#     ),
#     2000, 1500, "px",
#     res = 300
#   )
#   colplot(
#     df = df_x,
#     cols = n_veh[[i]],
#     xlab = "Age",
#     ylab = "veh",
#     main = paste0("Veh", names(n_veh)[i]),
#     type = "l",
#     pch = NULL,
#     lwd = 1,
#     theme = theme,
#     spl = 8
#   )
#   dev.off()
# }



# ggplot2
f <- list.files(path = "veh", pattern = ".rds", full.names = T)
na <- list.files(path = "veh", pattern = ".rds")
na <- gsub(pattern = ".rds", replacement = "", x = na)
ff <- rbindlist(lapply(seq_along(f), function(i) {
  dx <- data.frame(x = rowSums(readRDS(f[i]), na.rm = T),
             month = rep(1:12,length(net[[id_region]])),
             veh = na[i],
             region = rep(net[[id_region]], each = 12))
  
}))

uv <- unique(ff$veh)
n_PC <- uv[grep(pattern = "PC", x = uv)]
n_LCV <- uv[grep(pattern = "LCV", x = uv)]
n_TRUCKS <- uv[grep(pattern = "TRUCKS", x = uv)]
n_BUS <- uv[grep(pattern = "BUS", x = uv)]
n_MC <- uv[grep(pattern = "MC", x = uv)]
ff$vehicles <- fifelse(
  ff$veh %in% n_PC, "PC",
  fifelse(
    ff$veh %in% n_LCV, "LCV",
    fifelse(
      ff$veh %in% n_TRUCKS, "TRUCKS",
      fifelse(
        ff$veh %in% n_BUS, "BUS",
        fifelse(
          ff$veh %in% n_MC, "MC", "Fleet"
        )
      )
    )
  )
)


ggplot(ff[, sum(x, na.rm = T),
          by = .(month, region, vehicles)], 
       aes(x = region, 
           y = V1, 
           fill = as.factor(vehicles))) +
  # geom_point()+
  facet_wrap(~month,  nrow = 1)+
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    y = "veh",
    title = NULL
  ) +
  theme_bw() +
  coord_flip()+
  # ggplot2::scale_fill_gradientn("[veh]", colours = cptcity::cpt(rev = T)) +
  theme(axis.text.x = element_text(hjust = 1))  -> p

print(p)

png(
  filename = paste0("images/TRAFFIC.png"),
  width = 3500, height = 2000, units = "px", pointsize = 12,
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


invisible(gc())