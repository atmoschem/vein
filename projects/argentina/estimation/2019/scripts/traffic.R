year_selected <- as.numeric(basename(getwd()))

# n_veh
n_PC <- metadata$vehicles[grep(pattern = "PC", x = metadata$vehicles)]
n_LCV <- metadata$vehicles[grep(pattern = "LCV", x = metadata$vehicles)]
n_TRUCKS <- metadata$vehicles[grep(pattern = "TRUCKS", x = metadata$vehicles)]
n_BUS <- metadata$vehicles[grep(pattern = "BUS", x = metadata$vehicles)]
n_MC <- metadata$vehicles[grep(pattern = "MC", x = metadata$vehicles)]
n_veh <- list(PC = n_PC,
              LCV = n_LCV,
              TRUCKS = n_TRUCKS,
              BUS = n_BUS,
              MC = n_MC)

inte <- intersect(metadata$vehicles, names(veh))

if(length(inte) != length(metadata$vehicles)){

  switch (language,
          "portuguese" = cat( "veh precisa ter mesmos veiculos de metadata$vehicles:\n"),
          "english" = cat( "veh needs the same vehicles as metadata$vehicles:\n"),
          "spanish" = cat( "veh necesita los mismos vehiculos de metadata$vehicles:\n"))


  stop()
}

# apagando arquivos
switch (language,
        "portuguese" = message("Apagando veh/*.rds\n"),
        "english" = message("Deleting veh/*.rds\n"),
        "spanish" = message("Borrando veh/*.rds\n"))

arquivos <- list.files(path = "veh", pattern = ".rds", full.names = TRUE)
file.remove(arquivos)

# fleet age
veh[is.na(veh)] <- 0

# plotting
switch (language,
        "portuguese" = cat("Plotando fluxos\n"),
        "english" = cat("Plotting traffic flows\n"),
        "spanish" = cat("Plotando flujos\n"))

# identicar nomes de grupos
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

setDT(veh)
setorderv(veh,
          cols = c( "Year"),
          order = c( -1))

if(survival) {

  if(any(grepl("region", names(veh)))) {
    cat("Identified `region` in `veh`\n")

    lv <- split(veh, veh[["region"]])

    for(j in seq_along(lv)) {

      for(i in seq_along(metadata$vehicles)) {
        lv[[j]][[metadata$vehicles[i]]] <-   age(x = lv[[j]][[metadata$vehicles[i]]],
                                                 type = metadata$survival[i],
                                                 a = metadata$survival_param_a[i],
                                                 b = metadata$survival_param_b[i])
      }
    }
      veh <- rbindlist(lv)

  } else {
    cat("No `region` in `veh`\n")

      for(i in seq_along(metadata$vehicles)) {
        veh[[metadata$vehicles[i]]] <-   age(x = veh[[metadata$vehicles[i]]],
                                             type = metadata$survival[i],
                                             a = metadata$survival_param_a[i],
                                             b = metadata$survival_param_b[i])
  }}
}


# veh ####
#
v <- metadata$vehicles

# columna region en hoja fuel ####
reg <- unique(fuel[["region"]])

if(any(grepl("region", names(veh)))) {

  cat("Identified `region` in `veh`\n")

  rbindlist(lapply(seq_along(v), function(i) {

    if(verbose){
      cat("\n", metadata$vehicles[i],
          rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
      )}

    rbindlist(lapply(seq_along(reg), function(j) {

      # cat(reg[j], " " )


      x <- veh[[v[i]]]*fuel[region == reg[j] &
                              fuel == metadata$fuel[i]]$kinitial
      x <- remove_units(x)[1:metadata$maxage[i]]
      x <- Vehicles(matrix(x, ncol = metadata$maxage[i]))
      x$"region" <- reg[j]
      x
    })) -> dt

    saveRDS(dt, paste0("veh/", v[i], ".rds"))

    df <- melt.data.table(dt,
                          id.vars = "region",
                          measure.vars = paste0("V", 1:metadata$maxage[i]),
                          variable.name = "age",
                          value.name = "veh")
    df$vehicles <- v[i]
    df
  })) -> vv

} else {
  cat("No `region` in `veh`\n")

  rbindlist(lapply(seq_along(v), function(i) {

    if(verbose){
      cat("\n", metadata$vehicles[i],
          rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i]))
      )}


    # cat(reg[j], " " )


    x <- veh[[v[i]]]*fuel[fuel == metadata$fuel[i]]$kinitial
    x <- remove_units(x)[1:metadata$maxage[i]]
    x <- Vehicles(matrix(x, ncol = metadata$maxage[i]))
    x

    saveRDS(x, paste0("veh/", v[i], ".rds"))

    df <- melt.data.table(x,
                          # id.vars = "region",
                          measure.vars = paste0("V", 1:metadata$maxage[i]),
                          variable.name = "age",
                          value.name = "veh")
    df$vehicles <- v[i]
    df
  })) -> vv
}


cat("\n")

# plots ####
switch (language,
        "portuguese" = cat("Plotando frota \n"),
        "english" = cat("Plotting fleet \n"),
        "spanish" = cat("Plotando flota \n"))
names(vv)

vv <- merge(vv,
            metadata,
            by = "vehicles",
            all.x = TRUE)

vv$age <- as.numeric(gsub("V", "", vv$age))
vv$Year <- year_selected - vv$age + 1
vv$sf <- paste(vv$size, vv$fuel)
fam <- unique(metadata$family)

vv <- remove_units(vv)

for(i in seq_along(fam)) {

ggplot(vv[family == fam[i] &
                as.numeric(veh)> 0],
           aes(x = Year,
               y = veh,
               colour = vehicles)) +
      geom_line() +
      facet_wrap(~ region,
                 scales = "free_y") +
      theme_bw(base_size = 10)+
      theme(axis.text.x = element_text(angle = 90)) -> p

  png(paste0("images/FLEET_CIRCULATING_", fam[i], ".png"),
      width = 3000,
      height = 2500,
      "px",
      res = 300)
  print(p)
  dev.off()
}


# ggplot2

dx <- vv[,
         sum(veh, na.rm = T),
         by = .(vehicles,
                family,
                region
         )]
names(dx)[4] <- "veh"

p <- ggplot(dx,
            aes(x = vehicles,
                y = veh,
                fill = family)) +
  geom_bar(stat = "identity",
           col = "black")+
  labs(y = "veh",
       title = "Vehicles") +
  facet_wrap(~ region,
             scales = "free_x",
             nrow = 2) +
  theme_bw() +
  scale_y_sqrt() +
  coord_flip()+
  scale_x_discrete(limits = rev(metadata$vehicles)) +
  theme(axis.text.x = element_text(angle=90,
                                   hjust=1))

p

png(filename =  paste0("images/VEHICLES_INITIAL.png"),
    width = 2500, height = 2500, units = "px", pointsize = 12,
    bg = "white",  res = 300)
print(p)
dev.off()

switch (language,
        "portuguese" = message("\nArquivos em:"),
        "english" = message("\nFiles in:"),
        "spanish" = message("\nArchivos en:"))

message("veh/*\n")

switch (language,
        "portuguese" = message("\nFiguras em"),
        "english" = message("\nFigures in"),
        "spanish" = message("\nFiguras en"))
message("/images")

