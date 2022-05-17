
# identicar nomes de grupos
nveh <- names(veh)
n_PC <- nveh[grep(pattern = "PC", x = nveh)]
n_LCV <- nveh[grep(pattern = "LCV", x = nveh)]
n_TRUCKS <- nveh[grep(pattern = "TRUCKS", x = nveh)]
n_BUS <- nveh[grep(pattern = "BUS", x = nveh)]
n_MC <- nveh[grep(pattern = "MC", x = nveh)]

# rowSums
PC <- lapply(seq_along(n_PC), function(i){
   rowSums(readRDS(paste0("veh/", n_PC[i], ".rds")))
})
PC <- rowSums(do.call("cbind", PC))

LCV <- lapply(seq_along(n_LCV), function(i){
   rowSums(readRDS(paste0("veh/", n_LCV[i], ".rds")))
})
LCV <- rowSums(do.call("cbind", LCV))

TRUCKS <- lapply(seq_along(n_TRUCKS), function(i){
   rowSums(readRDS(paste0("veh/", n_TRUCKS[i], ".rds")))
})
TRUCKS <- rowSums(do.call("cbind", TRUCKS))

BUS <- lapply(seq_along(n_BUS), function(i){
   rowSums(readRDS(paste0("veh/", n_BUS[i], ".rds")))
})
BUS <- rowSums(do.call("cbind", BUS))

MC <- lapply(seq_along(n_MC), function(i){
   rowSums(readRDS(paste0("veh/", n_MC[i], ".rds")))
})
MC <- rowSums(do.call("cbind", MC))



# Fluxo semanal horario vezes veiculo equivalente (veh eq)
vkpc <- temp_fact(q = PC, pro = tf_PC)
vklcv <- temp_fact(LCV, pro = tf_LCV)
vkhgv <- temp_fact(TRUCKS, pro =  tf_TRUCKS)
vkbus <- temp_fact(BUS, pro = tf_BUS)
vkmc <- temp_fact(MC, pro = tf_MC)
vk <- vkpc + vklcv + vkmc + vkhgv + vkbus

# calculo ADT
ADT <-  adt(pc = PC,
            lcv = LCV,
            hgv = TRUCKS,
            bus = BUS,
            mc = MC,
            p_pc = tf_PC,
            p_lcv = tf_LCV,
            p_hgv = tf_TRUCKS,
            p_bus = tf_BUS,
            p_mc = tf_MC)


# calculo W
W <-  aw(pc = PC,
         lcv = LCV,
         hgv = TRUCKS,
         bus = BUS,
         mc = MC,
         p_pc = tf_PC,
         p_lcv = tf_LCV,
         p_hgv = tf_TRUCKS,
         p_bus = tf_BUS,
         p_mc = tf_MC)



pol <- c("PM10", "PM")
k <- c(0.62, 0.15)


for(i in seq_along(metadata$vehicles)) {
   
   cat("\n", metadata$vehicles[i], 
       rep("", max(nchar(metadata$vehicles) + 1) - nchar(metadata$vehicles[i])))
   
   for(j in seq_along(pol)) {
      
      cat(pol[j], " ")
      
      x <- readRDS(paste0("veh/", metadata$vehicles[i], ".rds"))
      q <- temp_fact(q = rowSums(x),  pro = tfs[[metadata$vehicles[i]]])
      
      emi <- emis_paved(veh = q, 
                        adt = ADT, 
                        lkm = lkm,
                        k = k[j], 
                        W = W)
      saveRDS(emi, 
              file = paste0('emi/', 
                            metadata$vehicles[i] ,'/', 
                            metadata$vehicles[i] ,
                            '_PAVED_ROADS_', 
                            pol[j], 
                            '_STREETS.rds'))
      
      g <- Emissions(colSums(emi))
      x_DF <- data.frame(array_x = rep("array_x", length(tfs[[metadata$vehicles[i]]])),
                         g = g,
                         veh = rep(metadata$vehicles[i], ncol(emi)),
                         size = rep("RESUS", ncol(emi)),
                         fuel = rep("ALL", ncol(emi)),
                         type_emi = rep("Paved Roads", ncol(emi)),
                         pollutant = rep(pol[j], ncol(emi)),
                         age = rep(NA, ncol(emi)),
                         hour = 1:ncol(emi))
      
      saveRDS(x_DF, 
              file = paste0('emi/', 
                            metadata$vehicles[i] ,'/', 
                            metadata$vehicles[i] ,
                            '_PAVED_ROADS_', 
                            pol[j], 
                            '_DF.rds'))
      
   }
}

switch (language,
        "portuguese" = message("\n\nArquivos em: /emi/*:"),
        "english" = message("\nFiles in: /emi/*"),
        "spanish" = message("\nArchivos en: /emi/*"))


switch (language,
        "portuguese" = message("Limpando..."),
        "english" = message("Cleaning..."),
        "spanish" = message("Limpiando..."))



suppressWarnings(
   rm(ADT, 
      PC, LCV, TRUCKS, BUS, MC,
      n_PC, n_LCV, n_TRUCKS, n_BUS, n_MC, 
      tf_PC, tf_LCV, tf_TRUCKS, tf_BUS, tf_MC, 
      emi, g, lkm, 
      metadata,  tfs,veh, 
      net, nveh,  pol, 
      verbose, 
      vk, vkbus, vkhgv, vklcv, vkmc, vkpc, W, x_DF,
      ef_cetesb2, ef_evaps, i, j, k, mileage, name_file_evap, num_vein, 
      q, type_emis, vein_version, x, cores, fuel,
      sL1,sL2, sL3, sL4)
)

