library(vein)
metadata <- readxl::read_excel("config/inventory_all.xlsx", "metadata")
setDT(metadata)
data(fkm)
metadata$vehicles

mi <- data.table(
  PC_G = fkm$KM_PC_E25(1:40),
  PC_E =  fkm$KM_PC_E100(1:40),
  PC_FG = fkm$KM_PC_FLEX(1:40),
  PC_FE = fkm$KM_PC_FLEX(1:40),
  LCV_G = fkm$KM_LCV_E25(1:40),
  LCV_E = fkm$KM_PC_E100(1:40),
  LCV_FG = fkm$KM_LCV_FLEX(1:40),
  LCV_FE = fkm$KM_LCV_FLEX(1:40),
  LCV_D = fkm$KM_LCV_B5(1:40), 
  TRUCKS_SL_D = fkm$KM_LCV_B5(1:40), 
  TRUCKS_L_D = fkm$KM_TRUCKS_B5(1:40), 
  TRUCKS_M_D = fkm$KM_TRUCKS_B5(1:40), 
  TRUCKS_SH_D = fkm$KM_ATRUCKS_B5(1:40), 
  TRUCKS_H_D = fkm$KM_ATRUCKS_B5(1:40), 
  BUS_URBAN_D = fkm$KM_BUS_B5(1:40), 
  BUS_MICRO_D = fkm$KM_BUS_B5(1:40), 
  BUS_COACH_D = fkm$KM_BUS_B5(1:40), 
  MC_150_G = fkm$KM_MOTO_E25(1:40), 
  MC_150_500_G = fkm$KM_MOTO_E25(1:40), 
  MC_500_G = fkm$KM_MOTO_E25(1:40), 
  MC_150_FG = fkm$KM_MOTO_E25(1:40), 
  MC_150_500_FG = fkm$KM_MOTO_E25(1:40), 
  MC_500_FG = fkm$KM_MOTO_E25(1:40), 
  MC_150_FE = fkm$KM_MOTO_E25(1:40), 
  MC_150_500_FE = fkm$KM_MOTO_E25(1:40), 
  MC_500_FE = fkm$KM_MOTO_E25(1:40)
)
mi
mi$Year <- 0
saveRDS(mi, "config/mileage.rds")
