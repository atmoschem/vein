
g <- readRDS("post/emi_grid.rds")["id"]

fs <- list.files(
  path = "post/spec_grid",
  pattern = ".rds",
  full.names = TRUE,
  recursive = TRUE
)
file.remove(fs)

months_subset <- c(paste0("0", 1:9), 10:12)

lpol_all <- list()

for(j in seq_along(mech)) {
  
  # Gasoline Exhaust ####

cat(mech[j], " ")
  
x <- readRDS("post/emi_table.rds")
if (length(unique(x[grepl("^Evaporatives", type_emi)]$type_emi)) > 1) {
  stop("emi_table has evaporatives for several temperature scenarios: ",
       "set `evap_scenario` in main.R")
}
gx <- x[type_emi == "Exhaust" &
          fuel == "G" &
          pollutant == "NMHC", 
        sum(emissions, na.rm = T),
        keyby = month]

dx <- speciate(
  x = Emissions(matrix(gx$V1, nrow = 1)),
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- 1
names(dx)[1:12] <- paste0("M", months_subset)

vocE25EX <- emis_chem2(
  df = dx,
  mech = mech[j],
  nx = names(dx)[1:12]
)


# Gasoline Evap ####
gx <- x[grepl("^Evaporatives", type_emi) &
          fuel == "G" &
          pollutant == "NMHC", 
        sum(emissions, na.rm = T),
        keyby = month]

dx <- speciate(
  x = Emissions(matrix(gx$V1, nrow = 1)),
  spec = "nmhc",
  fuel = "E25",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- 1
names(dx)[1:12] <- paste0("M", months_subset)

vocE25EV <- emis_chem2(
  df = dx,
  mech = mech[j],
  nx = names(dx)[1:12],
  na.rm = TRUE
)

# Etanol Exhaust ####
gx <- x[type_emi == "Exhaust" &
          fuel == "E" &
          pollutant == "NMHC", 
        sum(emissions, na.rm = T),
        keyby = month]

dx <- speciate(
  x = Emissions(matrix(gx$V1, nrow = 1)),
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Exhaust"
)

dx$id <- 1
names(dx)[1:12] <- paste0("M", months_subset)

vocE100EX <- emis_chem2(
  df = dx,
  mech = mech[j],
  nx = names(dx)[1:12],
  na.rm = TRUE
)

# Etanol Evap ####
gx <- x[grepl("^Evaporatives", type_emi) &
          fuel == "E" &
          pollutant == "NMHC", 
        sum(emissions, na.rm = T),
        keyby = month]

dx <- speciate(
  x = Emissions(matrix(gx$V1, nrow = 1)),
  spec = "nmhc",
  fuel = "E100",
  veh = "LDV",
  eu = "Evaporative"
)

dx$id <- 1
names(dx)[1:12] <- paste0("M", months_subset)

vocE100EV <- emis_chem2(
  df = dx,
  mech = mech[j],
  nx = names(dx)[1:12],
)

# Diesel Exhaust ####
gx <- x[type_emi == "Exhaust" &
          fuel == "D" &
          pollutant == "NMHC", 
        sum(emissions, na.rm = T),
        keyby = month]

dx <- speciate(
  x = Emissions(matrix(gx$V1, nrow = 1)),
  spec = "nmhc",
  fuel = "D",
  veh = "HDV",
  eu = "all"
)

dx$id <- 1
names(dx)[1:12] <- paste0("M", months_subset)

vocB5EX <- emis_chem2(
  df = dx,
  mech = mech[j],
  nx = names(dx)[1:12],
  na.rm = TRUE
)

voc <- rbind(vocB5EX,
             vocE100EV,
             vocE100EX,
             vocE25EV,
             vocE25EX
)

dfvoc <- voc[,
             lapply(.SD, sum, na.rm = T),
             .SDcols = names(dx)[1:12],
             by = .(id, group)
]


dfvoc$id <- NULL


# other gases ####
gx <- x[pollutant %in% pol, 
        sum(emissions, na.rm = T),
        by = .(month, pollutant)]
lpol <- split(gx, gx$pollutant)

mm_x <- as.list(units::set_units(mol, "g/mol")) # mm: massa molar
names(mm_x) <- pol

for (i in seq_along(lpol)) {
  lpol[[pol[i]]]$V1 <- lpol[[pol[i]]]$V1 * (mm_x[[pol[i]]])^-1
}
dfpol <- rbindlist(lpol)
dfpol <- dcast(dfpol, formula = pollutant ~ month)
names(dfpol)[1:13] <- c("group", names(dfvoc)[2:13])


# PM2.5: exhaust PM + wear PM2.5 (both "PM" in emi_table)
gxpm <- x[pollutant %in% c("PM"), 
        sum(emissions, na.rm = T),
        keyby = .(month)]

# coarse PM = PM10 - PM2.5 of wear, the only source with PM10
gxpm10res <- x[pollutant %in% c("PM10"), 
          sum(emissions, na.rm = T),
          keyby = .(month)]
gxpm25res <- x[pollutant %in% c("PM") & type_emi == "Wear", 
          sum(emissions, na.rm = T),
          keyby = .(month)]

gxpm10coarse <- gxpm10res
gxpm10coarse$V1 <- gxpm10res$V1 - gxpm25res$V1

# aerosol species split PM2.5; coarse goes to E_PM_10
lpm <- lapply(seq_along(aer), function(i) {
  data.frame(month = 1:12, 
             group = toupper(rep(names(aer)[i], 12)), 
             V1 = gxpm$V1*aer[i])
})
lpm[[length(lpm) + 1]] <- data.frame(month = 1:12, 
                                     group = "E_PM_10", 
                                     V1 = gxpm10coarse$V1)
lpm <- rbindlist(lpm)
dfpm <- dcast(lpm, formula = group ~ month, value.var="V1")

names(dfpm)[1:13] <- c("group", names(dfvoc)[2:13])
lpol_all[[j]] <- rbind(dfvoc, dfpol, dfpm)
lpol_all[[j]]$mech <- rep(mech[j], nrow(lpol_all[[j]]))
}

df <- rbindlist(lpol_all)
saveRDS(df, "post/spec_table.rds")

switch (language,
        "portuguese" = message("\n\nArquivos em:"),
        "english" = message("\n\nFiles in:"),
        "spanish" = message("\n\nArchivos en:"))

message("post/spec_table.rds\n")
