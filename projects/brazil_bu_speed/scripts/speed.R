# Weekly hourly speed per link ------------------------------------------------
# Computes the speed of every link for each of the 168 hours of a week using
# the BPR function in netspeed(). Requires: net, categories, tfs, language, verbose

switch(
  language,
  "portuguese" = message("\nCalculando velocidades horarias...\n"),
  "english" = message("\nComputing hourly speeds...\n"),
  "spanish" = message("\nCalculando velocidades horarias...\n")
)

if (!requireNamespace("vein", quietly = TRUE)) stop("vein needed")

# total traffic flow per link for the 168 hours of the week, with the same
# temporal profiles used for the emissions (tfs; a daily profile is repeated
# over the 7 days). One profile per category, as in scripts/pavedroads.R
tf <- list(
  pc = tfs$PC_G,
  lcv = tfs$LCV_G,
  trucks = tfs$TRUCKS_L_D,
  bus = tfs$BUS_URBAN_D,
  mc = tfs$MC_150_G
)
pcw <- Reduce(`+`, lapply(categories, function(cc) {
  temp_fact(net[[cc]], rep_len(as.numeric(tf[[cc]]), 168))
}))

# hourly speeds (BPR)
speed_week <- netspeed(
  pcw,
  net$ps,
  net$ffs,
  net$capacity,
  net$lkm,
  alpha = 1
)

saveRDS(speed_week, "config/speed_week.rds")

# figure
png("images/SPEED_WEEK.png", 2000, 1500, "px", res = 300)
sp <- as.data.frame(remove_units(speed_week))
msp <- colMeans(sp, na.rm = TRUE)
plot(1:ncol(sp), msp,
     type = "l", xlab = "Hour of week", ylab = "km/h",
     main = "Mean speed")
dev.off()

switch(
  language,
  "portuguese" = message("Arquivo: config/speed_week.rds\n"),
  "english" = message("File: config/speed_week.rds\n"),
  "spanish" = message("Archivo: config/speed_week.rds\n")
)
