# Weekly hourly speed per link ------------------------------------------------
# Computes the speed of every link for each of the 168 hours of a week using
# the BPR function in netspeed(). Requires: net, categories, language, verbose

switch(
  language,
  "portuguese" = message("\nCalculando velocidades horarias...\n"),
  "english" = message("\nComputing hourly speeds...\n"),
  "spanish" = message("\nCalculando velocidades horarias...\n")
)

if (!requireNamespace("vein", quietly = TRUE)) stop("vein needed")
data("pc_profile", package = "vein", envir = environment())

# total traffic flow per link (veh/h)
total_flow <- net$pc + net$lcv + net$trucks + net$bus + net$mc

# 168 hourly flows
pcw <- temp_fact(total_flow, pc_profile)

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
