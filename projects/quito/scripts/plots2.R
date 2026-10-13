# plots2 ####
# Modern ggplot2 figures for the Quito inventory:
#   - grid emission maps (single hour + hourly small multiples)
#   - street hotspot maps
#   - traffic flow / speed maps
#   - diurnal profiles (curves + stacked by vehicle family)
#   - pollutant x source heatmap
#   - fleet age heatmap
#   - fuel consumption calibration
# Self-contained: reads its inputs from disk. Can be run alone or
# from scripts/11_main_plots.R after scripts/plots.R.

# defaults (override before source() if needed) ####
if (!exists("language")) language <- "spanish" # portuguese english spanish
if (!exists("year") || !is.numeric(get("year"))) year <- 2024
if (!exists("dir_images")) dir_images <- "images"
if (!exists("dir_grids")) dir_grids <- "post/grids"
if (!exists("dir_streets")) dir_streets <- "post/streets"
if (!exists("dir_veh")) dir_veh <- "veh"
if (!exists("file_df")) file_df <- "post/DF/emissions.csv.gz"
if (!exists("file_net")) file_net <- "network/net.rds"
if (!exists("file_speed_net")) file_speed_net <- "network/speed_net.rds"
if (!exists("file_fuel")) file_fuel <- "config/fuel.rds"
if (!exists("file_fc")) file_fc <- "emi/FC.csv"
if (!exists("hour2")) hour2 <- 8 # hour of day for single-hour maps
if (!exists("pol2")) pol2 <- c("CO", "NO", "NOx", "NO2", "PM2.5", "NMHC_EXHAUST_G")
if (!exists("pol_facet")) pol_facet <- c("CO", "NOx", "PM2.5")
if (!exists("pol_diurnal")) pol_diurnal <- c("CO", "NO", "NOx", "NO2", "PM", "PM10", "NMHC", "HC")
if (!exists("pol_stack")) pol_stack <- c("CO", "NOx", "PM")
if (!exists("factor_emi")) {
  if (file.exists("config/tfs.rds")) {
    tfs2 <- readRDS("config/tfs.rds")
    factor_emi <- 365 / (nrow(tfs2) / 24) # hourly to annual
  } else {
    factor_emi <- 365
  }
}

suppressMessages({
  library(ggplot2)
  library(sf)
  library(data.table)
})

dir.create(dir_images, showWarnings = FALSE)

# helpers ####
fmt <- function(x) {
  suf <- c("", "k", "M", "B", "T", "P")
  vapply(x, function(v) {
    if (length(v) == 0 || is.na(v)) return("")
    if (v == 0) return("0")
    e <- max(min(floor(log10(abs(v)) / 3), length(suf) - 1), 0)
    paste0(signif(v / 1000^e, 3), suf[e + 1])
  }, character(1))
}

theme_map2 <- function() {
  theme_void(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey35"),
      plot.caption = element_text(color = "grey50", size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

theme_chart2 <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(color = "grey35"),
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption = element_text(color = "grey50", size = 8)
    )
}

save_plot <- function(p, file, width = 11, height = 8.5, dpi = 300) {
  png(file, width = width, height = height, units = "in", res = dpi, bg = "white")
  print(p)
  dev.off()
}

created <- character()
unit_lab <- function(v) {
  u <- tryCatch(as.character(units::units(v)), error = function(e) "")
  if (length(u) == 0 || is.na(u[1])) u <- ""
  u[1]
}
cap2 <- paste0("vein | ", paste0("Quito ", year))

switch(language,
       "portuguese" = message("\nplots2: figuras ggplot2...\n"),
       "english" = message("\nplots2: ggplot2 figures...\n"),
       "spanish" = message("\nplots2: figuras ggplot2...\n")
)

# 1. grid maps, single hour ####
for (p in pol2) {
  f <- file.path(dir_grids, paste0(p, ".rds"))
  if (!file.exists(f)) { message("skip ", p, " (grid missing)"); next }
  x <- suppressWarnings(readRDS(f))
  cn <- paste0("V", hour2)
  if (!cn %in% names(x)) next
  x$value <- as.numeric(x[[cn]])
  if (sum(x$value, na.rm = TRUE) == 0) { message("skip ", p, " (all zero)"); next }
  lab <- unit_lab(x[[cn]])
  p1 <- ggplot(x) +
    geom_sf(aes(fill = value), color = NA) +
    scale_fill_viridis_c(option = "inferno", labels = fmt, name = lab) +
    labs(
      title = paste0(p, " \u2022 hour ", sprintf("%02d", hour2), " (LT)"),
      subtitle = paste0("gridded emissions, ", year),
      caption = cap2
    ) +
    theme_map2()
  out <- file.path(dir_images, sprintf("MAP_GRID2_%s_h%02d.png", p, hour2))
  save_plot(p1, out, 11, 8.5)
  created <- c(created, out)
}

# 2. grid maps, hourly small multiples ####
hours_sel <- c(2, 8, 14, 20)
for (p in pol_facet) {
  f <- file.path(dir_grids, paste0(p, ".rds"))
  if (!file.exists(f)) { message("skip ", p, " (grid missing)"); next }
  x <- suppressWarnings(readRDS(f))
  hrs <- paste0("V", hours_sel)
  if (!all(hrs %in% names(x))) next
  if (sum(sapply(hrs, function(h) sum(as.numeric(x[[h]]), na.rm = TRUE))) == 0) {
    message("skip ", p, " (all zero)") 
    next
  }
  d <- do.call(rbind, lapply(seq_along(hours_sel), function(k) {
    st_sf(hour = sprintf("%02dh", hours_sel[k]),
          value = as.numeric(x[[hrs[k]]]),
          geometry = st_geometry(x))
  }))
  d$hour <- factor(d$hour, levels = sprintf("%02dh", hours_sel))
  lab <- unit_lab(x[[hrs[1]]])
  p1 <- ggplot(d) +
    geom_sf(aes(fill = value), color = NA) +
    facet_wrap(~hour, nrow = 2) +
    scale_fill_viridis_c(option = "inferno", labels = fmt, name = lab) +
    labs(
      title = paste0(p, " \u2022 hourly pattern"),
      subtitle = paste0("gridded emissions, ", year),
      caption = cap2
    ) +
    theme_map2()
  out <- file.path(dir_images, sprintf("MAP_GRID2_HOURS_%s.png", p))
  save_plot(p1, out, 12, 9)
  created <- c(created, out)
}

# 3. street hotspot maps ####
for (p in pol2) {
  f <- file.path(dir_streets, paste0(p, ".rds"))
  if (!file.exists(f)) { message("skip ", p, " (streets missing)"); next }
  x <- suppressWarnings(readRDS(f))
  cn <- paste0("V", hour2)
  if (!cn %in% names(x)) next
  x$value <- as.numeric(x[[cn]])
  if (sum(x$value, na.rm = TRUE) == 0) { message("skip ", p, " (all zero)"); next }
  q <- stats::quantile(x$value, 0.9, na.rm = TRUE)
  top <- x[x$value > q & x$value > 0, ]
  lab <- unit_lab(x[[cn]])
  p1 <- ggplot() +
    geom_sf(data = x, color = "grey85", size = 0.15) +
    geom_sf(data = top, aes(color = value), size = 0.5) +
    scale_color_viridis_c(option = "turbo", labels = fmt, name = lab) +
    labs(
      title = paste0(p, " \u2022 hotspots (top 10% links)"),
      subtitle = paste0("street emissions, hour ", sprintf("%02d", hour2), " (LT), ", year),
      caption = cap2
    ) +
    theme_map2()
  out <- file.path(dir_images, sprintf("MAP_HOTSPOTS_%s_h%02d.png", p, hour2))
  save_plot(p1, out, 11, 8.5)
  created <- c(created, out)
}

# 4. traffic flow map ####
if (file.exists(file_net)) {
  net <- readRDS(file_net)
  d <- as.data.frame(st_set_geometry(net, NULL))
  flow <- rowSums(
    as.data.frame(lapply(d[intersect(c("pc", "lcv", "trucks", "bus", "mc", "taxi"), names(d))], as.numeric)),
    na.rm = TRUE
  )
  net$flow <- flow
  p1 <- ggplot(net) +
    geom_sf(aes(color = flow, size = flow)) +
    scale_color_viridis_c(option = "turbo", trans = "sqrt", labels = fmt, name = "veh/h (total)") +
    scale_size(range = c(0.05, 1.4), guide = "none") +
    labs(
      title = "Traffic flows \u2022 total fleet",
      subtitle = paste0("sum of all vehicle categories, ", year),
      caption = cap2
    ) +
    theme_map2()
  out <- file.path(dir_images, "MAP_FLOW2.png")
  save_plot(p1, out, 11, 8.5)
  created <- c(created, out)
}

# 5. speed map ####
if (file.exists(file_speed_net)) {
  sn <- readRDS(file_speed_net)
  cn <- paste0("S", hour2)
  if (inherits(sn, "sf")) {
    if (is.null(names(sn)) || !cn %in% names(sn)) cn <- names(sn)[1]
    sn$speed <- as.numeric(as.data.frame(st_set_geometry(sn, NULL))[[cn]])
    p1 <- ggplot(sn) +
      geom_sf(aes(color = speed), size = 0.3) +
      scale_color_viridis_c(option = "turbo", limits = c(0, NA), name = "km/h") +
      labs(
        title = "Network speed",
        subtitle = paste0("hour ", gsub("S", "", cn), " (LT), ", year),
        caption = cap2
      ) +
      theme_map2()
    out <- file.path(dir_images, sprintf("MAP_SPEED2_h%02d.png", hour2))
    save_plot(p1, out, 11, 8.5)
    created <- c(created, out)
  }
}

# 6. fleet age heatmap ####
fs <- list.files(dir_veh, pattern = "\\.rds$", full.names = TRUE)
fs <- fs[!grepl("fleet_age", basename(fs))]
if (length(fs) > 0) {
  fl <- rbindlist(lapply(fs, function(f) {
    m <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    v <- as.numeric(colSums(as.data.frame(m), na.rm = TRUE))
    data.table(vehicle = sub("\\.rds$", "", basename(f)), age = seq_along(v), n = v)
  }))
  fl <- fl[n > 0]
  if (nrow(fl) > 0) {
    fl[, share := n / sum(n, na.rm = TRUE), by = vehicle]
    if (file.exists("config/metadata.rds")) {
      lv <- readRDS("config/metadata.rds")$vehicles
      lv <- c(lv[lv %in% fl$vehicle], setdiff(unique(fl$vehicle), lv))
      fl[, vehicle := factor(vehicle, levels = rev(lv))]
    }
    p1 <- ggplot(fl, aes(age, vehicle, fill = share)) +
      geom_tile() +
      scale_fill_viridis_c(option = "mako", trans = "sqrt",
                           labels = scales::label_percent(accuracy = 1),
                           name = "% of fleet") +
      labs(
        title = "Fleet age distribution",
        subtitle = paste0("share of each vehicle category by age, ", year),
        caption = cap2
      ) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey35"),
        plot.caption = element_text(color = "grey50", size = 8),
        plot.title.position = "plot"
      )
    out <- file.path(dir_images, "FLEET2_AGE.png")
    save_plot(p1, out, 10, 12)
    created <- c(created, out)
  }
}

# 7. diurnal profiles (from post/DF) ####
if (file.exists(file_df)) {
  dt <- data.table::fread(file_df)
  need <- c("pollutant", "hour", "t")
  if (all(need %in% names(dt))) {
    dt <- dt[pollutant %in% pol_diurnal]
    dt[, t_ano := as.numeric(t) * factor_emi]

    # 7a. normalized diurnal curves
    pr <- dt[, .(emis = sum(t_ano, na.rm = TRUE)), by = .(pollutant, hour)]
    pr <- pr[order(pollutant, hour)]
    pr[, frac := emis / sum(emis, na.rm = TRUE) * 100, by = pollutant]
    if (any(pr$frac > 0, na.rm = TRUE)) {
      pr$pollutant <- factor(
        pr$pollutant,
        levels = intersect(pol_diurnal, unique(as.character(pr$pollutant)))
      )
      p1 <- ggplot(pr, aes(hour, frac, color = pollutant)) +
        geom_line(size = 1) +
        geom_point(size = 1.8) +
        scale_color_viridis_d(option = "turbo", end = 0.9) +
        scale_x_continuous(breaks = c(1, 6, 12, 18, 24)) +
        labs(
          title = "Diurnal emission profiles",
          subtitle = paste0("% of daily total by hour (LT), ", year),
          x = "hour of day", y = "% of daily total",
          color = NULL, caption = cap2
        ) +
        theme_chart2()
      out <- file.path(dir_images, "DIURNAL2.png")
      save_plot(p1, out, 10, 6.5)
      created <- c(created, out)
    }

    # 7b. stacked by vehicle family
    dt[, family := data.table::fifelse(
      grepl("^PC|^TAXI", veh), "PC",
      data.table::fifelse(
        grepl("^LCV", veh), "LCV",
        data.table::fifelse(
          grepl("^TRUCKS", veh), "TRUCKS",
          data.table::fifelse(grepl("^BUS", veh), "BUS",
                              data.table::fifelse(grepl("^MC", veh), "MC", "Other"))
        )
      )
    )]
    for (p in pol_stack) {
      if (!p %in% dt$pollutant) next
      fa <- dt[pollutant == p, .(emis = sum(t_ano, na.rm = TRUE)), by = .(hour, family)]
      if (sum(fa$emis, na.rm = TRUE) <= 0) next
      fa$family <- factor(fa$family, levels = c("PC", "LCV", "BUS", "TRUCKS", "MC", "Other"))
      p1 <- ggplot(fa, aes(hour, emis, fill = family)) +
        geom_area(alpha = 0.9) +
        scale_fill_viridis_d(option = "turbo", end = 0.9) +
        scale_x_continuous(breaks = c(1, 6, 12, 18, 24)) +
        scale_y_continuous(labels = fmt) +
        labs(
          title = paste0(p, " \u2022 diurnal emissions by vehicle family"),
          subtitle = paste0("annualized t/ano by hour (LT), ", year),
          x = "hour of day", y = "t/ano",
          fill = NULL, caption = cap2
        ) +
        theme_chart2()
      out <- file.path(dir_images, sprintf("DIURNAL2_STACK_%s.png", p))
      save_plot(p1, out, 10, 6.5)
      created <- c(created, out)
    }

    # 7c. pollutant x source heatmap
    hm <- dt[, .(t_ano = sum(t_ano, na.rm = TRUE)), by = .(pollutant, type_emi)]
    if (sum(hm$t_ano, na.rm = TRUE) > 0) {
      hm <- hm[t_ano > 0]
      p1 <- ggplot(hm, aes(type_emi, pollutant, fill = t_ano)) +
        geom_tile(color = "white", size = 0.4) +
        geom_text(aes(label = fmt(t_ano)), size = 2.6, color = "white") +
        scale_fill_viridis_c(option = "mako", trans = "sqrt", labels = fmt, name = "t/ano") +
        labs(
          title = "Emissions by pollutant and source",
          subtitle = paste0("totals ", year, " (t/ano)"),
          x = NULL, y = NULL, caption = cap2
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey35"),
          plot.caption = element_text(color = "grey50", size = 8),
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.title.position = "plot"
        )
      out <- file.path(dir_images, "HEATMAP2_POL_SOURCE.png")
      save_plot(p1, out, 11, 7)
      created <- c(created, out)
    }
  } else {
    message("skip diurnal plots (missing columns in ", basename(file_df), ")")
  }
} else {
  message("skip diurnal plots (", file_df, " not found)")
}

# 8. fuel consumption calibration ####
fc_file <- file_fc
if (!file.exists(fc_file) && file.exists(paste0(fc_file, ".gz"))) fc_file <- paste0(fc_file, ".gz")
if (file.exists(fc_file) && file.exists(file_fuel)) {
  fc <- data.table::fread(fc_file)
  fr <- as.data.frame(readRDS(file_fuel))
  if (all(c("g", "fuel", "pollutant") %in% names(fc))) {
    est <- fc[pollutant == "FC",
              .(estimation = sum(as.numeric(g), na.rm = TRUE) / 1e6 * factor_emi),
              by = fuel]
    rep <- data.frame(fuel = fr$fuel,
                      reported = as.numeric(fr$consumption_lt) / 1000 * as.numeric(fr$density_tm3))
    rep <- as.data.table(rep)
    cal <- merge(est, rep, by = "fuel", all = TRUE)
    cal <- melt(cal, id.vars = "fuel", variable.name = "source", value.name = "t")
    cal[, source := factor(source, levels = c("estimation", "reported"),
                           labels = c("estimated", "reported"))]
    ks <- cal[, .(est = t[source == "estimated"], rep = t[source == "reported"]), by = fuel]
    ks[, k := rep / est]
    cap_k <- paste0("k = ", paste(sprintf("%s %.3f", ks$fuel, ks$k), collapse = " | "))
    p1 <- ggplot(cal, aes(fuel, t, fill = source)) +
      geom_col(position = "dodge") +
      scale_y_log10(labels = fmt) +
      scale_fill_viridis_d(option = "viridis", end = 0.8) +
      labs(
        title = "Fuel consumption \u2022 estimated vs reported",
        subtitle = paste0("t/ano by fuel, ", year),
        x = NULL, y = "t/ano (log scale)", fill = NULL,
        caption = paste(cap2, cap_k)
      ) +
      theme_chart2()
    out <- file.path(dir_images, "FUEL2_CALIB.png")
    save_plot(p1, out, 9, 6.5)
    created <- c(created, out)
  }
} else {
  message("skip fuel calibration (FC.csv or fuel.rds missing)")
}

# done ####
switch(language,
       "portuguese" = message("\nplots2: ", length(created), " figuras em ", dir_images, "\n"),
       "english" = message("\nplots2: ", length(created), " figures in ", dir_images, "\n"),
       "spanish" = message("\nplots2: ", length(created), " figuras en ", dir_images, "\n")
)
cat(paste(basename(created), collapse = "\n"), "\n")
