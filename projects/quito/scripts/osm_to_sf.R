library(sf)

# Specify the 'lines' layer to get streets (default is 'points')
x1 <- st_read("network/ecuador-260430.osm.pbf", layer = "lines")
x2 <- st_read("network/ecuador-260430.osm.pbf", layer = 'multilinestrings')
x <- rbind(x1, x2)

# Filter for features that have a 'highway' tag (actual streets/roads)
streets <- x[!is.na(x$highway), ]

dim(streets)
# now getting provinces

pro <- st_read("C:/Users/sibarrae/Documents/ecuador_mdpi/template/network/ecuador_provincias.gpkg")
pro <- pro[pro$DPA_DESPRO == "PICHINCHA", ]

stpi <- st_crop(st_transform(x, st_crs(pro)), pro)

dim(stpi)

unique(stpi$highway)

net <- stpi[stpi$highway %in% c("trunk",
"trunk_link",
# "residential",
"primary" ,
"secondary",
"tertiary", "primary_link",
"secondary_link",
"tertiary_link"
), ]

dim(net)

plot(net["highway"], axes = T, graticule = T)


# synthetic flow:
synthetic_params <- data.frame(
  highway = c("motorway", "trunk", "primary", "secondary", "tertiary",
              "trunk_link", "primary_link", "secondary_link", "tertiary_link"),
  
  # Traffic flow (veh/h) for each vehicle type
  pc      = c(8000, 4000, 2000, 800, 400, 2000, 1000, 400, 200),
  lcv     = c(1500, 800, 400, 150, 80,  400,  200,  80,  40),
  trucks  = c(800,  400, 150, 50,  20,  200,  75,   25,  10),
  bus     = c(100,  60,  40,  30,  15,  30,   20,   15,  8),
  mc      = c(2500, 1200, 600, 300, 150, 600,  300,  150, 75),
  taxi    = c(300,  150, 80,  40,  20,  80,   40,   20,  10),
  
  # Additional parameters
  capacity = c(2000, 1800, 1500, 1200, 800, 900, 750, 600, 400),
  ps       = c(66,   52,   42,   38,   32,  42,   38,   32,  28),   # Peak speed (km/h)
  ffs      = c(110,  80,   60,   50,   40,  60,   50,   40,  35)    # Free flow speed (km/h)
)

# Merge and add columns to net
net <- merge(net, synthetic_params, by = "highway", all.x = TRUE)

# Optional: Add small random variation for realism (remove if you want exact values)
set.seed(123)
net$pc      <- round(net$pc * runif(nrow(net), 0.8, 1.2))
net$lcv     <- round(net$lcv * runif(nrow(net), 0.8, 1.2))
net$trucks  <- round(net$trucks * runif(nrow(net), 0.8, 1.2))
net$bus     <- round(net$bus * runif(nrow(net), 0.8, 1.2))
net$mc      <- round(net$mc * runif(nrow(net), 0.8, 1.2))
net$taxi    <- round(net$taxi * runif(nrow(net), 0.8, 1.2))
net$capacity <- round(net$capacity * runif(nrow(net), 0.9, 1.1))

plot(net["pc"], axes = T, graticule = T)

plot(net["trucks"], axes = T, graticule = T)

st_write(net, "network/synthetic_quito.gpkg")
