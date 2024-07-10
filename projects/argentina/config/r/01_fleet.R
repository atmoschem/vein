# script to generate config/fleet_age.rds
library(data.table)

list.files()
x <- readxl::excel_sheets("config/inventory.xlsx")
fleet <- readxl::read_excel("config/inventory.xlsx", "fleet_age")
setDT(fleet)
saveRDS(fleet, "config/rds/fleet_age.rds")

writexl::write_xlsx(fleet, "config/xlsx/fleet_age.xlsx")