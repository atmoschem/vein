# script to generate config/mileage.rds
library(data.table)

readxl::excel_sheets("config/inventory.xlsx")
mileage <- readxl::read_excel("config/inventory.xlsx", "mileage")
setDT(mileage)
mileage
saveRDS(mileage, "config/rds/mileage.rds")

writexl::write_xlsx(mileage, "config/xlsx/mileage.xlsx")
