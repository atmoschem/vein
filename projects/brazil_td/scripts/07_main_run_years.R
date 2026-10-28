options(encoding = "UTF-8")

#
# x <- list.files(path="estimation",
#                 pattern = "main.R",
#                 recursive = T,
#                 full.names = T)
#
# xx <- paste0(getwd(),"/",
#              x[!grepl(pattern = ".Rproj", x)])
#
# xx <- gsub("main.R", "", xx)
#
# saveRDS(xx, paste0(getwd(), "/mains.rds"))
#
# for (i in seq_along(xx)) {
#
#   di <- readRDS(paste0(getwd(), "/mains.rds"))
#   setwd(di[i])
#   print(di[i])
#
#   sink(paste0(di[i], "/log.txt"))
#   print(paste0(di[i], "/log.txt"))
#
#   source(file = "main.R", echo = T)
#   gc()
#   sink()
#
# }
#
#
