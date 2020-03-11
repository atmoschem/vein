context("emis_post")

df <- as.data.frame(matrix(1:100, ncol =10, nrow = 10))
wCO <- emis_order2(x = df,
                    lt_emissions = "2020-02-19 00:00",
                    start_utc_time = "2020-02-20 00:00",
                    desired_length = 78)
