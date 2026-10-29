context("misc fixes")

test_that("add_polid uses the polygon and not its bounding box", {
  t1 <- sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0))))
  t2 <- sf::st_polygon(list(rbind(c(0, 0), c(10, 10), c(0, 10), c(0, 0))))
  polyg <- sf::st_sf(id = c("A", "B"),
    geometry = sf::st_sfc(t1, t2, crs = 31983))
  street <- sf::st_sf(len = 1:2,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(1, 8), c(2, 9))),
      sf::st_linestring(rbind(c(1, 5), c(9, 5))),
      crs = 31983))
  a <- add_polid(polyg, street, by = "id")
  expect_equal(nrow(a), 3)
  expect_equal(sort(a$id[a$len == 1]), "B")
  expect_equal(sort(a$id[a$len == 2]), c("A", "B"))
  expect_equal(sum(as.numeric(sf::st_length(a[a$len == 2, ]))), 8)
})

test_that("aw returns visibly with net", {
  data(net)
  data(pc_profile)
  p1 <- pc_profile[, 1]
  n3 <- sf::st_as_sf(net[1:3, ])
  v <- withVisible(aw(pc = n3$ldv, lcv = n3$ldv, hgv = n3$hdv, bus = n3$hdv,
    mc = n3$ldv, p_pc = p1, p_lcv = p1, p_hgv = p1,
    p_bus = p1, p_mc = p1, net = n3))
  expect_true(v$visible)
  expect_true(inherits(v$value, "sf"))
})

test_that("to_latex handles one column and escapes", {
  expect_output(to_latex(data.frame(PC_G = 1:2)), "PC\\\\_G \\\\\\\\ \\\\midrule")
  out <- capture.output(to_latex(data.frame(a = c("x%y", "z\\w"), n = 1:2)))
  expect_true(any(grepl("x\\%y & 1", out, fixed = TRUE)))
  expect_true(any(grepl("z\\textbackslash{}w & 2", out, fixed = TRUE)))
  f <- tempfile(fileext = ".tex")
  expect_output(to_latex(data.frame(a = 1:2, b = 3:4), file = f), ".tex")
  expect_true(any(grepl("\\end{table}", readLines(f), fixed = TRUE)))
})

test_that("vein_notes reports memory in Mb", {
  f <- vein_notes("x", file = file.path(tempdir(), "notes"))
  m <- grep("Memory used", readLines(f), value = TRUE)
  expect_true(grepl("Mb$", m))
  file.remove(f)
})

test_that("deprecated functions point to alternatives", {
  expect_warning(inventory(), "get_project")
  expect_warning(wide_to_long(), "data.table::melt.data.table")
})

test_that("colplot uses all_values and ylim", {
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(colplot(data.frame(a = c(0, 1, 2), b = c(1, 0, 3)),
    all_values = TRUE, ylim = c(0, 10)))
})

test_that("get_project downloads and extracts url", {
  src <- file.path(tempdir(), "proj_src")
  dir.create(src, showWarnings = FALSE)
  writeLines("x <- 1", file.path(src, "main.R"))
  tf <- file.path(tempdir(), "proj.tar.gz")
  old <- setwd(src)
  utils::tar(tf, files = "main.R", compression = "gzip")
  setwd(old)
  d <- file.path(tempdir(), "proj_dst")
  unlink(d, recursive = TRUE)
  expect_message(r <- get_project(d, url = paste0("file://", tf)), "directory")
  expect_true(file.exists(file.path(d, "main.R")))
  expect_equal(r, d)
  expect_error(get_project(d, url = "file:///nothing.txt"), "url")
  expect_error(get_project(d, case = "nothing"), "not supported")
})
