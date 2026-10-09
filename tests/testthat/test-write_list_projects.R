# Regression tests for LIST/ListProjects.txt generation.

# Minimal AquaCrop project skeleton: .write_prm() only checks that the
# mandatory climate/management/soil files exist, so empty stubs are enough
# as long as crop_duration is supplied (otherwise the .CRO is parsed).
local_prm_project <- function(sites, env = parent.frame()) {
  proj <- withr::local_tempdir(.local_envir = env)
  for (d in c("CLIMATE", "MANAGEMENT", "SOIL", "CROP", "LIST")) {
    dir.create(file.path(proj, d), recursive = TRUE)
  }
  for (s in sites) {
    file.create(file.path(proj, "CLIMATE", paste0(s, c(".CLI", ".Tnx", ".ETo", ".PLU"))))
    file.create(file.path(proj, "MANAGEMENT", paste0(s, ".MAN")))
    file.create(file.path(proj, "SOIL", paste0(s, c(".SOL", ".SW0"))))
  }
  file.create(file.path(proj, "CROP", "maize.CRO"))
  proj
}

test_that(".write_list_projects() lists the .PRM files, sorted, one per line", {
  path <- withr::local_tempdir()
  file.create(file.path(path, c("grid_002.PRM", "grid_001.PRM", "notes.txt")))

  out <- .write_list_projects(path, eol = "linux")

  expect_identical(out, file.path(path, "ListProjects.txt"))
  expect_identical(readLines(out), c("grid_001.PRM", "grid_002.PRM"))
})

test_that(".write_list_projects() honours eol and handles empty/missing dirs", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "grid_001.PRM"))

  raw_txt <- readr::read_file(.write_list_projects(path, eol = "windows"))
  expect_identical(raw_txt, "grid_001.PRM\r\n")

  empty <- withr::local_tempdir()
  expect_identical(readr::read_file(.write_list_projects(empty)), "")

  expect_null(.write_list_projects(file.path(path, "does-not-exist")))
})

test_that("write_prm() writes ListProjects.txt unless update_list = FALSE", {
  proj  <- local_prm_project("grid_001")
  plsch <- data.frame(year = 1990:1992, planting_doy = 180)

  withr::with_dir(proj, {
    write_prm(
      site_name         = "grid_001",
      planting_schedule = plsch,
      crop_name         = "maize",
      crop_duration     = 90
    )
    expect_identical(readLines("LIST/ListProjects.txt"), "grid_001.PRM")

    unlink("LIST/ListProjects.txt")
    write_prm(
      site_name         = "grid_001",
      planting_schedule = plsch,
      crop_name         = "maize",
      crop_duration     = 90,
      update_list       = FALSE
    )
    expect_false(file.exists("LIST/ListProjects.txt"))
  })
})

test_that("write_prm_batch() lists every site once", {
  sites <- c("grid_002", "grid_001")
  proj  <- local_prm_project(sites)
  plsch <- data.frame(year = 1990:1992, planting_doy = 180)

  withr::with_dir(proj, {
    write_prm_batch(
      site_name         = sites,
      crop_name         = "maize",
      planting_schedule = plsch,
      crop_duration     = 90,
      irrigation_path   = NULL,
      verbose           = FALSE
    )
    expect_identical(
      readLines("LIST/ListProjects.txt"),
      c("grid_001.PRM", "grid_002.PRM")
    )
  })
})
