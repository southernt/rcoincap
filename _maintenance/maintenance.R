#### Script for Building, Installing, & Misc. Maintenance ####

## set working directory
setwd("~/Programming/rcoincap")
wd <- getwd()

## identify base libraries
df_base_lib <- as.data.frame(installed.packages(.Library, priority = "base"))
base_lib_vctr <- sort(unique(rownames(df_base_lib)))

## package directory
pkg_dir <- "~/Programming/rcoincap"

## libraries
library(devtools)
library(usethis)

## build package
devtools::build(pkg_dir, vignettes = TRUE)
devtools::build(pkg_dir, binary = TRUE, vignettes = TRUE)

## install package
devtools::install(build_vignettes = TRUE)


## dependencies
dep_dirs <- paste(pkg_dir, c("R"), sep = "/")
df_depend <- lapply(dep_dirs, function(x) {
  dat <- renv::dependencies(x)
  dat <- dplyr::mutate(dat, Source = gsub(paste0(wd, "/"), "", Source))
  dat <- dplyr::distinct(dat)
})
df_depend <- dplyr::bind_rows(df_depend)
df_depend <- dplyr::filter(df_depend, Package != "fbfs")
all_pkgs <- sort(unique(df_depend$Package))
all_pkgs <- setdiff(all_pkgs, setdiff(base_lib_vctr, "parallel"))

## mandatory imports
import_pkgs <- unique(df_depend[startsWith(df_depend$Source, "R/"), "Package"])
import_pkgs <- sort(setdiff(import_pkgs, setdiff(base_lib_vctr, "parallel")))
if (length(import_pkgs) > 0) {
  purrr::walk(import_pkgs, usethis::use_package)
}

## suggested imports
suggest_pkgs <- sort(unique(setdiff(all_pkgs, import_pkgs)))
if (length(suggest_pkgs) > 0) {
  purrr::walk(suggest_pkgs, usethis::use_package, type = "Suggests")
}


## use package logo
logo_fp <- file.path(pkg_dir, "data-raw", "hex-rcoincap.png")
usethis::use_logo(logo_fp, geometry = "240x278", retina = TRUE)

