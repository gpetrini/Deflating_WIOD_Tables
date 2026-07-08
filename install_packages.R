## Installs the CRAN packages listed in requirements.txt.
## Run from the repository root or from code/:
##   Rscript install_packages.R

req_file <- if (file.exists("requirements.txt")) "requirements.txt" else "../requirements.txt"

pkgs <- readLines(req_file) |> trimws()
pkgs <- pkgs[nzchar(pkgs)]

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) == 0) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
