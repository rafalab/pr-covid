fn <- paste0("informe-", lubridate::today(),".html")
dir <- "../../docs/"
rmarkdown::render("informe.Rmd", output_format = "html_document", output_dir = dir, 
                  output_file = fn, clean = TRUE)


fns <- list.files(dir, pattern = "informe")
fns <- sort(fns, decreasing=TRUE)
name <- gsub(".html", "", fns)
cat("##  Informes Diarios Sobre COVID19 Coalición Científica de Puerto Rico\n\n", file = file.path(dir, "index.md"))
for(i in seq_along(name)){
  cat(paste0("* [", name[i], "](", fns[i], ")\n"), file = file.path(dir, "index.md"), append = TRUE)
}
