if(!require(rmarkdown)){install.packages("rmarkdown")}
rmarkdown::render("~/buildsNoBkup/covid19_mde/covid19_mde.Rmd")
rmarkdown::render("~/buildsNoBkup/covid19_mde/covid19_superficies.Rmd")
