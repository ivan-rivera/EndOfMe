# Compile report
cd project/dashboards/
R -e "rmarkdown::render('sleepdash.Rmd')"
cd ../../