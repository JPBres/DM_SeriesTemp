# Generacion Documento TP

# http://rmarkdown.rstudio.com/
# http://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf
workingDirectory = '/DM/stiempo/TP' 
setwd(workingDirectory)

#install.packages("knitr")
library(knitr); 

knit('tp_final.Rmd'); 
pandoc('tp_final.md','latex');
#pandoc('tp_exploracion.md','html');
