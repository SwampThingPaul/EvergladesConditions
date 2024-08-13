
path = "C:/Julian_LaCie/_GitHub/EvergladesConditions/report/"
library(quarto)
quarto_render(paste0(path,"LOK_PSC_figs.qmd"))


library(pagedown)
pdf.fname = paste0(format(Sys.Date(),"%Y%m%d"),"_LOKPSC.pdf")
chrome_print(paste0(path,"LOK_PSC_figs.html"),
             paste0(path,"pdfs/",pdf.fname),
             format = "pdf")
