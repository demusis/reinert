# install.packages("RefManageR")
# install.packages("openxlsx")

library(RefManageR)
library(openxlsx)

setwd("~/Olimpio")

refs <- ReadBib("artigos.bib")

refs_df <- as.data.frame(refs)

write.xlsx(refs_df, "artigos_r.xlsx")
