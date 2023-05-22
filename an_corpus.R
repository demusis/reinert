library(bibtex)
library(plyr)
library(textstem)
library(tm)
library(FactoMineR)
library(XML)
library(dplyr)
library(purrr)
library(tibble)
library(quanteda)
library(rainette)


# Definir o diretório de trabalho
work_dir <- "~/Angelica/Nova pasta (2)/My Collection - Poluição e Aerossóis"
setwd(work_dir)

# ------------------------------------------------------------------------------

# Caminho para o arquivo BibTeX
bib_file <- "My Collection - Poluição e Aerossóis.bib"

# Ler o arquivo BibTeX
bib_data <- read.bib(bib_file)

# Extrair os campos das entradas BibTeX
bib_abstract <- ldply(bib_data$abstract)

# -----------------------------------------------------------------------------

# http://www.numdam.org/item/?id=CAD_1983__8_2_187_0
# https://doi.org/10.1177/075910639002600103
# https://doi.org/10.1177/075910639002600103

colnames(bib_abstract) <- c("id", "abstract")
corp_MR <- corpus(bib_abstract, 
                     text_field  = "abstract")
docnames(corp_MR) <- bib_abstract$id
print(corp_MR)

corpus_MR <- split_segments(corp_MR, segment_size = 40)

# Definir os lexemas a serem removidos
custom_stopwords <- c("$", "â", "¯", "95", "g", "=", "m3", "ci", "±", "3", "mâˆ",
                      "m", "pm10", "gâ", "€", "rr", "mu")

# Adicionar os lexemas à lista de stopwords em inglês
extended_stopwords <- c(stopwords("en"), custom_stopwords)

tok <- tokens(corpus_MR, remove_punct = TRUE)
tok <- tokens_remove(tok, extended_stopwords)
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 10)

res <- rainette(dtm, k =4, min_segment_size = 15)

# rainette_explor(res, dtm, corpus_MR)

rainette_plot(res, dtm, k = 4)

docvars(corpus_MR)$cluster <- cutree(res, k = 4)

groups <- cutree_rainette(res, k = 4)
rs <- rainette_stats(groups, dtm)

# Validação utilizando dois clusters
res1 <- rainette(dtm, k = 4, min_segment_size = 12)
res2 <- rainette(dtm, k = 4, min_segment_size = 15)
res1_2 <- rainette2(res1, res2, max_k = 4)

# rainette2_explor(res1_2, dtm, corpus_MR)







