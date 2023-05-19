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

tok <- tokens(corpus_MR, remove_punct = TRUE)
tok <- tokens_remove(tok, stopwords("en"))
dtm <- dfm(tok, tolower = TRUE)
dtm <- dfm_trim(dtm, min_docfreq = 10)

res <- rainette(dtm, k =4, min_segment_size = 15)

rainette_explor(res, dtm, corpus_MR)

rainette_plot(res, dtm, k = 4)

docvars(corpus_MR)$cluster <- cutree(res, k = 4)

# Validação utilizando dois clusters
res1 <- rainette(dtm, k = 4, min_segment_size = 12)
res2 <- rainette(dtm, k = 4, min_segment_size = 15)
res <- rainette2(res1, res2, max_k = 4)

rainette2_explor(res, dtm, corpus_MR)



# ------------------------------------------------------------------------------

# Extrair apenas a segunda coluna do dataframe bib_abstract
text_data <- bib_abstract[, 2]

# Criar um corpus
corpus <- Corpus(VectorSource(text_data))

# Limpar e processar o texto
corpus_clean <- tm_map(corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords("english"))

# Lematizar o texto
corpus_lemma <- tm_map(corpus_clean, stemDocument)

# Criar uma matriz de termos e documentos
tdm <- TermDocumentMatrix(corpus_lemma)
tdm <- as.matrix(tdm)

chd_results <- HCPC(tdm, graph = FALSE)

# -----------------------------------------------------------------------------

# Criar uma tabela de contingência dos lexemas
contingency_table <- tdm %*% t(tdm)

afc_results <- CA(contingency_table, graph = FALSE)
print(afc_results)
plot(afc_results)


