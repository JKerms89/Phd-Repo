# connect to own database named "my_data.dna" as an example
library(rDNA)
dna_init()
conn <- dna_connection("my_data.dna")

# read in and convert docx files
library("LexisNexisTools")
# Look for DOCX files in current working directory
my_files <- list.files(pattern = "DOCX", ignore.case = TRUE)
# read files to R
LNToutput <- lnt_read(my_files)
# And this object can be converted to work in rDNA
docs <- lnt_convert(LNToutput, to = "rDNA")

# add docs to dna db
dna_setDocuments(conn, documents = docs, simulate = FALSE)

#If author and length is missing.
LNToutput <- lnt_read(my_files, author_keyword = "^Byline:", length_keyword = "LENGTH", verbose = FALSE)

# Alternative code to import newspaper articles but w/o converting dates
library("LexisNexisTools")
my_files <- list.files(pattern = "DOCX", ignore.case = TRUE)
LNToutput <- lnt_read(my_files, author_keyword = "^Byline:", verbose = FALSE)

#More than one language was detected. Choose one: 
# 1: Don't convert dates 2: Dutch (34.01%) 3: German (31.4%) 4: Spanish (14.24%)
# Selection: 1
View(LNToutput)
d <- LNToutput@meta[["Date"]]
library(stringi)
d_new <- stri_datetime_parse(d, format = "d MM yyyy",lenient = FALSE,tz = NULL, locale = NULL)
d_df <- as.data.frame(d_new)
docs <- lnt_convert(LNToutput, to = "rDNA")
d_df2 <- tibble::rowid_to_column(d_df, "id")
colnames(d_df2) <- c('id', 'date')
docs$date <- d_df2$date
View(docs)
setwd("C:/Users/kerme/OneDrive - LUISS Libera Università Internazionale degli Studi Sociali Guido Carli/PhD/PhD DATASET")
library(rDNA)
dna_init()
conn <- dna_connection("DNA_database.dna")
dna_setDocuments(conn, documents = docs, simulate = FALSE)

# First, import the different articles from different newspapers, and clean the
#dataset accordingly using dplyr ecc. Make an r object for each language-country
# set of articles (e.g. docPL, docDE, docNL, docIT)

# Afterwards, rbind the documents (as per below)
doc_updated <- rbind(docsDE,docsPL)

# Amend id no. so it is in order by rownumber.
doc_updated$id <- 1:nrow(doc_updated)

#Amend aggregate doc so that notes ID is in logical order. Remember to amend 
# the sequence according to the max. number of rows.

doc_updated$notes <- sprintf("ID: %d", seq(1:773))

#How to remove rows in R using dplyr
doc_updated2 <- select(doc_updated, -name)

#Add articles to the DNA.
library(rDNA)
dna_init()
conn <- dna_connection("depl.dna")
dna_setDocuments(conn, documents = doc_updated2, simulate = FALSE)

