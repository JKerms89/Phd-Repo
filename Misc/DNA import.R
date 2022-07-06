install.packages("statnet")
install.packages("xergm")
install.packages("igraph")
install.packages("cluster")
install.packages("rJava")
install.packages("remotes")
setwd("C:/OneDrive - Jan/Desktop/Bild")
library("rDNA")
set.seed(12345)
dna_init()
dna_gui("RCA_dna_test.dna")
conn <- dna_connection("RCA_dna_test.dna")
dn <- dna_network(conn,
                  networkType = "twomode",
                  statementType = "DNA Statement",
                  variable1 = "C_Org",
                  variable2 = "O_Nat+Type",
                  qualifier = "EU_Evaluation",
                  duplicates = "document")
dna_barplot(conn, variable1 = "C_Org",
            variable2 = "O_Nat+Type",
            qualifier = "EU_Evaluation", of = "O_Nat+Type", fontSize = 10)


affil <- dna_network(conn,
                     networkType = "twomode",
                     statementType = "DNA Statement",
                     variable1 = "C_Org",
                     variable2 = "O_Nat+Type",
                     qualifier = "EU_Evaluation",
                     qualifierAggregation = "combine",
                     duplicates = "document",
                     verbose = TRUE)

library(network)
nw <- network(affil, bipartite = TRUE)
colors <- as.character(t(affil))
colors[colors == "1"] <- "deepskyblue"
colors[colors == "0"] <- "indianred"
colors[colors == "-1"] <- "#31a354"
colors <- colors[colors != "0"]
set.edge.attribute(nw, "color", colors)
plot(nw, edge.col = get.edge.attribute(nw, "color"),
vertex.col = c(rep("white", nrow(affil)),
rep("black", ncol(affil))),
displaylabels = TRUE,
label.cex = 0.5
)
