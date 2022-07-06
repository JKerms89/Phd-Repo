library(dplyr)


########

dfDE <- OMN_DEnet_Stats_gexf %>% 
  arrange(desc(eigencentrality)) %>%
  slice(1:25) %>%
  select(Id,eigencentrality) %>%
  rename(Id_DE = Id,
         eigencentrality_DE = eigencentrality)

dfIT <- OMN_ITnet_Stats_gexf %>% 
  arrange(desc(eigencentrality)) %>%
  slice(1:25) %>%
  select(Id, eigencentrality) %>%
  rename(Id_IT = Id,
         eigencentrality_IT = eigencentrality)

dfNL <- OMN_NLnet_Stats_gexf %>% 
  arrange(desc(eigencentrality)) %>%
  slice(1:25) %>%
  select(Id, eigencentrality) %>%
  rename(Id_NL = Id,
         eigencentrality_NL = eigencentrality)


dfPL <- OMN_PLnet_Stats_gexf %>% 
  arrange(desc(eigencentrality)) %>%
  slice(1:25) %>%
  select(Id, eigencentrality) %>%
  rename(Id_PL = Id,
         eigencentrality_PL = eigencentrality)

dfagg <- OMN_EUnet_Stats %>% 
  arrange(desc(eigencentrality)) %>%
  slice(1:25) %>%
  select(Id, eigencentrality) %>%
  rename(Id_agg = Id,
         eigencentrality_agg = eigencentrality)

degdf <- cbind(dfDE, dfIT, dfNL, dfPL, dfagg)
write.xlsx(as.data.frame(degdf), 'eigencentralitydf.xlsx', row.names = FALSE)






