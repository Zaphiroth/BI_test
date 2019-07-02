
mappingData <- read.xlsx("examples/Mapping.xlsx")
colnames(mapping_table) <- tolower(colnames(mapping_table))

en_cn <- read.xlsx("examples/Mapping.xlsx",
                   sheet = "list")

save(mappingData, en_cn, file = "./data/mappingData.RData")
