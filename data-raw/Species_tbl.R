##Species Table

DP2R(Tables = c("Species", "CommonName"))

Species = merge(Species, CommonName[,c("common_name_id","common_name")], by = "common_name_id")
Species = Species[,-1]#remove id column
Species = Species[,c("common_name",setdiff(names(Species),"common_name"))]#move common name first

usethis::use_data(Species, overwrite = TRUE)
