library(GenomicRanges)
library(genomation)
library(plyranges)
library(Repitools)
library(data.table)

# if (!require("BiocManager", quietly = TRUE))
#   install.packages(c("BiocManager"))
# 
# BiocManager::install(c("genomation","plyranges","Repitools"))

####################
# # GRanges
load("~/Documents/git/big_files/i_5.2.8.RData")
# i_5.2.8 <- i_5.2.8 %>% dplyr::select(1,3)
# 
# 
file <- ("~/Documents/git/big_files/blast_corrected_shen.bed")
txdb <- readBed(file, track.line = FALSE, remove.unusual = FALSE,
                zero.based = TRUE)

col_headings_1 <- c('gene_id',	'uniprot', 'degradation',	'blastp_match_flag')
col_headings_2 <- c('gene_id',	'isoform')

data_4 <- QTL_02
gr5 <- GRanges(seqnames = data_4$Chrom,
               ranges = IRanges(data_4$Position, width = 1))

overlaps <- join_overlap_left(gr5, txdb)

df2 <- annoGR2DF(overlaps)
df2 <- unite(data = df2, col = "Marker1", 1:2, sep = "_", remove = F) %>% distinct(Marker1, .keep_all = TRUE) %>% dplyr::select(1:4,7) 

head(df2)

df3 <- df2 %>% separate(5, col_headings_1, sep = ";", remove = TRUE, convert = FALSE, extra = "warn") %>% separate(5, col_headings_2, sep = "\\.", remove = TRUE, convert = FALSE, extra = "warn") %>% inner_join(., i_5.2.8, by = c("gene_id", "uniprot")) %>% dplyr::select(1,7,10)


S2 <- QTL_01 %>% unite(col = "Marker1", 5:6, sep = "_", remove = F) %>% dplyr::select(1,4,5,10,11) %>% distinct(Marker1, Trait, .keep_all = T)


# R2
# 
# cc <- count(QTL_01,Trait)
# lev4 <- cc$Trait
# lev4
# cc1 <- count(QTL_01, Model)
# cc1$Model
# 
# QTL_3 <- QTL_01 %>% dplyr::filter(!Model %in% c("diplo-general", "diplo-additive"))
# 
# 
# fit_05 <- fit.QTL(data=data_5, trait = "ID_2020_ADICP",
#                   qtl=QTL_3[,c("Marker","Model")])
# 
# fit_06 <- fit_05 %>% unite(col = "Marker1", 2:3, sep = "_", remove = T) %>% dplyr::select(2,4) %>% distinct(Marker1, .keep_all = T)
# 
# df4 <- left_join(S2, fit_06, by = "Marker1") %>% left_join(., df3, by = "Marker1")
# 
# write.csv(df4, "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/anno_smit.csv", quote = F, row.names = F)


df4 <- left_join(S2, df3, by = "Marker1")
write.csv(df4, "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/anno_ces_ST2.csv", quote = F, row.names = F)
