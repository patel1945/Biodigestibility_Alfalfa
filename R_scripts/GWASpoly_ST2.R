library(GWASpoly)
library(tidyverse)

pheno=read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_2.csv")

trait1 <- colnames(pheno)[2:85]
length(trait1)

models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")

params <- set.params(fixed=c("PC1","PC2","PC3"),
                     fixed.type=rep("numeric",3), n.PC = 3)

data <- read.GWASpoly(ploidy=4, 
                      pheno.file="~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_2.csv", 
                      geno.file="~/Documents/git/big_files/Norberg_2.txt",
                      format="numeric", n.traits=length(trait1), delim=",")

data_2 <- set.K(data = data, LOCO = T, n.core = 8)

## pheno2.csv
data_3_st2 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 8)

# save(data_3_st2, file = "~/Documents/git/big_files/data_3_st2.RData")
load("~/Documents/git/big_files/data_3_st2.RData")

data_5 <- set.threshold(data_3_st2, method= "Bonferroni", level=0.1)

QTL_01 <- get.QTL(data_5)
QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 

write.csv(QTL_01, "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/QTL_01_ST2.csv", quote = F, row.names = F)



traits1 <- unique(QTL_01$Trait)


data_5@map$Chrom <- gsub("Chr", "", data_5@map$Chrom)
data_5@map$Chrom <- as.factor(data_5@map$Chrom)


p1 <- manhattan.plot(data = data_5, traits = traits1) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

# file too big
ggsave(filename = "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/Manhattan_ST2.pdf", plot = p1, dpi = 300, width = 10, height = 6) 

ggsave(filename = "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/Manhattan_ST2.jpg", plot = p1, dpi = 300, width = 7, height = 6)

