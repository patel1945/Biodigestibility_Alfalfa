
library(GWASpoly)
library(scam)
library(tidyverse)

pheno=read.csv("/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST/pheno_Bs.csv")

pheno=read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_1.csv")

pheno=read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_Bs.csv")

trait1 <- colnames(pheno)[2:16]

# pheno1
trait1 <- colnames(pheno)[2:76]

trait1
length(trait1)

params <- set.params(fixed="Env",
                     fixed.type="factor")
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")

# params <- set.params(fixed=c("PC1","PC2","PC3"),
#                      fixed.type=rep("numeric",3), n.PC = 3)


data <- read.GWASpoly(ploidy=4, 
                      pheno.file="~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_Bs.csv", 
                      geno.file="~/Documents/git/big_files/Norberg_2.txt",
                      format="numeric", n.traits=length(trait1), delim=",")

data_2 <- set.K(data = data, LOCO = T, n.core = 8)

## pheno1.csv
# data_3_cm <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 8)
# save(data_3_cm, file = "~/Documents/git/big_files/data_3_cm.RData")
load("~/Documents/git/big_files/data_3_cm.RData")

# pheno_Bs.csv
Pheno_GWAS_fitted <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 8)
save(Pheno_GWAS_fitted, file = "~/Documents/git/big_files/Pheno_GWAS_fitted.RData")

data_5 <- set.threshold(Pheno_GWAS_fitted, method= "Bonferroni", level=0.05)


library(ggplot2)
qq.plot(data = data_3, trait = 'DM')
qq.plot(data = data_3, trait = 'CP')
qq.plot(data = data_3, trait = 'Ash')
qq.plot(data = data_3, trait = 'aNDForm')
qq.plot(data = data_3, trait = 'ADICP')
qq.plot(data = data_3, trait = 'NDICP')
qq.plot(data = data_3, trait = 'SP')
qq.plot(data = data_3, trait = 'A2')
qq.plot(data = data_3, trait = 'B1')
qq.plot(data = data_3, trait = 'B2')
qq.plot(data = data_3, trait = 'C')
qq.plot(data = data_3, trait = 'A')
qq.plot(data = data_3, trait = 'B')
qq.plot(data = data_3, trait = 'kd')
qq.plot(data = data_3, trait = 'ERD6')

##Significance of markers
load("/home/hawkins/Desktop/Smit/Biodigestibilty/Pheno_GWAS_fitted.RData")
sig_B=set.threshold(data = data_3, method = 'Bonferroni', level = 0.05)
# sig_M=set.threshold(data = data_3, method = 'M.eff', level = 0.05)

data_5 <- set.threshold(Pheno_GWAS_fitted, method= "Bonferroni", level=0.05)

data_5 <- set.threshold(data_3_cm, method= "Bonferroni", level=0.05)

QTL_01 <- get.QTL(data_5)
QTL_02 <- QTL_01 %>% distinct(Marker, .keep_all = T) 


write.csv(QTL_01, "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/QTL_01.csv", quote = F, row.names = F)

write.csv(QTL_02, "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/QTL_02.csv", quote = F, row.names = F)


##Manhattan plots

QTL_01 <- get.QTL(data_5)
traits1 <- unique(QTL_01$Trait)

data_5@map$Chrom <- gsub("Chr", "", data_5@map$Chrom)
data_5@map$Chrom <- as.factor(data_5@map$Chrom)

manhattan.plot(data_5, traits = traits1) #repeat for each trait in trait1
p + theme(axis.text.x = element_text(angle=90,vjust=0.5))

p1 <- manhattan.plot(data = data_5, traits = traits1) + theme_classic(base_family = "Arial", base_size = 12) + scale_color_manual(values=c("royalblue2","gray70")) + theme(legend.position = "none", axis.title.y = element_text(size = 12), plot.tag = element_blank(), strip.background = element_blank())

# file too big
# ggsave(filename = "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/Manhattan_ST2.pdf", plot = p1, dpi = 300, width = 10, height = 6) 

ggsave(filename = "~/Documents/git/Biodigestibility_Alfalfa/GWAS_results/Manhattan_ST1.jpg", plot = p1, dpi = 300, width = 7, height = 6)


