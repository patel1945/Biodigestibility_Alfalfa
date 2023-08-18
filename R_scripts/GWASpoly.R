
library(GWASpoly)
library(scam)

pheno=read.csv("/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST/pheno_Bs.csv")
str(pheno)
pheno$Env=as.factor(pheno$Env)
head(pheno)
levels(pheno$Env)
colnames(pheno)
trait1 <- colnames(pheno)[2:16]
trait1
length(trait1)

params <- set.params(fixed="Env",
                     fixed.type="factor")
models_1 <- c("general", "additive", "1-dom", "2-dom",  "diplo-additive", "diplo-general")




data <- read.GWASpoly(ploidy=4, 
                      pheno.file="/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST/pheno_Bs.csv", 
                      geno.file="/home/hawkins/Documents/git/big_files_git/Norberg_2.txt",
                      format="numeric", n.traits=15, delim=",")

data_2 <- set.K(data = data, LOCO = T, n.core = 10)
data_3 <- GWASpoly(data = data_2, models = models_1, traits = trait1, params = params, n.core = 30)

save(data_3, file = "/home/hawkins/Desktop/Smit/Biodigestibilty/Pheno_GWAS_fitted.RData")

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
sig_M=set.threshold(data = data_3, method = 'M.eff', level = 0.05)


##Manhattan plots
p= manhattan.plot(sig_B,traits="ERD6") #repeat for each trait in trait1
p + theme(axis.text.x = element_text(angle=90,vjust=0.5))


##QTL
q= LD.plot(sig_B, max.loci=1000)
q + xlim(0,30) 



