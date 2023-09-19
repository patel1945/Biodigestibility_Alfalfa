# tidy pheno to include PC

pheno=read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_Bs.csv")

str(pheno)
pheno$Env=as.factor(pheno$Env)
head(pheno)
levels(pheno$Env)
colnames(pheno)
p1 <- pheno %>% dplyr::filter(Env == "ID_2020")
p2 <- pheno %>% dplyr::filter(Env == "OR_2020")
p3 <- pheno %>% dplyr::filter(Env == "OR_2021")
p4 <- pheno %>% dplyr::filter(Env == "WA_2020")
p5 <- pheno %>% dplyr::filter(Env == "WA_2021")
colnames(p1)[2:16] <- paste0("ID_2020_", colnames(p1)[2:16])
colnames(p2)[2:16] <- paste0("OR_2020_", colnames(p2)[2:16])
colnames(p3)[2:16] <- paste0("OR_2021_", colnames(p3)[2:16])
colnames(p4)[2:16] <- paste0("WA_2020_", colnames(p4)[2:16])
colnames(p5)[2:16] <- paste0("WA_2021_", colnames(p5)[2:16])

p1 <- p1 %>% dplyr::select(-Env)
p2 <- p2 %>% dplyr::select(-Env)
p3 <- p3 %>% dplyr::select(-Env)
p4 <- p4 %>% dplyr::select(-Env)
p5 <- p5 %>% dplyr::select(-Env)

p6 <- inner_join(p1, p2, by = "Treatment") %>% inner_join(., p3, by = "Treatment") %>% inner_join(., p4, by = "Treatment") %>% inner_join(., p5, by = "Treatment")
colnames(p6)[1] <- "gen"

PCA1 <- read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_Bs.csv")


p6 <- inner_join(p6, PCA1, by = "gen")
colnames(p6)
write.csv(p6, "~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_1.csv", quote = F, row.names = F)

# pheno1 contains 79 cols:
# 1 col for ID (gen)
# 3 cols for 3 PCs (3 last cols)
# 15 traits by 5 env: 75 cols

