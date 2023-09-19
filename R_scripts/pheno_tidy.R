
library(readxl)
library(dplyr)
library(tidyr)


# setwd("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/")
setwd("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/")

BLUE_CP <- read_excel("BLUE_CP.xlsx")
BLUE_DM <- read_excel("BLUE_DM.xlsx")
BLUE_Ash <- read_excel("BLUE_Ash.xlsx")
BLUE_aNDForm <- read_excel("BLUE_aNDForm.xlsx")
BLUE_ADICP <- read_excel("BLUE_ADICP.xlsx")
BLUE_NDICP <- read_excel("BLUE_NDICP.xlsx")
BLUE_SP <- read_excel("BLUE_SP.xlsx")
BLUE_A2 <- read_excel("BLUE_A2.xlsx")
BLUE_B1 <- read_excel("BLUE_B1.xlsx")
BLUE_B2 <- read_excel("BLUE_B2.xlsx")
BLUE_C <- read_excel("BLUE_C.xlsx")
BLUE_A <- read_excel("BLUE_A.xlsx")
BLUE_B <- read_excel("BLUE_B.xlsx")
BLUE_kd <- read_excel("BLUE_kd.xlsx")
BLUE_ERD6 <- read_excel("BLUE_ERD6.xlsx")


#CP
BLUE_CP=BLUE_CP[,c(2,3,5,7,9,11)]
BLUE_CP_long=BLUE_CP%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "CP")

#DM
BLUE_DM=BLUE_DM[,c(2,3,5,7,9,11)]
BLUE_DM_long=BLUE_DM%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "DM")

#Ash
BLUE_Ash=BLUE_Ash[,c(2,3,5,7,9,11)]
BLUE_Ash_long=BLUE_Ash%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "Ash")

#aNDForm
BLUE_aNDForm_long=BLUE_aNDForm[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "aNDForm")
#ADICP
BLUE_ADICP_long=BLUE_ADICP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "ADICP")

#NDICP
BLUE_NDICP_long=BLUE_NDICP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "NDICP")

#SP
BLUE_SP_long=BLUE_SP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "SP")

#A2
BLUE_A2_long=BLUE_A2[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "A2")

#B1
BLUE_B1_long=BLUE_B1[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B1")

#B2
BLUE_B2_long=BLUE_B2[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B2")

#C
BLUE_C_long=BLUE_C[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "C")

#A
BLUE_A_long=BLUE_A[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "A")

#B
BLUE_B_long=BLUE_B[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B")

#kd
BLUE_kd_long=BLUE_kd[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "kd")

#ERD6
BLUE_ERD6_long=BLUE_ERD6[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "ERD6")

pheno_Bs=left_join(BLUE_DM_long,BLUE_CP_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_Ash_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_aNDForm_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_ADICP_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_NDICP_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_SP_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_A2_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_B1_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_B2_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_C_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_A_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_B_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_kd_long,by=c('Treatment', 'Env'))%>% 
  left_join(BLUE_ERD6_long, by=c('Treatment', 'Env'))
setwd("/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST")
getwd()
pheno_Bs=pheno_Bs%>%select(Treatment, DM, CP, Ash, aNDForm, ADICP, NDICP, SP,A2,B1, B2,C,A,B, kd, ERD6, Env)
write.csv(pheno_Bs, file = "pheno_Bs.csv", row.names = FALSE)


head(BLUE_ERD6)
colnames(BLUE_ERD6)

list1 <- list(BLUE_A, BLUE_A2, BLUE_ADICP, BLUE_aNDForm, BLUE_Ash, BLUE_B, BLUE_B1, BLUE_B2, BLUE_C, BLUE_CP, BLUE_ERD6, BLUE_kd, BLUE_NDICP, BLUE_SP)

# 14 traits. DM was not included

names1 <- c("A","A2","ADICP","aNDForm","Ash","B","B1","B2","C","CP","ER6","kd","NDICP","SP")
names(list1) <- names1
# A A2 ADICP aNDForm Ash B B1 B2 C CP ER6 kd NDICP SP


list2 <- list()
for (i in 1:length(list1)) {
  a1 <- list1[[i]]
  a2 <- a1 %>% dplyr::select(2,3,4) # ID_2020
  a3 <- a1 %>% dplyr::select(2,5,6) # OR_2020
  a4 <- a1 %>% dplyr::select(2,7,8) # OR_2021
  a5 <- a1 %>% dplyr::select(2,9,10) # WA_2020
  a6 <- a1 %>% dplyr::select(2,11,12) # WA_2021
  
  a2$env <- "ID_2020"
  a3$env <- "OR_2020"
  a4$env <- "OR_2021"
  a5$env <- "WA_2020"
  a6$env <- "WA_2021"
  
  colnames(a2) <- c("gen", "BLUE", "weight", "env")
  colnames(a3) <- c("gen", "BLUE", "weight", "env")
  colnames(a4) <- c("gen", "BLUE", "weight", "env")
  colnames(a5) <- c("gen", "BLUE", "weight", "env")
  colnames(a6) <- c("gen", "BLUE", "weight", "env")
  
  a7 <- rbind(a2,a3,a4,a5,a6)
  
  setwd("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST1/")
  write.csv(a7, paste0(names1[i], ".csv"), row.names = F, quote = F)
  
  list2[[length(list2) + 1]] <- a7
}

names(list2) <- names1

a1 <- BLUE_ERD6



a2 <- a1 %>% dplyr::select(2,3,4) # ID_2020
a3 <- a1 %>% dplyr::select(2,5,6) # OR_2020
a4 <- a1 %>% dplyr::select(2,7,8) # OR_2021
a5 <- a1 %>% dplyr::select(2,9,10) # WA_2020
a6 <- a1 %>% dplyr::select(2,11,12) # WA_2021

a2$env <- "ID_2020"
a3$env <- "OR_2020"
a4$env <- "OR_2021"
a5$env <- "WA_2020"
a6$env <- "WA_2021"

colnames(a2) <- c("gen", "BLUE", "weight", "env")
colnames(a3) <- c("gen", "BLUE", "weight", "env")
colnames(a4) <- c("gen", "BLUE", "weight", "env")
colnames(a5) <- c("gen", "BLUE", "weight", "env")
colnames(a6) <- c("gen", "BLUE", "weight", "env")

a7 <- rbind(a2,a3,a4,a5,a6)
