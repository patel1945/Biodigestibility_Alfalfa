
library(readxl)
library(dplyr)
library(tidyr)

#CP
BLUE_CP <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_CP.xlsx")
BLUE_CP=BLUE_CP[,c(2,3,5,7,9,11)]
BLUE_CP_long=BLUE_CP%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "CP")
#DM
BLUE_DM <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_DM.xlsx")
BLUE_DM=BLUE_DM[,c(2,3,5,7,9,11)]
BLUE_DM_long=BLUE_DM%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "DM")
#Ash
BLUE_Ash <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_Ash.xlsx")
BLUE_Ash=BLUE_Ash[,c(2,3,5,7,9,11)]
BLUE_Ash_long=BLUE_Ash%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "Ash")
#aNDForm
BLUE_aNDForm <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_aNDForm.xlsx")
BLUE_aNDForm_long=BLUE_aNDForm[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "aNDForm")
#ADICP
BLUE_ADICP <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_ADICP.xlsx")
BLUE_ADICP_long=BLUE_ADICP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "ADICP")
#NDICP
BLUE_NDICP <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_NDICP.xlsx")
BLUE_NDICP_long=BLUE_NDICP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "NDICP")
#SP
BLUE_SP <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_SP.xlsx")
BLUE_SP_long=BLUE_SP[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "SP")
#A2
BLUE_A2 <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_A2.xlsx")
BLUE_A2_long=BLUE_A2[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "A2")
#B1
BLUE_B1 <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_B1.xlsx")
BLUE_B1_long=BLUE_B1[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B1")
#B2
BLUE_B2 <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_B2.xlsx")
BLUE_B2_long=BLUE_B2[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B2")
#C
BLUE_C <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_C.xlsx")
BLUE_C_long=BLUE_C[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "C")
#A
BLUE_A <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_A.xlsx")
BLUE_A_long=BLUE_A[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "A")
#B
BLUE_B <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_B.xlsx")
BLUE_B_long=BLUE_B[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "B")
#kd
BLUE_kd <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_kd.xlsx")
BLUE_kd_long=BLUE_kd[,c(2,3,5,7,9,11)]%>%pivot_longer(cols = c(2,3,4,5,6), names_to = "Env", values_to = "kd")
#ERD6
BLUE_ERD6 <- read_excel("~/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_ERD6.xlsx")
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
