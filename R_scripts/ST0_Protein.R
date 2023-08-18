
#Stage 0 (Single trial analysis of phenotypic data)

##Tydying data for the analysis
library(dplyr)
##adding an column called env combining Location and year
library(readxl)
getwd()
Complete_Pred_Pheno <- read_excel("/home/hawkins/Desktop/Smit/Biodigestibilty/Complete_Pred_Pheno.xlsx")
View(Complete_Pred_Pheno)
Complete_Pred_Pheno=Complete_Pred_Pheno%>%mutate(Env=paste(Location, Year, sep ='_'))
str(Complete_Pred_Pheno)
##changing the class of following columns to factor
Columns=c("Sample","Block", "Column", "Row", "Position", "Treatment", "Location", "Year", "Env")
Complete_Pred_Pheno[Columns]=lapply(Complete_Pred_Pheno[Columns], factor)
Complete_Pred_Pheno$GH1=as.numeric(Complete_Pred_Pheno$GH1)



#checking for number of treatments
library(tidyverse)

c.1=Complete_Pred_Pheno%>%select(-1, -GH1, -Treatment_name)%>%dplyr::filter(Treatment %in% c(201))
c.2=Complete_Pred_Pheno%>%select(-1, -GH1, -Treatment_name)%>%dplyr::filter(Treatment %in% c(202))
head(c.1)
summary(c.1)

colnames(c.1)=c("N", "Sample", "DM_201", "CP_201", "Ash_201", "aNDForm_201", "ADICP_201", "NDICP_201", 
                "SP_201", "A2_201", "B1_201", "B2_201", "C_201", "A_201", "B_201", "kd_201",
                "ERD6_201", "Block", "Column", "Row", "Position", "Treatment", "Location", "Year",
                "Env")

colnames(c.2)=c("N", "Sample", "DM_202", "CP_202", "Ash_202", "aNDForm_202", "ADICP_202", "NDICP_202", 
                "SP_202", "A2_202", "B1_202", "B2_202", "C_202", "A_202", "B_202", "kd_202",
                "ERD6_202", "Block", "Column", "Row", "Position", "Treatment", "Location", "Year",
                "Env")
data=Complete_Pred_Pheno%>%select(-1, -GH1, -Treatment_name)

c.1=c.1%>%select(-N,-Sample,-Column, -Row, -Position, -Treatment, -Location, -Year)
c.2=c.2%>%select(-N,-Sample,-Column, -Row, -Position, -Treatment, -Location, -Year)

C <- full_join(data, c.1, by= c( "Block","Env"))%>% full_join(., c.2, by= c("Block","Env"))
C=C %>% filter(Env %in% c("ID_2020", "OR_2020", "OR_2021", "WA_2020", "WA_2021" ))%>%droplevels()
str(C)
nrow(C)
#C%>%select(Block,Env, Treatment)%>%filter(Treatment==201)%>%group_by(Env)%>%count()

library(xlsx)
write.xlsx2(C,"/home/hawkins/Desktop/Smit/Biodigestibilty/C.xlsx")

##preparing for Modeling using asreml
str(C)

getwd()
setwd("/home/hawkins/Desktop/Smit/Biodigestibilty/split_data")
C1 <- C %>% unite("merged", c(Location, Year), sep = "_", remove = F)
C1$merged <- as.factor(C1$merged)
C1 <- split(C1, C1$merged)
names(C1)
str(C1)

for (i in names(C1)) {
  write.csv(C1[[i]],  paste0(i, '.csv'), row.names = F, quote = F)
}



library(readxl)
C <- read_excel("~/Desktop/Smit/Biodigestibilty/C.xlsx")
str(C)
##changing the class of following columns to factor
C$Block=as.numeric(C$Block)
C$Row=as.numeric(C$Row)
CC= C %>%
  mutate(C_Row = if_else(
    Location == 'OR',
    if_else(Block == 1, Row, (Block - 1) * 2 + Row),
    if_else(Block == 1, Row, (Block - 1) * 4 + Row)
  ))
Columns=c("Sample","Block", "Column", "Row", "Position", "Treatment", "Location", "Year", "Env","C_Row")
CC[Columns]=lapply(CC[Columns], factor)


R_DM= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, DM, DM_201, DM_202) %>%
  arrange(Env,C_Row,Column) %>% 
  rename( Res=DM,
          Cov1=DM_201,
          Cov2=DM_202)

R_CP= CC %>% 
  select(N,Sample, Block,Column, Row, C_Row,Position, Treatment, Location, Year, Env, CP, CP_201, CP_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=CP,
          Cov1=CP_201,
          Cov2=CP_202)
R_Ash= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row,Position, Treatment, Location, Year, Env, Ash, Ash_201, Ash_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=Ash,
          Cov1=Ash_201,
          Cov2=Ash_202)

R_aNDForm= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, aNDForm, aNDForm_201, aNDForm_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=aNDForm,
          Cov1=aNDForm_201,
          Cov2=aNDForm_202)
R_ADICP= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row ,Position, Treatment, Location, Year, Env, ADICP, ADICP_201, ADICP_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=ADICP,
          Cov1=ADICP_201,
          Cov2=ADICP_202)
R_NDICP= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, NDICP, NDICP_201, NDICP_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=NDICP,
          Cov1=NDICP_201,
          Cov2=NDICP_202)
R_SP= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, SP, SP_201, SP_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=SP,
          Cov1=SP_201,
          Cov2=SP_202)
R_A2= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row ,Position, Treatment, Location, Year, Env, A2, A2_201, A2_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=A2,
          Cov1=A2_201,
          Cov2=A2_202)
R_B1= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row ,Position, Treatment, Location, Year, Env, B1, B1_201, B1_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=B1,
          Cov1=B1_201,
          Cov2=B1_202)
R_B2= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, B2, B2_201, B2_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=B2,
          Cov1=B2_201,
          Cov2=B2_202)
R_C= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, C, C_201, C_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=C,
          Cov1=C_201,
          Cov2=C_202)
R_A= CC %>% 
  select(N,Sample, Block,Column, Row, C_Row,Position, Treatment, Location, Year, Env, A, A_201, A_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=A,
          Cov1=A_201,
          Cov2=A_202)
R_B= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, B, B_201, B_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=B,
          Cov1=B_201,
          Cov2=B_202)
R_kd= CC %>% 
  select(N,Sample, Block,Column, Row,C_Row, Position, Treatment, Location, Year, Env, kd, kd_201, kd_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=kd,
          Cov1=kd_201,
          Cov2=kd_202)

R_ERD6= CC %>% 
  select(N,Sample, Block,Column, Row, C_Row,Position, Treatment, Location, Year, Env, ERD6, ERD6_201, ERD6_202) %>%
  arrange(Env,Block,Row, Column) %>% 
  rename( Res=ERD6,
          Cov1=ERD6_201,
          Cov2=ERD6_202)
nrow(R_ERD6)
str(C)
Pheno=list(R_DM=R_DM,
           R_CP=R_CP,
           R_Ash=R_Ash,
           R_aNDForm=R_aNDForm,
           R_ADICP=R_ADICP,
           R_NDICP=R_NDICP,
           R_SP=R_SP,
           R_A2=R_A2,
           R_B1=R_B1,
           R_B2=R_B2,
           R_C=R_C,
           R_A=R_A,
           R_B=R_B,
           R_kd=R_kd,
           R_ERD6=R_ERD6)
names(Pheno)

saveRDS(Pheno, file = "/home/hawkins/Desktop/Smit/Biodigestibilty/Pheno_Control.rds")

library(asreml)
library(asremlPlus)
library(Matrix)

head(R_DM)

data=(Pheno[1])
nrow(R_DM)
str(data)
names(data)

BLUE_DM=data.frame()
BLUE_CP=data.frame()
BLUE_Ash=data.frame()
BLUE_aNDForm=data.frame()
BLUE_ADICP=data.frame()
BLUE_NDICP=data.frame()
BLUE_SP=data.frame()
BLUE_A2=data.frame()
BLUE_B1=data.frame()
BLUE_B2=data.frame()
BLUE_C=data.frame()
BLUE_A=data.frame()
BLUE_B=data.frame()
BLUE_kd=data.frame()
BLUE_ERD6=data.frame()

##########################################################################################
m3 <- asreml::asreml(fixed = Res ~ 1 + Treatment + Cov1 + Cov2, 
                     random = ~ + Block+spl(C_Row), 
                     residual = ~C_Row:Column,
                     data = R_ERD6%>%filter(Env%in%levels(R_ERD6$Env)[1])%>%arrange(C_Row, Column),
                     na.action = list(x = "include", y = "include"))
blue=predict.asreml(m3, classify = 'Treatment', vcov = TRUE)$pval
blue$weight=(1/blue$std.error)^2
#Normality_test=shapiro.test()
colnames(blue)=c("Treatment", levels(R_DM$Env)[1], "std.error", "status", paste("Weight_",levels(R_DM$Env)[1]))
blue=select(blue,-std.error, -status)

BLUE_ERD6=blue
#BLUE_CP = left_join(BLUE_DM,blue, by="Treatment")


#####################################################################################################

for (i in 2:length(levels(R_kd$Env))) {
   
  m3 <- asreml::asreml(fixed = Res ~ 1 + Treatment + Cov1 + Cov2, 
                       random = ~ + Block+spl(C_Row), 
                       residual = ~C_Row:Column,
                       data = R_ERD6%>%filter(Env%in%levels(R_ERD6$Env)[i])%>%arrange(C_Row, Column),
                       na.action = list(x = "include", y = "include"))
  blue=predict.asreml(m3, classify = 'Treatment', vcov = TRUE)$pval
  blue$weight=(1/blue$std.error)^2
  #Normality_test=shapiro.test()
  colnames(blue)=c("Treatment", levels(R_DM$Env)[i], "std.error", "status", paste("Weight_",levels(R_DM$Env)[i]))
  blue=select(blue,-std.error, -status)
  
  #BLUE_DM=blue
  BLUE_ERD6 = left_join(BLUE_ERD6,blue, by="Treatment")
  
}
str(BLUE_ERD6)
getwd()
setwd("/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST")
library(xlsx)
write.xlsx2(BLUE_ERD6, "/home/hawkins/Desktop/Smit/Biodigestibilty/BLUEs_ST/BLUE_ERD6.xlsx" )



head(BLUE_ERD6)

###Stop here
#check normality of predicted data
result=shapiro.test(BLUE_DM$ID_2020)
Normality_test=result$p.value


#check normality of predicted data
shapiro.test(blue$predicted.value)

#genetic variance
names(m3)
m3$vparameters

m4 <- asreml::asreml(fixed = Res ~ 1 +  Cov1 + Cov2, 
                     random = ~ + Treatment + Block + spl(Row), 
                     residual = ~Block:Row:Column,
                     data = R_DM%>%filter(Env%in%c('ID_2020')), 
                     na.action = list(x = "include", y = "include"))
blup=predict.asreml(m4, classify = 'Treatment', vcov = TRUE)$pval
 
names(m4)
m4$vparameters

##H square (heterobility), contact Cesar

library(ggplot2)
BLUE_DM
