library(tidyverse)

setwd("~/Documents/git/Biodigestibility_Alfalfa/R_scripts/")

names1 <- c("A","A2","ADICP","aNDForm","Ash","B","B1","B2","C","CP","ER6","kd","NDICP","SP")
# names(list1) <- names1
# A A2 ADICP aNDForm Ash B B1 B2 C CP ER6 kd NDICP SP

data3 <- data.frame()
list1 <- list()
for (i in 1:length(names1)) {
  path <- paste0("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST2/",names1[i])
  setwd(path)
  data <- read.csv("predictions_2stage_ASReml_mrbean.csv")
  data$trial <- paste0(data$trial, "_",names1[i])
  data <- data %>% dplyr::select(1:3) %>% spread(key = trial, value = predicted.value)
  data1 <- read.csv("Overall_predictions_2stage.csv")
  data1 <- data1 %>% dplyr::select(1:2)
  colnames(data1)[2] <- paste0("all_",names1[i])
  data2 <- inner_join(data,data1, by = "gen")
  # data2 <- data2 %>% column_to_rownames("gen")
  # data3 <- rbind(data3, data2)
  # data3 <- inner_join(data2 ,data3, by = "gen")
  list1[[length(list1)+1]] = data2
  
  rm(data, data1, data2)
}

names(list1) <- names1

PCA1 <- read.csv("~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/PCA.csv")

data3 <- list1[[1]] %>% 
  inner_join(., list1[[2]], by = "gen") %>% 
  inner_join(., list1[[3]], by = "gen") %>% 
  inner_join(., list1[[4]], by = "gen") %>% 
  inner_join(., list1[[5]], by = "gen") %>% 
  inner_join(., list1[[6]], by = "gen") %>% 
  inner_join(., list1[[7]], by = "gen") %>% 
  inner_join(., list1[[8]], by = "gen") %>% 
  inner_join(., list1[[9]], by = "gen") %>% 
  inner_join(., list1[[10]], by = "gen") %>% 
  inner_join(., list1[[11]], by = "gen") %>%
  inner_join(., list1[[12]], by = "gen") %>%
  inner_join(., list1[[13]], by = "gen") %>%
  inner_join(., list1[[14]], by = "gen") %>%
  inner_join(., PCA1, by = "gen")

# data 3 contains:
# 192 cols
# 1 gen
# 3 PCA
# 14 traits with 6 ST2_BLUPs (14 * 6 = 84)

write.csv(data3, "~/Documents/git/Biodigestibility_Alfalfa/BLUEs_ST/pheno_2.csv", quote = F, row.names = F)
