library(asreml)
library(dplyr)

Yi_H2 <- list()
lev1 <- c("block", "gen", "row", "col", "check", "Env")
env <- c("ID_2020", "OR_2020", "WA_2020", "OR_2021", "WA_2021")

for (i in 1:length(Pheno_Control)) {
  data <- as.data.frame(Pheno_Control[[i]])
  data <- data[, c(3, 8, 6, 4, 11, 12, 13, 14)]
  colnames(data) <- c("block", "gen", "row", "col", "Env", "resp", "cov1", "cov2")
  data <- data[order(data$row, data$col), ]
  data$check <- dplyr::recode_factor(data$gen, "201" = "control", "202" = "control", .default = "test")
  data[, lev1] <- lapply(data[, lev1], factor)
  
  trait_name <- names(Pheno_Control)[i]
  
  trait_H2_list <- list()  # Create a list to store H2 results for each trait
  
  for (n in 1:length(env)) {
    data1 <- data %>% filter(Env == env[n])
    m3 <- asreml::asreml(fixed = resp ~ 1 + at(check, "control"):gen, 
                         random = ~ block + at(check, "test"):gen, 
                         residual = ~ ar1(row):id(col),
                         data = data1, 
                         na.action = list(x = "include", y = "include"))
    
    m3 <- update.asreml(m3)
    vg <- vpredict(m3, VG ~ V2)
    h2 <- vpredict(m3, h2 ~ V2 / (V1 + V2 + V3 + V4))
    H2 <- rbind(vg, h2)
    H2 <- H2 %>% rownames_to_column("component")
    H2 <- H2 %>% mutate(Env = env[n])
    
    trait_H2_list[[env[n]]] <- H2
  }
  
  # Store H2 results for the current trait in the main list
  Yi_H2[[trait_name]] <- trait_H2_list
}

saveRDS(Yi_H2, file = "/home/hawkins/Desktop/Smit/Biodigestibilty/H2_nested.rds")
########################################################################
library(tidyverse)

# Create an empty list to store data frames
H2 <- list()

# Loop through each trait in Yi_H2
for (trait_name in names(Yi_H2)) {
  trait_list <- Yi_H2[[trait_name]]
  
  # Create a data frame for the current trait
  trait_df <- do.call(rbind, trait_list)
  
  
  # Combine the modified data frames back into the trait list
  H2[[trait_name]] <- trait_df
  
}
H2
#############################################################

H2_final <- lapply(H2, function(df) {
  df %>%
    rownames_to_column() %>%  # Convert rownames to a column
    select(-rowname, everything())%>%
    select(component, Estimate, SE, Env)# Remove the rownames column
})
H2_final[6]
#saveRDS(H2_final, file = "/home/hawkins/Desktop/Smit/Biodigestibilty/H2.rds")

