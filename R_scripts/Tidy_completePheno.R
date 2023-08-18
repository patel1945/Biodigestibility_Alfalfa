library(xlsx)
library(readxl)
# Specify the file path and name of the Excel file
getwd()
file_path <- "Biodigestibilty/Pred_Pheno.xlsx"
#Extracting the sheetnames from a workbook
sheetnames=excel_sheets(file_path)
#reading sheets as list
mylist<-lapply(sheetnames, read_excel, path=file_path)
names(mylist)=sheetnames
str(mylist)
# Eactracting dataframes from list to global environment
list2env(mylist, .GlobalEnv)

#Combining all tthe data frames by N and sample number
library(dplyr)

# Initialize the joined_df variable with the first data frame
joined_df <- mylist[[1]]

# Loop through the remaining data frames and perform joins
for (i in 2:length(mylist)) {
  joined_df <- inner_join(joined_df, mylist[[i]], by = c("Sample"))
}

# View the final joined data frame
print(joined_df)

#Combine treatment and blocks
colnames(design)[2]="Sample"
design
Complete_Pheno=left_join(joined_df, design, by=c("N", "Sample"))


library(ggplot2)
ggplot(Complete_Pheno, aes(Treatment))+geom_histogram()

library(xlsx)
#write.xlsx2(Complete_Pheno, "Desktop/Smit/Biodigestibilty/Complete_Pred_Pheno.xlsx")


