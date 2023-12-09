

setwd("~/Documents/git/Biodigestibility_Alfalfa/")

dif1 <- read.csv("diff_gen.csv")
diff2 <- dif1[, 1, drop = F]
diff3 <- na.omit(dif1[, 2, drop = F])
colnames(diff2) <- "gen"
anti_join(diff2, diff3, by = "gen")

#   gen
# 1  34
# 2  56
# 3  91
# 4  98
# 5 102
# 6 110
# 7 111
# 8 114