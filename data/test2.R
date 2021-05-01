csv2 <- read.csv("criminal_analysis2.csv")
df2 <- as.data.frame(csv2, row.names = NULL)

result3 <- lm(Cases~Gun, data = df2)