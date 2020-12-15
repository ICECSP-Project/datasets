library(dplyr)
data = read.csv("../Datacleaning/cleaned_beijing.csv")

set.seed(193)
inSubset = sample(1:nrow(data),size = 10000, replace = F)
s_data = data[inSubset,]

write.csv(s_data, "s_data.csv")
write.csv(s_data, "../../../house-price-prediction-app/shinyApp/data/s_data.csv")
saveRDS(s_data, file = "s_data.rds")
saveRDS(s_data, file = "../../../house-price-prediction-app/shinyApp/data/s_data.rds")
