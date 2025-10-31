library(dplyr)
#Create the dataset
df <- data.frame(
  Subject = c('Claudius', 'Julius', 'Augustus', 'Octavian','Zeus','Hera','Aphrodite','Artemis'),
  Smoking_Status = rep(c('Former', 'Current', 'Former', 'Current'),2),
  A = factor(rep(c('Drug 1', 'Drug 1', 'Drug 2', 'Drug 2'),2)),
  R_a1 = c(80, 76, 85, 70, 81, 74, 87 , 71),   # Potential outcome under Drug 1
  R_a2 = c(95, 60, 100, 55, 96, 61, 101, 54),  # Potential outcome under Drug 2
  stringsAsFactors = FALSE
)

# Add observed outcome (what we actually see in the study)
df$R_observed <- ifelse(df$A == 'Drug 1', df$R_a1, df$R_a2)

df

ate_df = df %>% group_by(A) %>% summarise(ATE=mean(R_observed))

cate_df = df %>% group_by(A,Smoking_Status) %>% summarise(CATE = mean(R_observed))



cate_df2=cate_df %>% tidyr::pivot_wider(names_from = Smoking_Status, values_from = CATE) %>% as.data.frame()

sum(as.numeric(cate_df2[2,]) - as.numeric(cate_df2[1,]))


