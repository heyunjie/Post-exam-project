library(corrplot)
library(glmnet)
library(reshape)
df=read.csv('~/Desktop/R data/ReferendumResults.csv')

# Feature engineering

# Check for na
sum(is.na(df))
#No na

# Three age groups
# Minor, Working people and the elderly
# Minor cannot vote, is dropped to prevent perfect multicollinearity
# Working= proportion for geographic dividend
# Fewer working people, more children and old people without steady source of income
# more stressful working people 

df$Working=df$Age_18to19+df$Age_20to24+df$Age_25to29+df$Age_30to44+df$Age_45to59+df$Age_60to64

df$Old=df$Age_65to74+df$Age_75to84+df$Age_85to89+df$Age_90plus

#We may get rid of the ages, and ID
df=df %>%
  
  select(-c(1,11:26)) 



# Split the train and test set
# Change -1 in Leave column to na
# Will be using cross val, so not dividing the validation set
train=subset(df,df$Leave!=-1)
test=subset(df,df$Leave==-1)


# Asian seems to include India actually
df %>%
  mutate(Sum = select(., White:Asian) %>% rowSums(na.rm = TRUE))

race=df %>%
  mutate(Sum = select(., White:Asian) %>% rowSums(na.rm = TRUE))

#We are actually safe to consider including all races variables as they don't sum to a constant








# Correlation
# Not very helpful

  
  select_if(is.numeric)
corr <-  df%>%
  select_if(is.numeric)%>%
  cor()%>%
  
  corrplot(method='shade',tl.cex=0.6,number.cex = 0.8)

# lasso regression(L1) for feature selection
train_numeric=train %>%
  select_if(is.numeric)

output=glmnet(as.matrix(train_numeric[,-4]), train_numeric[,4] , standardize=TRUE, alpha=1)
beta=coef(output)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)

tmp <- melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- output$lambda[tmp$variable+1] 
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1]

ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(2,"lines"))












