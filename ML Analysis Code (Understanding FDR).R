#Machine Learning Homework 1(Analysis Code)
#Section-2
#Members: Vardhini Manivannan, Shalini Mishra, Prakriti Rastogi, Shivani Verma
#------------------------------------------------------------------------------


#### Purchases of Ice Cream
ice = read.csv("ice_cream.csv")

## explore
names(ice)

## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity
y <- log(1 + priceper1)

#Additional:checking the distribution of priceper1
boxplot(priceper1)
hist(priceper1)
#There are a lot of outliers, to resolve this issue 
#log transform to resolve
hist(y)
boxplot(y)

##-------ORIGINAL MODEL-----------------
## collect some variables of interest
## create a data.frame
x <- ice[ ,c("flavor_descr", "size1_descr", "household_income", "household_size")]
colnames(x)
#Assumption-household income is an ordinal variable order based on numeric value
class(x$household_income)

View(ice)
## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
#Derived Variable-Coupon value/unit
x$couponper1 <- ice$coupon_value / ice$quantity
#x$usecoup
## organize some demographics
x$region <- factor(ice$region, levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(ice$marital_status==1)
x$race <- factor(ice$race, levels=1:4, labels=c("white", "black", "asian", "other"))
x$hispanic_origin <- ice$hispanic_origin==1
x$microwave <- ice$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- ice$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- ice$type_of_residence == 1
x$internet <- ice$household_internet_connection == 1
x$tvcable <- ice$tv_items > 1


class(x$household_income)
## combine x and y
## cbind is "column bind".  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 
## Summary and R-squared for glm
summary(fit)
library(rsq)
rsq(fit)
rsq(fit,adj=TRUE)
#AIC: -21721

#Doing the same using lm()
#Adding a column for total spent - assumption:total basket spent
xy_1 <- xy
xy_1 <- xy_1[,-c(16)]
xy_1$TOTAL <- ice$total_spent
model_sol_ex_1a <- lm(y ~ .,xy_1)
summary(model_sol_ex_1a)
colnames(xy_1)

#Visual check
#Checking relationship with independant variables
cor(xy_1$household_size,xy_1$household_income)
plot(priceper1,xy_1$couponper1)
plot(xy_1$household_income,priceper1)
plot(xy_1$household_size,priceper1)


#Categorical variables
#Running an ANOVA- just to understand the relation
boxplot(y~xy_1$household_income)
summary(aov(y~xy_1$household_income))

boxplot(y~xy_1$race)
summary(aov(y~xy_1$race))

boxplot(y~xy_1$region)
summary(aov(y~xy_1$region))



a1 <- lm(x$household_income~x$microwave + x$dishwasher + x$internet + x$tvcable)
summary(a1)
#Model trial 1 - removed microwave and dishwasher
#affluent households are likely to have appliances which
#is accounted for in HH income anyway
colnames(xy_1)
x_1 <-  xy_1[,-c(11,12)]
model_sol_2 <- lm(y ~.,x_1)
summary(model_sol_2)

#Model trial 2 - removed microwave,dishwasher, hispanic origin
#Hispanic_originTRUE was insignificant in the previous model
colnames(x_1)
x_2 <-  x_1[,-c(10)]
model_sol_3 <- lm(y ~.,x_2)
summary(model_sol_3)


#Model trial 3 - removed microwave,dishwasher, hispanic origin
#internet, cable were insignificant and we lacked enough correlation data to 
#derive any insights
colnames(x_2)
x_3 <-  x_2[,-c(11,12)]
model_sol_4 <- lm(y ~.,x_3)
summary(model_sol_4)

#Model trial 4 - removed microwave,dishwasher, hispanic origin, internet, cable, 
#changed race to numeric(dummy) such that for black and others it will be 1 for rest 0
colnames(x_3)
x_4 <- x_3
x_4$race <-  as.numeric(ifelse(x_3$race=='black' | x_3$race=='other',1,0))
class(x_4$race)
model_sol_5 <- lm(y ~.,x_4)
summary(model_sol_5)

#No change between the model with race as dummy and race as factors
#Looking at just internet, tvcable and dishwasher to comprehend the extent of their effects
model_sol_5 <- lm(y~x$internet + x$tvcable + x$dishwasher,x)
summary(model_sol_5)

#Model trial 5 - removed microwave,dishwasher, hispanic origin, internet, cable,added flavor type
#Checking if flavor type-regular/non fat makes any difference
colnames(x_4)
x_4$flavr_type <- ice$formula_descr
model_sol_6 <- lm(y ~.,x_4)
summary(model_sol_6)

#not significant -doesn't bring any value to the model
#Removing flavor type again
promo <- ifelse(is.na(ice$promotion_type),1,0)
colnames(x_4)
x_4 <- x_4[,c(-12)]
x_4 <- cbind(x_4,promo)
#Running an lm()
model_sol_6 <- lm(y ~.,x_4)
#running a glm()
glm(y ~.,data=x_4)

#IMPROVED MODEL
summary(model_sol_6) #the most sorted regression

#Comparing AIC of models
AIC(fit)          #given model, #AIC:-21720.65
AIC(model_sol_6)  #improved model, #AIC: -22070.2


#independent variables in the final model
colnames(x_4)

#running an outlier test
car::outlierTest(model_sol_6)

#---FDR Control----
## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(model_sol_6)$coef[-1,4] 
## source the fdr_cut function
#taking 5% our selected FDR=0.05
#total significant variables (5% significance level)
sum(ifelse(pvals<=0.05,1,0)) #41
source("fdr.R")
fdr(pvals,0.05) # 0.02342345
sum(ifelse(pvals<0.02342345,1,0))
#41 true discoveries, No False Discoveries at q<=0.05
