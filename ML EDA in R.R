#Machine Learning Homework 1 (EDA Code)
#Section-2
#Members: Vardhini Manivannan, Shalini Mishra, Prakriti Rastogi, Shivani Verma
#--------------------------------------------------------------------------------
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

#### Purchases of Ice Cream
ice = read.csv("ice_cream.csv")

## explore
names(ice)

str(ice)
##

##understanding effect on price
price_perhousehold_pertransaction = (ice$price_paid_deal + ice$price_paid_non_deal)


##diff-din-diff
## create a new variable for price per unit
priceper1 = (ice$price_paid_deal + ice$price_paid_non_deal) / ice$quantity

y <- log(1 + priceper1)

## collect some variables of interest
## create a data.frame
x <- ice[ ,c("household_id","promotion_type","qu","total_spent","flavor_descr", "size1_descr", "household_income", "household_size","age_and_presence_of_children")]
x$pricepertransaction = (ice$price_paid_deal + ice$price_paid_non_deal)
View(x)


#promotion as a treatment, 0-pre, 1-false
x$promotion_treatment<-sapply(x$promotion_type, function(x)
{ifelse(is.na(x),'without_promotion','with_promotion')})



## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr, "VAN")
## coupon usage
x$usecoup = factor(ice$coupon_value > 0)
x$couponper1 <- ice$coupon_value / ice$quantity
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
x$household_id<-as.factor(x$household_id)

x$priceper1<-priceper1
View(x)
str(x)



#eda
#household eda
View(x)
str(x)

#household revenue, frequency of purchase calculations
household<-x %>% group_by(household_id) %>% summarize(revenue=sum(pricepertransaction),freuqencyofpurchase=n(),per_purchase_revenue=revenue/freuqencyofpurchase)
x$age_and_presence_of_children<-as.factor(x$age_and_presence_of_children)

#price per unit
ggplot(data = x, aes(fill="pink",x = married, y = priceper1)) + 
  geom_boxplot()+ylab("Price per Unit")

ggplot(data = x, aes(x = race, y = priceper1)) + 
  geom_boxplot(fill="pink")+ylab("Price per Unit")

ggplot(data = x, aes(x = age_and_presence_of_children, y = priceper1)) + 
  geom_boxplot(fill="skyblue")+ylab("Price per Unit")

ggplot(data = x, aes(x = flavor_descr, y = priceper1)) + 
  geom_boxplot()+ylab("Price per Unit")

ggplot(data = x, aes(x = promotion_treatment, y = priceper1)) + 
  geom_boxplot(fill="lightgreen")+ylab("Price per Unit")

x$household_size<-as.factor(x$household_size)
ggplot(data = x, aes(x = household_size, y = priceper1)) + 
  geom_boxplot(fill="lightgreen")+ylab("Price per Unit")

View(x %>% select(promotion_treatment,priceper1))


household_ppu<-x %>% group_by(age_and_presence_of_children) %>% summarize(mean(priceper1))
household_ppu<-x %>% group_by(race) %>% summarize(sum(priceper1))

#household price per unit
x %>% group_by(household_id) %>% summarize()

#other household details
hh<-distinct(x%>% select(household_id, household_income, household_size,married, age_and_presence_of_children, race,internet, region) %>% group_by(household_id))

#consolidates household dataframe
household_details<-inner_join(hh,household, by="household_id")

#marital status and revenue, frequency of purchase
total_rev<-as.numeric(total_rev)
hh_eda<-as.data.frame(household_details %>% group_by(household_size) %>% summarise(no_of_households=n(), total_hh_rev=sum(revenue), per_total_rev=total_hh_rev/total_rev ,rev_per_hh=total_hh_rev/n()) %>% arrange(desc(per_total_rev),desc(rev_per_hh)))
write.csv(hh_eda,file="hh_eda.csv")
household_details %>%  group_by(married) %>% summarise(n())
3829/(3829+2556)
hh_married<-household_details %>% filter(household_size==2) %>% group_by(married,age_and_presence_of_children) %>% summarise(n())
write.csv(hh_married,file="hh_married.csv")
1709/(1709+666)
ggplot(data = household_details, aes(x = married, y = revenue)) + 
  geom_boxplot()
ggplot(data = household_details, aes(x = married, y = freuqencyofpurchase)) + 
  geom_boxplot()
ggplot(data = household_details, aes(x = married, y = per_purchase_revenue)) + 
  geom_boxplot()

str(household_details)
household_details %>% filter(household_size==2) %>% group_by(married) %>% summarize(sum(revenue))
27063/total_rev

household_details %>% filter(household_size==2, married==TRUE) %>% group_by(age_and_presence_of_children)%>%summarise(number=n())

#marital status
ggplot(data = household_details, aes(x = household_size, y = revenue)) +geom_bar(stat="identity", color = "blue")

#revenue is highest from 2 and 1 household size
household_details %>%  group_by(married) %>% summarise(n())
child_ed<-household_details %>%  group_by(age_and_presence_of_children) %>% summarise(no_of_households=n(),percentage_revenue=(sum(revenue)/as.numeric(total_rev))*100)
write.csv(child_ed, file="child_eda.csv")
#race
race_eda<-household_details %>%group_by(race) %>% summarise(no_of_household=n_distinct(household_id), totalrevenue=sum(revenue),revenue_per_household=sum(revenue)/n_distinct(household_id))
write.csv(race_eda, file="race_eda.csv")


ice%>%  group_by(flavor_descr) %>% summarise(total_rev=sum(price_paid_deal+price_paid_non_deal),households=n_distinct(household_id), transactions=n()) %>% arrange(desc(total_rev))

#age and presence of children
ice%>% filter(marital_status==1,age_and_presence_of_children==9) %>%  group_by(flavor_descr) %>% summarise(total_rev=sum(price_paid_deal+price_paid_non_deal),households=n_distinct(household_id), transactions=n()) %>% arrange(desc(total_rev))
ice%>% filter(age_and_presence_of_children==9) %>%  group_by(flavor_descr) %>% summarise(total_rev=sum(price_paid_deal+price_paid_non_deal),households=n_distinct(household_id), transactions=n()) %>% arrange(desc(total_rev))
ice%>% filter(age_and_presence_of_children==9) %>% group_by(marital_status==1) %>%
  #region
  ggplot(data = household_details, aes(x = region, y = revenue)) + 
  geom_boxplot()

rg<-household_details %>% group_by(region) %>% summarize(number_of_households=n(),region_revenue=sum(revenue),region_fop=sum(freuqencyofpurchase), rev_per_household=region_revenue/number_of_households)
write.csv(rg, file="region.csv")
household_details %>% filter(household_size==2) %>% group_by(region) %>% summarize(region_revenue=sum(revenue),region_fop=sum(freuqencyofpurchase))
total_rev<-rg %>% select(region,region_revenue,region_fop) %>% summarise( totalrevenue=sum(region_revenue))
region_summary<-rg %>% select(region,region_revenue,region_fop)  %>% group_by(region) %>% summarise(region_revenue,region_fop, per_totalrevenue=(region_revenue/as.numeric(total_rev))*100)
region_sum<-gather(region_summary,"data","value"  ,2:4)
ggplot(data = region_sum, aes(fill=data,x = region, y = value)) +geom_bar(position="dodge", stat="identity")
ggplot(data = region_summary, aes(x = region, y = per_totalrevenue)) +geom_bar(position="dodge", stat="identity")

#flavors in each region
x %>%  group_by(region,flavor_descr) %>% summarise(total_rev=sum(price_paid_deal+price_paid_non_deal),households=n_distinct(household_id), transactions=n()) %>% arrange(desc(households))


# household revenue
hist(household$revenue,xlim=c(min(household$revenue), max(household$revenue)),axis(side=1, at=seq(0,500, 20), labels=seq(0,500,20)))


#total basket spend_factors
No_of_households<-x%>% summarise(n_distinct(household_id))
spend_demographics<-x %>% select(household_id,total_spent,household_size,household_income,married,age_and_presence_of_children,race,region) %>% group_by(household_id)
pairs(spend_demographics)
model_spend<-lm(x$total_spent~x$age_and_presence_of_children+x$household_size+x$household_income+x$married)
summary(model_spend)

boxplot(total_spent~age_and_presence_of_children, data=spend_demographics)
boxplot(total_spent~household_income, data=spend_demographics)
boxplot(total_spent~married, data=spend_demographics)
boxplot(total_spent~race, data=spend_demographics)
boxplot(total_spent~region, data=spend_demographics)
boxplot(total_spent~region, data=spend_demographics)


spend_demographics<-x %>% select(household_id,total_spent,promotion_type,flavor_descr,size1_descr,) %>% group_by(household_id)
pairs(spend_demographics)
