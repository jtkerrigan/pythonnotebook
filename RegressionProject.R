install.packages("ggplot2")
library(ggplot2)
ldata<-read.csv("shopping_trends.csv")

set.seed(202457648)

n<-nrow(data)
data$binomial<-rbinom(n, 1, 0.7)
project.data<-subset(data, data$binomial==1)

write.csv(project.data, "shoppingdata_202457648.csv", row.names=FALSE)
project.data<-read.csv("shoppingdata_202457648.csv")

##Question 1 - Exploratory Analysis

#Purchase Amount exploration

summary(project.data$Purchase.Amount)

ggplot(project.data, aes(y=Purchase.Amount))+
  geom_boxplot()+
  labs(title="Purchase Amount Boxplot",
       y="Purchase Amount (£)")

sum(project.data$Purchase.Amount>150)
#Gender boxplot

ggplot(project.data, aes(x=Gender, y=Purchase.Amount))+
  geom_boxplot(outlier.shape=NA)+
  labs(title="Comparison of Most Recent Purchases by Gender",
       x="Gender",
       y="Purchase Amount")+
  ylim(0,160)

#Type boxplot

ggplot(project.data, aes(x=Type, y=Purchase.Amount))+
  geom_boxplot(outlier.shape=NA)+
  labs(title="Comparison of Most Recent Purchase Amount by Item Type",
       x="Item Type",
       y="Purchase Amount")+
  ylim(0,160)

#Payment Method boxplot

ggplot(project.data, aes(x=Payment.Method, y=Purchase.Amount))+
  geom_boxplot(outlier.shape=NA)+
  labs(title="Comparison of Most Recent Purchase Amount by Payment Method",
       x="Payment Method",
       y="Purchase Amount (£)")+
  ylim(0,160)+
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

#Shipping Type Boxplot

ggplot(project.data, aes(x=Shipping.Type, y=Purchase.Amount))+
  geom_boxplot(outlier.shape=NA)+
  labs(title="Comparison of Most Recent Purchase Amount by Shipping Type",
       x="Shipping Type",
       y="Purchase Amount (£)")+
  ylim(0,160)

#Purchase Frequency Boxplot

ggplot(project.data, aes(x=Frequency.of.Purchases, y=Purchase.Amount))+
  geom_boxplot(outlier.shape=NA)+
  labs(title="Comparison of Most Recent Purchase Amount by Purchase Frequency",
       x="Purchase Frequency",
       y="Purchase Amount (£)")+
  ylim(0,160)

#Age scatter

ggplot(project.data, aes(x=Age, y=Purchase.Amount))+
  geom_point()+
  labs(title="Scatter Plot of Purchase Amount vs Age",
       x="Age (Years)",
       y="Purchase Amount (£)")+
  ylim(0,160)

##Items purchased scatter

ggplot(project.data, aes(x=Items.Purchased, y=Purchase.Amount))+
  geom_point()+
  labs(title="Scatter Plot of Items Purchased vs Purchase Amount",
       x="Items Purchased",
       y="Purchase Amount (£)")+
  ylim(0,160)

##Review Rating scatter

ggplot(project.data, aes(x=Review.Rating, y=Purchase.Amount))+
  geom_point()+
  labs(title="Scatter Plot of Review Rating vs Purchase Amount",
       x="Review Rating (Stars)",
       y="Purchase Amount (£)")+
  ylim(0,160)

##Previous Purchases Scatter

ggplot(project.data, aes(x=Previous.Purchases, y=Purchase.Amount))+
  geom_point()+
  labs(title="Scatter Plot of Previous Purchases vs Purchase Amount",
       x="Previous Purchases",
       y="Purchase Amount (£)")+
  ylim(0,160)

##Pairs

pairs(project.data[,c(2,4,6,7,10)])

##Question 2a)
project.data<-read.csv("shoppingdata_202457648.csv")
project.data$Frequency.of.Purchases<-factor(project.data$Frequency.of.Purchases,
                                            levels = c("First Purchase", "Annually", "Every 3 Months", "Monthly", "Fortnightly", "Weekly"))

shopping.model.full<-lm(Purchase.Amount ~ Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)
shopping.model.full

##Question 2b)
##Step 1
drop1(shopping.model.full,
      test="F",
      scope = ~ Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases)

##Remove Type
shopping.model.backwards1<-lm(Purchase.Amount ~ Age + Gender + Items.Purchased + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)
drop1(shopping.model.backwards1,
      test="F",
      scope = ~ Age + Gender + Items.Purchased + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases)

##Remove Gender
shopping.model.backwards2<-lm(Purchase.Amount ~ Age + Items.Purchased + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)
drop1(shopping.model.backwards2,
      test="F",
      scope = ~ Age + Items.Purchased + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases)

##Remove Review Rating
shopping.model.backwards3<-lm(Purchase.Amount ~ Age + Items.Purchased + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)
drop1(shopping.model.backwards3,
      test="F",
      scope = ~ Age + Items.Purchased + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases)

##Remove Shipping Type
shopping.model.backwards4<-lm(Purchase.Amount ~ Age + Items.Purchased + Payment.Method + Previous.Purchases + Frequency.of.Purchases, data=project.data)
drop1(shopping.model.backwards4,
      test="F",
      scope = ~ Age + Items.Purchased + Payment.Method + Previous.Purchases + Frequency.of.Purchases)
##Remove Frequency of Purchases
shopping.model.backwards5<-lm(Purchase.Amount ~ Age + Items.Purchased + Payment.Method + Previous.Purchases, data=project.data)
drop1(shopping.model.backwards5,
      test="F",
      scope = ~ Age + Items.Purchased + Payment.Method + Previous.Purchases)
##Remove Previous Purchases
shopping.model.backwards6<-lm(Purchase.Amount ~ Age + Items.Purchased + Payment.Method, data=project.data)
drop1(shopping.model.backwards6,
      test="F",
      scope = ~ Age + Items.Purchased + Payment.Method)
##Finished
shopping.model.backwards6

##c)
plot(shopping.model.backwards,pch=16)

##d)
library(MASS)
par(mfrow=c(1,1))
boxcox(shopping.model.backwards6)

shopping.model.backwards6.trans<-lm((Purchase.Amount)^(1/3) ~ Age + Items.Purchased + Payment.Method, data=project.data)
plot(shopping.model.backwards6.trans, pch=16)

##Forward Selection Test

shopping.model.int<-lm(Purchase.Amount~1, data=project.data)
add1(shopping.model.int,
     test="F",
     scope=~Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)

##Add Items Purchased

shopping.model.forwards1<-lm(Purchase.Amount~Items.Purchased, data=project.data)
add1(shopping.model.forwards1,
     test="F",
     scope=~Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)

##Add Payment Method
shopping.model.forwards2<-lm(Purchase.Amount ~ Items.Purchased + Payment.Method, data=project.data)
add1(shopping.model.forwards2,
     test="F",
     scope=~Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)

##Add Age
shopping.model.forwards3<-lm(Purchase.Amount ~ Items.Purchased + Payment.Method + Age, data=project.data)
add1(shopping.model.forwards3,
     test="F",
     scope=~Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)

##Add Previous.Purchases
shopping.model.forwards4<-lm(Purchase.Amount~Items.Purchased+Payment.Method+Age+Previous.Purchases, data=project.data)
add1(shopping.model.forwards4,
     test="F",
     scope=~Age + Gender + Items.Purchased + Type + Review.Rating + Payment.Method + Shipping.Type + Previous.Purchases + Frequency.of.Purchases, data=project.data)

##Finished

shopping.model.forwards4

plot(shopping.model.forwards4)

#d)
library(MASS)
boxcox(shopping.model.forwards4)
shopping.model.forwards4.trans<-lm((Purchase.Amount)^(1/4)~Items.Purchased+Payment.Method+Age+Previous.Purchases, data=project.data)
par(mfrow=c(1,1))
plot(shopping.model.forwards4.trans)
shopping.model.forwards4.trans

#e)

summary(shopping.model.full)
AIC(shopping.model.full)
require(olsrr)
ols_mallows_cp(shopping.model.full, shopping.model.full)

residuals.full<-residuals(shopping.model.full)
hat_values.full<-hatvalues(shopping.model.full)
press.full<-sum((residuals.full/(1-hat_values.full))^2)
press.full

summary(shopping.model.forwards4.trans)
AIC(shopping.model.forwards4.trans)
ols_mallows_cp(shopping.model.forwards4.trans, shopping.model.full)
residuals.small<-residuals(shopping.model.forwards4.trans)
hat_values.small<-hatvalues(shopping.model.forwards4.trans)
press.small<-sum((residuals.small/(1-hat_values.small))^2)
press.small

##f)

require(car)
vif(shopping.model.forwards4.trans)
install.packages("GGally")
require(GGally)
ggcorr(project.data[2:11], label_round=4, label=TRUE)

install.packages("dplyr")
library(dplyr)

average_spend_age1<-project.data%>%
  mutate(decade=cut(Age,
                    breaks = seq(10,100,by=10),
                    labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"),
                    right = FALSE
  ))

average_spend_age<-average_spend_age %>%
  group_by(decade)%>%
  summarise(Purchase.Amount = mean(Purchase.Amount, na.rm=TRUE))

ggplot(average_spend_age, aes(x=decade, y=Purchase.Amount, fill=decade))+
  geom_bar(stat="identity")+
  labs(title="Average Spend by Age Group",
       x = "Age Group",
       y = "Average Spend")

anova_age <- aov(Purchase.Amount ~ decade, data = average_spend_age1)
summary(anova_age)

age_group_percentages <- average_spend_age1%>%
  count(decade) %>%
  mutate(percentage=(n/sum(n))*100)

##Payment Methods

purchase_counts <- project.data %>%
  count(Payment.Method)

purchase_counts

pie(purchase_counts$n,
    labels=paste(purchase_counts$Payment.Method, ":", purchase_counts$n),
    main= "Distribution of Purchases by Method",
    col=rainbow(length(purchase_counts$Payment.Method)))

boxplot_values<-by(project.data$Purchase.Amount, project.data$Payment.Method, boxplot.stats)
boxplot_values

payment_info<-project.data%>%
  mutate(Payment.Group=ifelse(Payment.Method=="Klarna", "Klarna", "Other"))

ggplot(payment_info, aes(x = Payment.Group, y = Purchase.Amount)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = paste("Comparison of", "Klarna", "vs. Other Payment Methods"),
       x = "Payment Method",
       y = "Purchase Amount (£)") +
  theme_minimal() +
  ylim(0,150)+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
