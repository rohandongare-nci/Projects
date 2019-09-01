#brief analysis of the underlying data
library(scales)
library(plyr)
library(tidyverse)
library(data.table)
library(gridExtra)
library(arules)
library(ggplot2)
customer_data<-read.csv("C:/Users/rohan/OneDrive/Desktop/1.ACRM-Proposal/train.csv")
names(customer_data)<-c("u_id","p_id","gender","age","occupation","city_number","stay_in_current_city_years","marital_status"," product_category_1"," product_category_2","product_category_3","purchase")
levels(customer_data$city_number)
customer_data$city_number<-revalue(customer_data$city_number, c("A"="city_1", "B"="city_2", "C"="city_3"))

#general descriptive statistics
summary(customer_data)
levels(customer_data$age)
levels(customer_data$stay_in_current_city_years)

#checking for missing values
sapply(customer_data, function(x) sum(is.na(x)))
customer_data[is.na(customer_data)]<-0

#checking unique values
lapply(customer_data, function(x)length(unique(x)))

#which gender shopped the most
cust_gender = customer_data %>% select(gender,u_id) %>% 
group_by(u_id) %>% distinct()
summary(user_gender)
gender<-cust_gender$gender
gender_plot  = ggplot(data = cust_gender) + g
eom_bar(mapping = aes(x = gender, y = ..count.., fill = gender)) 
+labs(title = 'Customer Gender') 
plot(gender_plot)

#Finding Top Customers and their gender
customer_purchases = customer_data %>%select(purchase,u_id,gender) %>% group_by(u_id) %>% arrange(u_id) %>% summarise(Purchase_value_total = sum(purchase))
head(customer_purchases)
#arranging customers in descending order of purchases
top_customers <- customer_purchases[order(-customer_purchases$Purchase_value_total),]
head(top_customers)
gender2 = customer_data %>% select(gender,u_id) %>% group_by(u_id) %>%arrange(u_id) %>% distinct()
purchase_and_gender<- full_join(top_customers,gender2,by="u_id")
#top 200 customers with gender
top_customer_with_gender<-head(purchase_and_gender,200)
summary(top_customer_with_gender)
head(top_customer_with_gender)
top_200_plot  = ggplot(data = top_customer_with_gender) + geom_bar(mapping = aes(x = gender, y =..count.., fill = gender)) +labs(title = 'Top 200 customers') 
plot(top_200_plot)

#AVerage spending of customers by gender
avg_spending = purchase_and_gender %>% group_by(gender) %>% 
summarize(C = n(),
purchases = sum(Purchase_value_total), Avg = purchases/C)
print(avg_spending)
avg_spend  = ggplot(data = avg_spending) + 
geom_bar(mapping = aes(x = gender, y =Avg, fill = gender),
stat = 'identity') 
+labs(title = 'Average Spending By Gender')
plot(avg_spend)


#Segregation of customers according to age/which age bracket spends the most
cust_age = customer_data %>% select(age,u_id,purchase) %>% 
distinct() %>% count(age) 
age_purchases = customer_data %>%select(purchase,u_id,age) %>% 
group_by(age) %>% arrange(age) %>% 
summarise(PurchaseS_value_total = sum(purchase))
head(age_purchases)
age_purchase<-  ggplot(data = age_purchases) + 
geom_bar(mapping =aes(x = age, y =PurchaseS_value_total, fill = age), 
stat = 'identity')
+labs(title = 'Average spending by age bracket')
plot(age_purchase)

a<-age_purchases$PurchaseS_value_total[3]
b<-age_purchases$PurchaseS_value_total[4]/sum(age_purchases$PurchaseS_value_total)

#/sum(age_purchases$PurchaseS_value_total)

#which products were sold the most and who bought them
most_sold_products = customer_data %>% count(p_id, sort = TRUE)
TopProducts = head(most_sold_products, 10)
TopProducts
Top_selling_products = customer_data[customer_data$p_id == 'P00025442'| customer_data$p_id =='P00265242'| customer_data$p_id =='P00110742', ]
head(Top_selling_products)
#gender/age
top_product_age<-Top_selling_products %>%select(purchase,u_id,gender,age) %>% group_by(u_id) %>% arrange(u_id) %>% summarise(Purchase_value_total2 = sum(purchase))
age2 = Top_selling_products %>% select(age,u_id) %>% group_by(u_id) %>%arrange(u_id) %>% distinct()
top_age_product<-full_join(top_product_age,age2,by="u_id")
head(top_age_product)
top_products_by_age<-  ggplot(data = top_age_product) + geom_bar(mapping = aes(x = age, y =Purchase_value_total2, fill = age), stat = 'identity') +labs(title = 'Age Distribution Of Customers Who Bought the Most Popular Products By Age')
plot(top_products_by_age)
#now gender
top_product_gender<-Top_selling_products %>%select(purchase,u_id,gender,age) %>% group_by(u_id) %>% arrange(u_id) %>% summarise(Purchase_value_total3 = sum(purchase))
gender3 = Top_selling_products %>% select(gender,u_id) %>% group_by(u_id) %>%arrange(u_id) %>% distinct()
top_gender<-full_join(top_product_gender,gender3,by="u_id")
top_gender_product<-ggplot(data = top_gender) + geom_bar(mapping = aes(x = gender, y =Purchase_value_total3, fill = gender), stat = 'identity') +labs(title = 'Customers Who Bought the Most Popular Products By Gender')
plot(top_gender_product)

#which city do most customers come from ?
cust_loc =  customer_data %>% select(u_id, city_number,purchase)%>% distinct()
head(cust_loc)
loc_plot = ggplot(data = cust_loc) + geom_bar(color = "black", 
mapping = aes(x = city_number, y = ..count.., fill = city_number)) +
labs(title = 'Which City Do Most Customers Come From?')
plot(loc_plot)
summary(cust_loc)

#which city spends the most?
library(dplyr)
city_1<-filter(customer_data,city_number=="A")
city1_sum<-sum(city_1$purchase)
city_2<-filter(customer_data,city_number=="B")
city2_sum<-sum(city_2$purchase)
city_3<-filter(customer_data,city_number=="C")
city3_sum<-sum(city_3$purchase)
#creating a new dataframe
City_Purchase<-c(city1_sum,city2_sum,city3_sum)
City_Number<-c("City_1","City_2","City_3")
city_purchase_final<-data.frame(City_Number,City_Purchase)
city_purchase_final
  city_purchase_plot = ggplot(data = city_purchase_final) + 
geom_bar(color = "black", mapping = aes(x = City_Number,
y = City_Purchase, fill = City_Number), stat = 'identity') +
labs(title = 'Which City Spends The Most?')
plot(city_purchase_plot)

#marital status and occupation


m_status = customer_data%>%select(u_id, marital_status) %>%
group_by(u_id) %>%
distinct()
ms_plot<- ggplot(data = m_status)+geom_bar(color="black",
mapping=aes(x=marital_status,y=..count..,fill=marital_status))
+labs(title = "Purchases By Marital Status")
plot(ms_plot)


m_status$marital_status<-as.character(marital_status$marital_status)

occupation =  customer_data %>%
  select(u_id, occupation) %>%
  group_by(u_id) %>%
  distinct()
occupation_purchase<-full_join(occupation,customer_purchases,by="u_id")
summary(occupation_purchase)
occupation_purchase_summary<-occupation_purchase %>% group_by(occupation) %>%
dplyr::summarise(Purchase_Amount = sum(Purchase_value_total))
occupation_plot<-ggplot(data = occupation_purchase_summary)+geom_bar(color="black",stat="identity",
mapping = aes(x=occupation,y=Purchase_Amount,fill=occupation))+
scale_x_discrete(name="Occupation", breaks = seq(0,20, by = 1),labels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
plot(occupation_plot,las=2)

chi2 = chisq.test(customer_data, correct=F)
c(chi2$statistic, chi2$p.value)
cor(rank(customer_data$purchase), rank(customer_data$age))
#Apriori
library(arules)

library(tidyverse)
product_sparse<-customer_data %>% 
select(u_id,p_id)%>% group_by(u_id) %>% arrange(u_id)
sparse_matrix_products = product_sparse %>% 
dplyr::mutate(id = row_number()) %>% spread(u_id, p_id) %>% t()                               
head(sparse_matrix_products)
sparse_matrix_products = sparse_matrix_products[-1,]
write.csv(customers_products, file = 'C:/Users/rohan/OneDrive/Desktop/1.ACRM-Proposal/sparse_matrix_products.csv')
customer_product_data = read.transactions('C:/Users/rohan/OneDrive/Desktop/1.ACRM-Proposal/customers_products.csv', sep = ',', rm.duplicates = TRUE)
summary(customer_product_data)

itemFrequencyPlot(customer_product_data,topN=15, type="absolute")

rules_customer_product = apriori(data = customer_product_data, parameter = 
list(support = 0.00678, confidence = 0.88, maxtime = 0))
inspect(sort(rules_customer_product, by = 'lift'))
library(arulesViz)
plot(rules_customer_product, method = 'graph')

rules_customer_product_2 = apriori(data = customer_product_data, 
parameter = list(support = 0.00678, confidence = 0.85, maxtime = 0))
inspect(sort(rules_customer_product_2, by = 'lift'))

40/5892 
