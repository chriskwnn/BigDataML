library(data.table)
library(dplyr)
orders.df <- read.csv("/Users/chriskwan/Downloads/Pricing Data/order_data.csv")
head(orders.df)
#Top 5 ordered SKUs lets look at top one first
sort(table(orders.df$sku_ID), decreasing=TRUE)[1:5]
#The most ordered is 068f4481b3
top1.order.df <- orders.df[orders.df$sku_ID =="068f4481b3",] 

#Only looking at Direct Discount
new.direct.df<-subset(top1.order.df, direct_discount_per_unit>=0 & quantity_discount_per_unit==0 & bundle_discount_per_unit==0 & coupon_discount_per_unit==0)
top1.price.freq.direct.df <- data.table(new.direct.df, key='final_unit_price')
top1.price.freq.direct.df<-top1.price.freq.direct.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.direct.df<-as.data.frame(top1.price.freq.direct.df)
#top1.price.freq.direct.df<- top1.price.freq.direct.df[-c(11,1,3,4,8,10), ]
plot(top1.price.freq.direct.df$quantity_sold,top1.price.freq.direct.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for top SKU")
top1.price.freq.direct.df %>% arrange(desc(quantity_sold))

#Only looking at Quantity Discount
new.quantity.df<-subset(top1.order.df, bundle_discount_per_unit==0 & direct_discount_per_unit==0 & quantity_discount_per_unit>0 & coupon_discount_per_unit==0)
top1.price.freq.quantity.df <- data.table(new.quantity.df, key='final_unit_price')
top1.price.freq.quantity.df<-top1.price.freq.quantity.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.quantity.df<-as.data.frame(top1.price.freq.quantity.df)
top1.price.freq.quantity.df<-top1.price.freq.quantity.df %>% arrange(desc(quantity_sold))
plot(top1.price.freq.quantity.df$quantity_sold,top1.price.freq.quantity.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for top SKU")

#Only looking at Bundle Discount
new.bundle.df<-subset(top1.order.df, bundle_discount_per_unit>0 & direct_discount_per_unit==0 & quantity_discount_per_unit==0 & coupon_discount_per_unit==0)
top1.price.freq.bundle.df <- data.table(new.bundle.df, key='final_unit_price')
top1.price.freq.bundle.df<-top1.price.freq.bundle.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.bundle.df<-as.data.frame(top1.price.freq.bundle.df)
top1.price.freq.bundle.df<-top1.price.freq.bundle.df %>% arrange(desc(quantity_sold))
top1.price.freq.bundle.df<- top1.price.freq.bundle.df[-c(21,20,19,18,17,16,15,14,13,12), ]
plot(top1.price.freq.bundle.df$quantity_sold,top1.price.freq.bundle.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for top SKU")

#Only looking at Coupon Discount
new.coupon.df<-subset(top1.order.df, bundle_discount_per_unit==0 & direct_discount_per_unit==0 & quantity_discount_per_unit==0 & coupon_discount_per_unit>0)
top1.price.freq.coupon.df <- data.table(new.coupon.df, key='final_unit_price')
top1.price.freq.coupon.df<-top1.price.freq.coupon.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.coupon.df<-as.data.frame(top1.price.freq.coupon.df)
top1.price.freq.coupon.df<-top1.price.freq.coupon.df %>% arrange(desc(quantity_sold))
#top1.price.freq.coupon.df<- top1.price.freq.coupon.df[-c(21,20,19,18,17,16,15,14,13,12), ]
plot(top1.price.freq.coupon.df$quantity_sold,top1.price.freq.coupon.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for top SKU")

#Only looking at Full price
new.full.df<-subset(top1.order.df, direct_discount_per_unit==0 & quantity_discount_per_unit==0 & bundle_discount_per_unit==0 & coupon_discount_per_unit==0)
top1.price.freq.full.df <- data.table(new.full.df, key='final_unit_price')
top1.price.freq.full.df<-top1.price.freq.full.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.full.df<-as.data.frame(top1.price.freq.full.df)
top1.price.freq.full.df<-top1.price.freq.full.df %>% arrange(desc(quantity_sold))
#top1.price.freq.direct.df<- top1.price.freq.direct.df[-c(11,10,9,8,7,1), ]
plot(top1.price.freq.full.df$quantity_sold,top1.price.freq.full.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for top SKU")

#Price and Quantity sold at various prices (looking at all sales)
top1.price.freq.df <- data.table(top1.order.df, key='final_unit_price')
top1.price.freq.df<-top1.price.freq.df[,list(quantity_sold=sum(quantity)), by='final_unit_price']
top1.price.freq.df<-as.data.frame(top1.price.freq.df)
plot(top1.price.freq.df$quantity_sold,top1.price.freq.df$final_unit_price,xlab="Number of Orders", ylab="Price",main="Price vs Orders for Top SKU")



