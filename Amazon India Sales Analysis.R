
print("amazon india sales Analysis 2022")

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

df=read.csv('E:\\EXTRA\\Amazon Sale Report.csv')

View(df)
str(df)
summary(df)
nrow(df)
ncol(df)

# Cleaning and Transforming 

df<-df[,!names(df) %in% c('index','Sales.Channel','currency',"ship.country",'Unnamed..22')]

# Filled NA Values to 0
df$Amount <-replace(df$Amount,is.na(df$Amount),0)

colSums(is.na(df))

# filled null values to NA
df[df =='' |df==' '] <- NA

colSums(is.na(df))

# Replace NA to Unknown
df <- replace(df,is.na(df),'unknown')

apply(df,2,function(x) any(is.na(x)))

class(df$Date)
#Converting Date column 'character' to 'date'
df$Date <-mdy(df$Date)

class(df$Date)                           

# arranging dates in ascending order
df <- arrange(df,by=Date)
---------------------------
# amount summary 
Amount_summary= df%>%
         summarise(mean_amount=  mean(df$Amount),
         max_amount= max(df$Amount),
         min_amount=min(df$Amount),
         median_amount=median(df$Amount),
         sum_amount=sum(Amount))


Amount_summary
count(df)
sum(df$Qty)

-----------------------------------------------------------------------------(1)

par(mfrow=c(1,2))
# box plot on Amount Distribution
boxplot(df$Amount,main="Boxplot of Amount Distribution",
        horizontal = T,notch = T,col = "lightblue")
# hist graph of Amount Distribution
hist(df$Amount,xlab='Amount',main = "Histogram of Amount Distribution",col ='lightblue')
-----------------------------------------------------------------------------(2)
library(patchwork)

# scatter graph on Amount vs Qty
x <- ggplot(df,aes(Amount,Qty,color=Fulfilment)) +geom_point()+
     labs(title="Amount vs Quantity")+theme_bw()


print(x) 

# hist graph on Qty over Amount distribution
par(mfrow=c(1,1))
     hist(df$Amount,main="Qunatity Over Amount distribution",xlab ='Amount',xlim=c(0,4000),
     col="lightgreen",border='black')
     text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.15))

-----------------------------------------------------------------------------(3)
# Category wise Total Sale (Bar) and Total Qty (Line)  

df_cat1 =  df %>%
           group_by(Category) %>%
           summarise(Total_Qty = sum(Qty),Total_amount=sum(Amount))%>%
           select(Category,Total_Qty,Total_amount)


# viz1
p1= ggplot(df_cat1,aes(x=Category))+
    geom_bar(aes(y=Total_amount),stat='identity',col="black",fill='lightgreen')+
    geom_line(aes(y=Total_Qty*350),size=1,color='red',group=1)+
    geom_text(aes(y=Total_amount,label=format(Total_amount,big.mark=",",
                   scientific_format=F)),size=3.5,vjust=-0.5,col='blue4')+
    geom_text(aes(y=Total_Qty,label=Total_Qty),size=3.5,vjust=1,color='red')+
    scale_y_continuous(sec.axis=sec_axis(~./350,name='Items Sold'),
                     labels=label_number(suffix="M",scale=1e-6))+
    labs(title='Category wise Total Sale Bar and Total Quantity (Line)',x='Category',y='Amount')+
    theme_bw()
    legend("topleft",fill=c('lightgreen','red'),legend=c('Total Sale','Total Qty'),cex=0.8)


print(p1)
-----------------------------------------------------------------------------(4)
# Top 3 Category wise total qty and total amount
df_cat =  df %>%
          mutate(month=format(as.Date(df$Date),"%m")) %>%
          group_by(month,Category) %>%
          summarise(Total_Qty = sum(Qty),Total_amount=sum(Amount))%>%
          select(month,Category,Total_Qty,Total_amount)%>%
          top_n(3)

df_cat

#category wise total Qty viz
k1=  ggplot(df_cat,aes(x=Category,y=Total_Qty,fill=month,label=Total_Qty))+
  geom_bar(stat="identity",position='stack')+
  geom_text(size=3,position=position_stack(vjust = 0.5))+
  labs(title='Monthly top 3 Category wise items sold',y="Quantity")+
  scale_y_continuous(labels = label_number(suffix = "K",scale = 1e-3))

#category wise total Sale viz
k2=ggplot(df_cat,aes(x=Category,y=Total_amount,fill=month,
                     label=paste(round(Total_amount / 1e6, 1), "M")))+
  geom_bar(stat="identity",position='stack')+
  geom_text(size=3,position=position_stack(vjust = 0.5))+
  labs(title='Monthly top 3 Category total sale',y="Amount")+
  scale_y_continuous(labels = label_number(suffix = "M",scale=1e-6))

k1+k2

-----------------------------------------------------------------------------(5)
# monthly sale and Qty
# Monthly Total sale
df_month =df_cat %>%
  group_by(month) %>%
  summarise(amount=sum(Total_amount),qty=sum(Total_Qty)) %>%
  select(month,amount,qty)



m_sum = ggplot(df_month,aes(month,amount,
        label=paste(round(amount/1e6),"M"),fill=month))+
        geom_bar(stat='identity')+
        geom_text(size=4,color='red',vjust=-0.5)+
        theme_grey()+
        labs(title=' Monthly Total Sale',x='Month',y='Amount')+
        scale_y_continuous(label=label_number(suffix = 'M',scale=1e-6))

# Monthly no of items sale
m_qty = ggplot(df_month,aes(month,qty,
        label=paste(round(qty/1e3),"K"),fill=month))+
        geom_bar(stat='identity')+
        geom_text(size=4,color='red',vjust=-0.5)+
        theme_grey()+
        labs(title=' Monthly Total Qty Sale',x="Month",y='Qty')+
        scale_y_continuous(label=label_number(suffix = "K",scale=1e-3))

# *** side by side
dev.new(width=5,height=5,unit="in")
cowplot::plot_grid(m_sum,m_qty)

-----------------------------------------------------------------------------(6)
# percentage of monthly Sales and Qty
## monthly total sales


n=c('March','April','May','June')
percent<-round(100*df_month$amount/sum(df_month$amount),1)

# *** viz
par(mfrow=c(1,2))
pie(df_month$amount,labels = percent,main='Monthly Percentage of Total Sales',
    col=rainbow(length(df_month$amount)))
legend('bottomleft',n,cex=0.8,fill=rainbow(length(df_month$amount)))

pie(df_month$qty,labels=round(100*df_month$qty/sum(df_month$qty)),
    main='Monthly Percentage of Total Qty Sales',
    col=rainbow(length(df_month$qty)))
legend('bottomleft',n,cex=0.8,fill=rainbow(length(df_month$qty)))

---------------------------------------------------------------------------- (7)
## status wise total qty and total amount
df_status <- df %>%
            group_by(Status) %>%
            summarise(Total_Qty=sum(Qty),Total_amount=sum(Amount)) %>%
            select(Status,Total_Qty,Total_amount)

df_status

# *** status wise total qty and total amount viz
p2 = ggplot(df_status,aes(x=Status,y=Total_Qty,label=Total_Qty))+
     geom_bar(stat='identity',fill='lightgreen',col="black")+
     geom_text(size=4,hjust=-0.15,color='red2')+coord_flip()+
     theme_bw()+
     labs(title="Status wise Total Qty Sales ",y="Total Qty")

print(p2)
  
-----------------------------------------------------------------------------(8)
# Size wise total Qty and Total amount
df_size=df %>%
        group_by(Size) %>%
        summarise(total_qty=sum(Qty),total_amount=sum(Amount)) %>%
        select(Size,total_qty,total_amount)

df_size

p3= ggplot(df_size, aes(x="", y=total_qty, fill=Size)) +
    geom_bar(stat="identity",col=rainbow(length(df_size$total_qty))) +
    coord_polar("y",start = 0)+
    geom_text(aes(label =round(100*total_qty/sum(total_qty),1)),
            position = position_stack(vjust = 0.5),size=5)+
    theme_void()+
    labs(title="Percentage of Size wise total Qty Sold")


print(p3)

-----------------------------------------------------------------------------(9)
# Shipment type 
df_ful= df %>%
  group_by(Fulfilment,ship.service.level)%>%
  summarise(famount=sum(Amount))%>%
  select(Fulfilment,famount,ship.service.level)
  
df_ful

# bar
w1=ggplot(df_ful,aes(Fulfilment,y=famount,fill=ship.service.level,
                  label=paste(round(famount/1e6),"M")))+
  geom_bar(stat='identity')+
  geom_text(size=4,vjust=-0.5)+
  labs(title='Fulfilment type',y='Amount')+
  scale_y_continuous(label=label_number(suffix = 'M',scale=1e-6))

# pie
w2=ggplot(df_ful, aes(x="", y=famount, fill=Fulfilment)) +
  geom_bar(stat="identity") +
  coord_polar("y")+
  geom_text(aes(label =round(100*famount/sum(famount),1)),
            position = position_stack(vjust = 0.5),size=5)+
  theme_void()+
  scale_fill_manual(values = c("green2",'orange'))+
  guides(fill = guide_legend(title = "Status"))+
  labs(title="Percentage of Fulfilment type")


cowplot::plot_grid(w1,w2)

w1+w2
----------------------------------------------------------------------------(10)
# courier status
df_courier= df%>%
  group_by(Courier.Status)%>%
  summarise(ccount= table(Courier.Status))%>%
  select(Courier.Status,ccount)
  
df_courier

# bar
z1=ggplot(df_courier,aes(Courier.Status,ccount,label=ccount))+
  geom_bar(stat='identity',fill='lightblue')+
  geom_text(size=4,position=position_dodge(),color='red',vjust=-0.5)+
  theme_minimal()+
  labs(title='Courier status',x='Courier Status',y='Qty')

#pie
z2=  ggplot(df_courier, aes(x="", y=ccount, fill=Courier.Status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  geom_text(aes(label =round(100*ccount/sum(ccount),1)),
            position = position_stack(vjust = 0.5),size=5)+
  theme_void()+
  scale_fill_manual(values = c("red","green2","skyblue",'orange'))+
  guides(fill = guide_legend(title = "Status"))+
  #theme(legend.position = "bottom")+
  labs(title="Percentage of Courier Status")
  
z1+z2

----------------------------------------------------------------------------(11)
# Top 10 state wise total sales and Total items sold
df_state= df%>%
          group_by(ship.state)%>%
          summarise(state_count=sum(Qty),state_sum=sum(Amount)) %>%
          select(ship.state,state_count,state_sum)%>%
          arrange(desc(state_count))%>%
          top_n(10)

df_state

s1= ggplot(df_state)+
    geom_col(aes(x=ship.state,y=state_sum),col='black',fill='purple')+
    geom_line(aes(x=ship.state,y=state_count*300),size=1.5,color='red',group=1)+
    geom_text(aes(x=ship.state,y=state_sum,label=paste(round(state_sum/1e6),"M")),
              size=4,vjust=-0.5,col="grey3")+
    geom_text(aes(x=ship.state,y=state_count,label=paste(round(state_count/1e3),"K")),
              size=4,color='darkblue',vjust=1.5)+
    scale_y_continuous(sec.axis=sec_axis(~./300, name='Qty'),
                       labels=label_number(suffix = 'M',scale=1e-6))+
    labs(title='Top 10 state Total Sales  and Total Quantity Sales (Line)',
         x='State',y='Total Sales')+theme_bw()
    legend("topleft",col=c('purple','red'),legend=c('Total Sale','Total Qty'),cex=0.8)
 

dev.new(width=5,height=5,unit='in')
print(s1)
  --------------------------------------------------------------------------(12)
# Top 10 City Total Sales and Items Sold
 df_city= df%>%
            group_by(ship.city) %>%
            summarise(citysum=sum(Amount),citycount=sum(Qty))%>%
            select(ship.city,citysum,citycount)%>%
            arrange(desc(citycount))%>%
            top_n(10)
            
df_city

c1 =ggplot(df_city,aes(x=ship.city))+
    geom_bar(aes(y=citysum),stat='identity',col='black',fill='darkblue')+
    geom_line(aes(y=citycount*300),size=1.5,color='red2',group=1)+
    geom_text(aes(y=citysum,label=paste(round(citysum/1e6),"M")),size=4,
              vjust=-0.5,col="grey3")+
    geom_text(aes(y=citycount,label=paste(round(citycount/1e3),"K")),size=3.5,
              vjust=1.5,col="red4")+
    scale_y_continuous(sec.axis=sec_axis(~./300,name='Qty'),
                       labels = label_number(suffix='M',scale=1e-6))+
    labs(title='Top 10 City Total Sales and Total Quantity Sales (Line)',x='City',y="Sales")+
  theme_bw()
    

print(c1)
-------------------------------------  -----------------------------------------
print("CONCLUSTION")
      
print(" > mean and median value of the sales are same.      
       >  Highest sales comes from Set around 39M.     
       >  During April and May has more sales.      
       >   Free and L size has more order.     
       >   Expidited Delivery  has more sales than standard.
       >   State wise Maharastra  has higest sales around 13M.   
       >   City wise Banglore has higest sales around 7M.")

