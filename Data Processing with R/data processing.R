
data = read.csv("P3-Future-500-The-Dataset.csv",na.strings=c(""))

backup = data

str(data)
head(data)
tail(data)

summary(data)

data$ID <- factor(data$ID)
data$Inception <- factor(data$Inception)
data$Profit <- factor(data$Profit)

data$Profit <- backup$Profit

#replace unwanted pattern with empty space. 
head(data)
data$Expenses <- gsub("Dollars","",data$Expenses)
data$Expenses <- gsub(",","",data$Expenses)
data$Growth <- gsub("%","",data$Growth)
data$Revenue<- gsub(",","",data$Revenue)
data$Revenue<- gsub("\\$","",data$Revenue)

str(data)

#now revenue , expences  and growth needs to be recignise as numeric. 

data$Revenue <- as.numeric(data$Revenue)
data$Expenses <- as.numeric(data$Expenses)
data$Growth <- as.numeric(data$Growth)

str(data)
summary(data)

head(data,25)

data[which(data$Revenue == 9746272),]

data[is.na(data$Expenses),]

data[is.na(data$Revenue),]

#removing missing data

backup1 <- data;

data[is.na(data$Industry),]

data<- data[!is.na(data$Industry),]

data[is.na(data$State) & data$City == "New York",]  # show state = NA w.r.t new york city

data[is.na(data$State) & data$City == "New York", "State"] <- "NY" # replaced NA value with NY. 

data[c(11,377),]

data[!complete.cases(data),]

data[is.na(data$State) & data$City == "San Francisco", "State"] <- "CA"

# median imputation #
data[! complete.cases(data),]

#find out the median of employees
median(data[,"Employees"], na.rm = TRUE)
mean(data[,"Employees"], na.rm =  TRUE)

med_emp_retail <- median(data[data$Industry == "Retail", "Employees"], na.rm = TRUE)

data[is.na(data$Employees) & data$Industry == "Retail", "Employees"] <- med_emp_retail

med_emp_finserv <- median(data[data$Industry == "Financial Services", "Employees"], na.rm = TRUE)

data[is.na(data$Employees) & data$Industry =="Financial Services", "Employees" ] <- med_emp_finserv

#replace missing value of growth 

med_growth <-median(data[data$Industry == "Construction", "Growth"], na.rm = TRUE)

data[is.na(data$Growth) & data$Industry == "Construction", "Growth"] <- med_growth

data[! complete.cases(data),]

#fill profit for construction industry. 

median (data[data$Industry == "Construction", "Profit"], na.rm = TRUE) -> med_con_profit

data[is.na(data$Profit) & data$Industry == "Construction", "Profit"] <- med_con_profit

#---------calculate median of revenue for cinstruction industry. 
med_rev_con <- median(data[data$Industry == "Construction", "Revenue"], na.rm = TRUE)
data[is.na(data$Revenue) & data$Industry == "Construction", "Revenue" ]<- med_rev_con 

data[! complete.cases(data),]



#now we will calculate missing vale in revenue expenses and  profit
#profit = revenue - expenses
#expenses  = revenue - profit

data[is.na(data$Expenses),"Expenses"] <- data[is.na(data$Expenses),"Revenue"] - data[is.na(data$Expenses),"Profit"]

#take a backup. 
backup2 <- data
fin<- data
# visulizing #


#scatter plot revenue , expenses and profit. 
library(ggplot2)

p<-  ggplot(fin)

p + geom_point(aes(x = Revenue, y = Expenses,
                   colour = Industry, size = Profit))


d <- ggplot(data = fin, aes(x = Revenue, y = Expenses,
                        colour = Industry))

# add smoother and trend 

d + geom_point()+ geom_smooth(fill = NA, size = 1.2)
                
   
#box plot 

f <- ggplot(data = fin,
            aes (x = Industry, 
                 y = Growth,
                 colour = Industry))

f + geom_boxplot()

f + geom_jitter() +geom_boxplot(size = 1, outlier.alpha =NA)






                   
                
                   
                  
