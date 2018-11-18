library(tidyverse)
library(base)
library(xlsx)
library(statsr)



train_data = data[,!names(data) %in% c("Duration..in.seconds.","StartDate", "EndDate","Status","Progress","Finished","RecordedDate","ResponseId","DistributionChannel","UserLanguage","Q1","Q2","Q4","Q5","Q7_4_TEXT")]
train_data[train_data== ""] = NA
train_data = train_data[rowSums(is.na(train_data))  < 50,]
train_data$Q2...Topics = NULL
train_data$Q2...Topics.1 = NULL
train_data =train_data[-c(1,2),]


train_data$ID = seq.int(nrow(train_data))


for(i in 1:ncol(train_data)){
  if(grepl("Q17",names(train_data[i]))==TRUE){
    train_data[,i] = gsub("\\D", " ",train_data[,i])
  }
}

for(i in 1:ncol(train_data)){
  if(grepl("Q18",names(train_data[i]))==TRUE){
    train_data[,i] = gsub("\\D", " ",train_data[,i])
  }
}

parents_activities = select(train_data,ID,starts_with("Q17"))
parents_activities = sapply(parents_activities,as.numeric)
parents_activities = as.data.frame(parents_activities)
parents_activities$parents_act = rowSums(parents_activities[,2:13],na.rm = TRUE)



kids_activities = select(train_data,ID,starts_with("Q18"))
kids_activities = sapply(kids_activities,as.numeric)
kids_activities = as.data.frame(kids_activities)
kids_activities$kids_act = rowSums(kids_activities[,2:13],na.rm = TRUE)



temp = cbind.data.frame(parents_activities,kids_activities)
temp2 = cbind.data.frame(kids_no_outliers,no_outliers)
complete_data = data.frame("ID" = temp$ID,"parents_activities" = temp$parents_act,"kids_activities"= temp$kids_act)

complete_data = cbind(complete_data,train_data$Q30,train_data$Q33,train_data$Q38,train_data$Q42,train_data$Q43,train_data$Q47,train_data$Q51,train_data$Q52,train_data$Q53,train_data$Q54,train_data$Q56,train_data$Q55,train_data$Q6,train_data$Q7,train_data$Q8,train_data$Q10)



ggplot(complete_data, aes(x=parents_activities)) + 
  geom_histogram(bins=32, closed='left', colour='black', aes(fill=..count..)) +
  scale_fill_gradient('Count', low='blue', high ='orange')




ggplot(complete_data, aes(x=kids_activities)) + 
  geom_histogram(bins=32, closed='left', colour='black', aes(fill=..count..)) +
  scale_fill_gradient('Count', low='blue', high ='orange')



cor(complete_data$parents_activities,complete_data$kids_activities)


bplot = ggplot(complete_data,aes(parents_activities,kids_activities,fill = parents_activities))
bplot + geom_point(aes(colour = factor(kids_activities)))
linearModel <- lm(kids_activities ~ parents_activities, data=complete_data)
summary(linearModel)


colnames(complete_data)[4]= "Q30"
colnames(complete_data)[5]= "Q33"
colnames(complete_data)[6]= "Q38"
colnames(complete_data)[7]= "Q42"
colnames(complete_data)[8]= "Q43"
colnames(complete_data)[9]= "Q47"
colnames(complete_data)[10]= "Q51"
colnames(complete_data)[11]= "Q52"
colnames(complete_data)[12]= "Q53"
colnames(complete_data)[13]= "Q54"
colnames(complete_data)[14]= "Q56"
colnames(complete_data)[15]= "Q55"



kruskal.test(parents_activities~Q30,data=complete_data)
kruskal.test(parents_activities~Q33,data=complete_data)
kruskal.test(parents_activities~Q38,data=complete_data)
kruskal.test(parents_activities~Q42,data=complete_data)
kruskal.test(parents_activities~Q47,data=complete_data)
kruskal.test(parents_activities~Q51,data=complete_data)
kruskal.test(parents_activities~Q52,data=complete_data)
kruskal.test(parents_activities~Q53,data=complete_data)
kruskal.test(parents_activities~Q54,data=complete_data)
kruskal.test(parents_activities~Q56,data=complete_data)
kruskal.test(parents_activities~Q55,data=complete_data)

kruskal.test(kids_activities~Q30,data=complete_data)
kruskal.test(kids_activities~Q33,data=complete_data)
kruskal.test(kids_activities~Q38,data=complete_data)
kruskal.test(kids_activities~Q42,data=complete_data)
kruskal.test(kids_activities~Q47,data=complete_data)
kruskal.test(kids_activities~Q51,data=complete_data)
kruskal.test(kids_activities~Q52,data=complete_data)
kruskal.test(kids_activities~Q53,data=complete_data)
kruskal.test(kids_activities~Q54,data=complete_data)
kruskal.test(kids_activities~Q56,data=complete_data)
kruskal.test(kids_activities~Q55,data=complete_data)


bplot = ggplot(complete_data,aes(Q56,parents_activities,fill = parents_activities))
bplot + geom_boxplot(aes(group = cut_width(parents_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)

bplot = ggplot(complete_data,aes(Q56,kids_activities,fill = kids_activities))
bplot + geom_boxplot(aes(group = cut_width(kids_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)


bplot = ggplot(complete_data,aes(Q53,kids_activities,fill = kids_activities))
bplot + geom_boxplot(aes(group = cut_width(kids_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)


 
bplot = ggplot(complete_data,aes(Q55,kids_activities,fill = kids_activities))
bplot + geom_boxplot(aes(group = cut_width(kids_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)


complete_data_no_outliers = complete_data[complete_data$parents_activities < 40 & complete_data$kids_activities < 40,]


ggplot(complete_data_no_outliers, aes(x=parents_activities)) + 
  geom_histogram(bins=32, closed='left', colour='black', aes(fill=..count..)) +
  scale_fill_gradient('Count', low='blue', high ='orange')




ggplot(complete_data_no_outliers, aes(x=kids_activities)) + 
  geom_histogram(bins=32, closed='left', colour='black', aes(fill=..count..)) +
  scale_fill_gradient('Count', low='blue', high ='orange')



cor(complete_data_no_outliers$parents_activities,complete_data_no_outliers$kids_activities)


bplot = ggplot(complete_data_no_outliers,aes(parents_activities,kids_activities,fill = parents_activities))
bplot + geom_point(aes(colour = factor(kids_activities)))
linearModel <- lm(kids_activities ~ parents_activities, data=complete_data_no_outliers)
summary(linearModel)


colnames(complete_data_no_outliers)[4]= "Q30"
colnames(complete_data_no_outliers)[5]= "Q33"
colnames(complete_data_no_outliers)[6]= "Q38"
colnames(complete_data_no_outliers)[7]= "Q42"
colnames(complete_data_no_outliers)[8]= "Q43"
colnames(complete_data_no_outliers)[9]= "Q47"
colnames(complete_data_no_outliers)[10]= "Q51"
colnames(complete_data_no_outliers)[11]= "Q52"
colnames(complete_data_no_outliers)[12]= "Q53"
colnames(complete_data_no_outliers)[13]= "Q54"
colnames(complete_data_no_outliers)[14]= "Q56"
colnames(complete_data_no_outliers)[15]= "Q55"



kruskal.test(parents_activities~Q30,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q33,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q38,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q42,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q47,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q51,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q52,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q53,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q54,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q56,data=complete_data_no_outliers)
kruskal.test(parents_activities~Q55,data=complete_data_no_outliers)

kruskal.test(kids_activities~Q30,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q33,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q38,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q42,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q47,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q51,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q52,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q53,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q54,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q56,data=complete_data_no_outliers)
kruskal.test(kids_activities~Q55,data=complete_data_no_outliers)

bplot = ggplot(complete_data_no_outliers,aes(Q56,kids_activities,fill = kids_activities))
bplot + geom_boxplot(aes(group = cut_width(kids_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)

bplot = ggplot(complete_data_no_outliers,aes(Q47,parents_activities,fill = parents_activities))
bplot + geom_boxplot(aes(group = cut_width(parents_activities, 1)),outlier.color = "red",outlier.shape = 16,outlier.size = 2)
