
library(data.table)

dtAttrAll <- get(load("../output/all_attrs.Rdata"))
lstm <- as.data.table(read.csv("../output/lstm_attributes.csv",header=TRUE))

print(names(dtAttrAll))
print(names(lstm))

attr1 <- "sand_frac"
attr2 <- "sand.1m.percent"
dt1 <- merge(dtAttrAll[,c("id",attr1),with=F], lstm[,c("ID",attr2),with=F],all.x=T, by.x="id",by.y="ID")
dt1[,diff:=get(attr1)-get(attr2)*100]