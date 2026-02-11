print("test")
whale<-read.table("whaledata.txt",
                 header=TRUE,
                 sep="\t")
str(whale)

#6. 
#7
summary (whale)
#8
#a.extract first 10 rows abd first 4 columns
whale.sub<- whale[1:10, 1:4]
whale.sub
#b. all rows, but only some columns by name
whale.num<-whale[,c("month", "water.noise", "number.whales")]
whale.num
#c. first 50 rows, all columns
whale.may<- whale[1:50, ]
whale.may
#d. all rows except first 10, all columns except last column
whale.cut<-whale[-(1:10), -ncol(whale)]
whale.cut

#9