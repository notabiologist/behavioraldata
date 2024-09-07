data3 <-read.csv("population1_vinitha_2months.csv")


# loop scaling the data @copilot

for (i in 2:8){
  data3[,i] <- scale(data3[,i])
}


