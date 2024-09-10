#load data
vinitha8month <- read.csv("C:/Users/depha/Downloads/vinitha8month.csv")
dephanakshiti <- read.csv("C:/Users/depha/Downloads/dephanakshiti.csv")
apoorvalonavla <- read.csv("C:/Users/depha/Downloads/apoorvalonavla.csv")#lonovala
febdephakrepday1 <- read.csv("C:/Users/depha/Downloads/febdephakrepday1.csv")


vinitha8month$Emergence.time
apoorvalonavla$Emergence.time
dephanakshiti$Emergence.time
febdephakrepday1$Emergence.time

result <- kruskal.test(list(
        vinitha8month$Emergence.time,
        apoorvalonavla$Emergence.time,
        dephanakshiti$Emergence.time))

dunn_result <- dunn.test(list(
        vinitha8month$Emergence.time,
        apoorvalonavla$Emergence.time,
        dephanakshiti$Emergence.time), method = "bonferroni")
