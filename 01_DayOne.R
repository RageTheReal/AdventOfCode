rm(list = ls())

input <- read.table("01_DayOneInput.txt", na.strings = TRUE,blank.lines.skip = FALSE)


fPE <- c(1,which(is.na(input)),dim(input)[1])
elfChart <- 1: (length(fPE) - 1)
rowName <- 1: (length(fPE) - 1)

for (i in 1:(length(fPE) - 1) ) {
  
  sub <- input[ fPE[i]:fPE[(i+1)] ,1]
  elfChart[i] <- sum(sub, na.rm = T)
  rowName[i] <- paste0("Elf ",i)
}

myElves <- data.frame("Elf" = rowName, "Calories" = elfChart)
maxCalories <- which(myElves$Calories == max(myElves$Calories))
print(myElves[maxCalories,])

orderedCalories <- sort.default(myElves$Calories,decreasing = TRUE)
max <- sum(orderedCalories[1:3])
print(max)

