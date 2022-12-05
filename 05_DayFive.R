##  without parser -> removed the blanks and split the two text parts 
##  with the second solution (Line 45: <- theOrder[length(theOrder):1]; for the first solution remove the part after "theOrder")

rm(list = ls())

setwd("")
input2 <- read.table("05_DayFiveInput_2.txt",
                     sep = "",
                     col.names = c("1","move","2","source","3","destination"))[,c(2,4,6)]

input1 <- read.table("05_DayFiveInput_1.txt",fill = T,
                     na.strings = F,
                     blank.lines.skip = FALSE,
                     header = FALSE,
                     sep = " ",
                     strip.white = F)

input1[input1 == ""] <- NA
input1 <- input1[dim(input1)[1]:1,] 


for (i in 1:dim(input2)[1]) {
  
  destNA <- which(is.na(input1[input2$destination[i]]))
  sourceNA <- which(is.na(input1[input2$source[i]]))
  
  if( identical(integer(0),sourceNA) )
    sourceNA <- dim(input1)[1] + 1
  
  if( identical(integer(0),destNA) )
    destNA <- dim(input1)[1] +1
  
  if(sourceNA[1] == (dim(input1)[1] + 1) ){
    
    rows <- (sourceNA[1] - 1):( sourceNA[1] - input2$move[i] )
    
  }else{
    
    rows <- (sourceNA[1] - 1):( sourceNA[1] - input2$move[i] )
    
  }
  
  theOrder <- na.omit(input1[ rows, input2$source[i] ])
    
  input1[ destNA[1]:(destNA[1]-1 + input2$move[i]), input2$destination[i] ] <- theOrder[length(theOrder):1]
    
  input1[ rows, input2$source[i] ] <- NA
    
}

solution <- ""
for (j in 1:ncol(input1)) {
  
  nonNA <- input1[!is.na(input1[,j]),j]
  solution <- paste0(solution,nonNA[length(nonNA)])
  
}

solution <- stringr::str_replace_all(solution,"[\\[-\\]]","") ## klammern
print(solution)

