
rm(list = ls())

input <- read.table("03_DayThreeInput.txt")#, na.strings = TRUE,blank.lines.skip = FALSE)

priorities <- data.frame("priority" = 1:52, "letter" = c(letters[seq(1,26)],LETTERS[seq(1,26)]))

sum <- 0 

hi <- function(x){
    
    l <- strsplit(left,"")[[1]] 
    r <- strsplit(right,"")[[1]][x]
    ans <- stringi::stri_detect(l,pattern = r ,fixed = F) ## compare the right side letters to the left side one
  
    if( ! identical(character(0),l[ans]) ){
      
      return(l[ans][1])
      
    }else{
      
      return(NA) 
    
  }
}
  

## Part 1: -----------------------------------------------------------------------------------
for (i in 1:dim(input)[1]) {
  
  lengthCompart <- nchar(input[i,1])/2
  
  splitAt <- paste0("(?<=.{",lengthCompart,"})")
  
  sol <- strsplit(input[i,1], splitAt, perl = TRUE)[[1]]
  
  left <- sol[1]
  right <- sol[2]
  
  finalLetter <- na.omit(sapply(1:lengthCompart,hi))[1] ## if sth is duplicated -> here it gets truncated
  
  sum <- sum + priorities$priority[which(priorities$letter == finalLetter)]
  
}

cat("The sum of all priorities of part 1 is:",sum,"\n")

## Part 2: -----------------------------------------------------------------------------------

sumTeams <- 0

sequenc <- seq(from = 0, to = dim(input)[1], by = 3)

for (j in  1:(length(sequenc)-1) ) {
  
  sub <- input[(sequenc[j]+1):sequenc[j+1] ,1]
  sub <- data.frame("items" = sub, "length" = nchar(sub))
  sub <- sub[order(sub$length),]
  
  finalLetter <- na.omit(sapply(1:sub$length[1], function(x) {
    
    first <- strsplit(sub[1,1],"")[[1]][x] ## first searches the other two
    second <- strsplit(sub[2,1],"")[[1]]
    third <- strsplit(sub[3,1],"")[[1]]
    
    ansS <- stringi::stri_detect(second,pattern = first ,fixed = F) 
    ansT <- stringi::stri_detect(third,pattern = first ,fixed = F)
    
    if( !identical(character(0),second[ansS]) & !identical(character(0),third[ansT])  ){
      
      return(second[ansS][1])
      
    }else{
      
      return(NA) 
      
    }
  }))[1]
  
  sumTeams <- sumTeams + priorities$priority[which(priorities$letter == finalLetter)]
  
}

cat("The sum of all priorities of part 1 is:",sumTeams)

