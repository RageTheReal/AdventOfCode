
rm(list = ls())

input <- read.table("02_DayTwoInput.txt")#, na.strings = TRUE,blank.lines.skip = FALSE)





## First Solution: -----------------------------------------------
# 
# score <- 0
# input$Score <- NA
# input$Desc <- NA
# 
# pointCalculator <- function(hisDec, myDec){
#   
#   if( (hisDec == "A" & myDec == "X" ) |
#       (hisDec == "B" & myDec == "Y" ) |
#       (hisDec == "C" & myDec == "Z" ) ){
#     
#     draw <- 3 
#     return(c(draw,"draw"))
#     
#   }else if( (hisDec == "A" & myDec == "Y" ) |
#             (hisDec == "B" & myDec == "Z" ) |
#             (hisDec == "C" & myDec == "X" ) ) {
#     
#     win <- 6 
#     return(c(win,"win"))
#     
#   }else if( (hisDec == "A" & myDec == "Z" ) |
#             (hisDec == "B" & myDec == "X" ) |
#             (hisDec == "C" & myDec == "Y" ) ) {
#     
#     loss <- 0
#     return(c(loss,"lose"))
#     
#   }
#   
# }
# 
# for (i in 1:nrow(input)) {
#   
#   myDecision <- input[i,2]
#   hisDec <- input[i,1]
# 
#     switch (myDecision,
#     "X" = {
#       
#       sol <- pointCalculator(hisDec,myDecision)
#       points <- 1 + as.integer(sol[1])
#       input$Desc[i] <- sol[2]
#       
#       },
#     "Y" = {
#       
#       sol <- pointCalculator(hisDec,myDecision)
#       points <- 2 + as.integer(sol[1])
#       input$Desc[i] <- sol[2]
#       
#       },
#     "Z" = {
#       
#       sol <- pointCalculator(hisDec,myDecision)
#       points <- 3 + as.integer(sol[1])
#       input$Desc[i] <- sol[2]      
#     }
#   )
#   
#   input$Score[i] <- points
#   
#   
#   score <- score + points
#   
#   cat("I",input$Desc[i],"with",input$Score[i],"points!\n")
#   
# }


## Second Solution ----------------------------------------------------------

score <- 0


pointCalculator <- function(hisDec, myDec){
  
  
  
}

for (i in 1:nrow(input)) {
  
  myDecision <- input[i,2]
  hisDec <- input[i,1]
  
  switch (myDecision,
          "X" = {
            
            switch (hisDec,
              "A" = p  <- 3,
              "B" = p <- 1,
              "C" = p <- 2
            )
            points <- 0 + p
            
          },
          "Y" = {

            switch (hisDec,
                    "A" = p  <- 1,
                    "B" = p <- 2,
                    "C" = p <- 3
            )
            points <- 3 + p
              
          },
          "Z" = {
            
            switch (hisDec,
                    "A" = p  <- 2,
                    "B" = p <- 3,
                    "C" = p <- 1
            )
            points <- 6 + p
            
          }
  )
  
  score <- score + points
  
}

cat("The final score is:",score)

