
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2", "\n")[[1]]  # Example
inp <- get_input("2016/10", parse = F, user = "blakeebets", cache = T)

inp

proc <- rep(FALSE, length(inp))
bots <- list()
output <- list()

while(any(!proc)) {
    for(i in 1:length(inp)) {
        if(proc[i]) {
            next
        }
        
        ls <- strsplit(inp[i], " ")[[1]]
        
        if(ls[1] == "value") {
            bots[[ls[6]]] <- c(bots[[ls[6]]], as.numeric(ls[2]))
            proc[i] <- TRUE
        } else if(length(bots[[ls[2]]]) == 2) {
            inv <- sort(bots[[ls[[2]]]])
            
            type_lo <- ls[6]
            target_lo <- ls[7]
            
            type_hi <- ls[11]
            target_hi <- ls[12]
            
            if(type_lo == "bot") {
                bots[[target_lo]] <- c(bots[[target_lo]], inv[1])
            } else {
                output[[target_lo]] <- c(output[[target_lo]], inv[1])
            }
            
            if(type_hi == "bot") {
                bots[[target_hi]] <- c(bots[[target_hi]], inv[2])
            } else {
                output[[target_hi]] <- c(output[[target_hi]], inv[2])
            }
                
            proc[i] <- TRUE
        }
    }
}

bots[sapply(bots, function(x) identical(sort(x), c(17, 61)))]

# 7:03 (2nd)

output[["0"]] * output[["1"]] * output[["2"]]

# 7:20 (1st)
