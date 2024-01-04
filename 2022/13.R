rm(list=ls())
source("utilities.R")
inp <- get_input("2022/13", parse = F, deesblake = F, cache = T)

inp <- strsplit("[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]", "\n")[[1]]

i1 <- inp[seq(1, length(inp), 3)]
i2 <- inp[seq(2, length(inp), 3)]


compare <- function(left, right, verbose = F) {
    if(verbose) print(sprintf("Compare %s vs %s", 
                  sprintf("[%s]", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(left, collapse = ","))))), 
                  sprintf("[%s]", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(right, collapse = ",")))))))
    
    if(length(left) >= 1) {
        for(j in 1:length(left)) {
            if(j > length(right)) {
                if(verbose) print("Right side ran out of items, so inputs are not in the right order")
                return(FALSE)
            }
            
            if(is.numeric(left[[j]]) && is.numeric(right[[j]])) {
                if(verbose) print(sprintf("Compare %d vs %d", left[[j]], right[[j]]))
                if(left[[j]] > right[[j]]) {
                    if(verbose) print("Right side is smaller, so inputs are not in the right order")
                    return(FALSE)
                } else if(left[[j]] < right[[j]]) {
                    if(verbose) print("Left side is smaller, so inputs are in the right order")
                    return(TRUE)
                }
            } else if(is.list(left[[j]]) && is.list(right[[j]])) {
                res <- compare(left[[j]], right[[j]])
                if(!is.na(res)) {
                    return(res)
                }
            } else if(is.list(left[[j]]) && is.numeric(right[[j]])) {
                if(verbose) print(sprintf("Compare %s vs %s", 
                              sprintf("[%s]", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(left[[j]], collapse = ","))))), 
                              sprintf("%s", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(right[[j]], collapse = ",")))))))
                if(verbose) print(sprintf("Mixed types; convert right to [%s] and retry comparison", right[[j]]))
                res <- compare(left[[j]], as.list(right[[j]]))
                if(!is.na(res)) {
                    return(res)
                }
            } else if(is.numeric(left[[j]]) && is.list(right[[j]])) {
                if(verbose) print(sprintf("Compare %s vs %s", 
                              sprintf("%s", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(left[[j]], collapse = ","))))), 
                              sprintf("[%s]", gsub("\\)", "\\]", gsub("list\\(", "\\[", gsub(" ", "", paste(right[[j]], collapse = ",")))))))
                if(verbose) print(sprintf("Mixed types; convert left to [%s] and retry comparison", left[[j]]))
                res <- compare(as.list(left[[j]]), right[[j]])
                if(!is.na(res)) {
                    return(res)
                }
            }
        }
    }
    
    if(length(right) > length(left)) {
        if(verbose) print("Left side ran out of items, so inputs are in the right order")
        return(TRUE)        
    }
    
    return(NA)
}


z <- c()
i <- 1
for(i in 1:length(i1)) {
    res <- compare(left, right)
    if(res) z <- c(z, i)
    print(sprintf("%d: %s", i, res))
}

sum(z)

# Part 2 ------------------------------------------------------------------

dividers <- c("[[2]]", "[[6]]")
packets <- c(i1, i2, dividers)

shuffle <- TRUE
while(shuffle) {
    shuffle <- FALSE
    
    for(i in 1:(length(packets) - 1)) {
        left <- eval(parse(text = gsub("\\]", ")", gsub("\\[", "list(", packets[i]))))
        right <- eval(parse(text = gsub("\\]", ")", gsub("\\[", "list(", packets[i+1]))))
        
        if(!compare(left, right)) {
            # Swap
            tmp <- packets[i]
            packets[i] <- packets[i+1]
            packets[i+1] <- tmp
            shuffle <- TRUE
        }
    }
}

prod(which(packets %in% dividers))

