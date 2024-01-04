rm(list=ls())
source("utilities.R")
inp <- get_input("2023/07", parse = F, deesblake = F, cache = T)

inp <- strsplit("32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483", "\n")[[1]]

hands <- sapply(strsplit(inp, " "), function(x) x[1])
bids <- sapply(strsplit(inp, " "), function(x) x[2])

rank <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

# strength <- function(hand) {
#     h <- strsplit(hand, "")[[1]]
#     
#     if(length(unique(h)) == 5) {
#         return(0)  # high card
#     } else if(length(unique(h)) == 4) {
#         return(1)  # one pair
#     } 
#     
#     s <- rev(sort(table(h)))
#     
#     if(s[1] == 2 && s[2] == 2) {
#         return(2)  # 2 pair
#     }
#     
#     if(s[1] == 3 && s[2] != 2) {
#         return(3)  # three of a kind
#     } else if(s[1] == 3 && s[2] == 2) {
#         return(4)  # full house
#     } else if(s[1] == 4) {
#         return(5)  # 4 of a kind
#     } else if (s[1] == 5) {
#         return(6)  # 5 of a kind
#     }
# }

strength2 <- function(hand) {
    h <- strsplit(hand, "")[[1]]
    
    jokers <- sum(h == "J")
    h <- h[h != "J"]
    
    s <- rev(sort(table(h)))
    
    if(length(s) == 0 || length(s) == 1) {
        return(6)  # 5 of a kind
    }
    
    if(s[1] == 1) {
        if(jokers == 0) {
            return(0)  # high card
        } else if(jokers == 1) {
            return(1)  # one pair
        } else if(jokers == 2) {
            return(3)
        } else if(jokers == 3) {
            return(5)
        }
        
    }
    
    if(s[1] == 2 && s[2] == 1) {
        if(jokers == 0) {
            return(1)  # one pair
        } else if(jokers == 1) {
            return(3)  # 3 of a kind
        } else if(jokers == 2) {
            return(5)
        } else if(jokers >= 3) {
            return(6)
        }
    } 
    
    if(s[1] == 2 && s[2] == 2 && jokers == 0) {
        return(2)  # 2 pair
    } else if(s[1] == 2 && s[2] == 2 && jokers == 1) {
        return(4)  # full house
    }
    
    if(s[1] == 3 && s[2] == 1) {
        if(jokers == 0) {
            return(3)  # three of a kind
        } else if(jokers == 1) {
            return(5)  # 4 of a kind
        }
    } 
    
    if(s[1] == 3 && s[2] == 2) {
        return(4)  # full house
    } 
    
     if(s[1] == 4) {
        if(jokers == 0) {
            return(5)  # 4 of a kind
        } else {
            return(6)
        }
    }
    
    if (s[1] == 5) {
        return(6)  # 5 of a kind
    }
}

for(hand in hands) {
    strength2(hand)
}

str <- sort(sapply(hands, strength2))

acode <- function(x) {
    paste(letters[as.numeric(paste(sapply(strsplit(x, "")[[1]], function(y) which(rank == y)), sep = ""))], collapse = "")
}

codes <- sapply(names(str), acode)
fin <- rev(names(str[order(-str, codes)]))

fin


sum(as.numeric(bids) * sapply(hands, function(x) which(x == fin)))
