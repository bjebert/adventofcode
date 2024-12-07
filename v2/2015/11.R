
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/11", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

lmap <- setNames(1:26, letters)   
nmap <- setNames(letters, 1:26) 

has_str8 <- function(nums) {
    for(i in 1:(length(nums) - 2)) {
        if(nums[i] == (nums[i+1] - 1) && nums[i] == (nums[i+2] - 2)) {
            return(T)
        }
    }
    return(F)
}

meets <- function(pass) {
    if(grepl("i|o|l", pass)) {
        return(F)
    }
    
    x <- strsplit(pass, "")[[1]]
    
    nums <- lmap[x]
    if(!has_str8(nums)) {
        return(F)
    }
    
    w <- which(x == shift(x))
    
    if(sum(w - shift(w, fill = 0) >= 2) < 2) {
        return(F)
    }
    
    return(T)
}


inc_pass <- function(pass) {
    nums <- lmap[strsplit(pass, "")[[1]]]
    new <- copy(nums)
    i <- max(which(nums != 26))
    
    new[i] <- nums[i] + 1
    
    if(i < length(nums)) {
        new[(i+1):(length(new))] <- 1
    }
    
    return(paste(nmap[as.character(new)], collapse = ""))
}

pass <- inp
while(TRUE) {
    if(!meets(pass)) {
        pass <- inc_pass(pass)
    } else {
        print(pass)
        break
    }
}

# 12:18 (32nd)

# 13:04 (27th)
