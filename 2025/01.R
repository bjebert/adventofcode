
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("L68
L30
R48
L5
R60
L55
L1
L99
R14
L82", "\n")[[1]]  # Example

inp <- get_input("2025/01", parse = F, user = "bjebert", cache = F)


dial <- 50
cnt <- 0
for(rot in inp) {
    dir <- substr(rot, 1, 1)
    amt <- as.numeric(substr(rot, 2, nchar(rot)))
    
    dial <- if(dir == "L") dial - amt else dial + amt
    
    while(dial < 0) {
        dial <- dial + 100
    }
    while(dial >= 100) {
        dial <- dial - 100
    }
    
    if(dial == 0) {
        cnt <- cnt + 1
    }
    
    print(dial)
}

print(cnt)


# part 2 ------------------------------------------------------------------

dial <- 50
cnt <- 0
rot <- inp[1]
for(rot in inp) {
    dir <- substr(rot, 1, 1)
    amt <- as.numeric(substr(rot, 2, nchar(rot)))    
    
    step <- if(dir == "L") -1 else 1
    
    for(i in 1:amt) {
        dial <- dial + step
        
        if(dial == -1) {
            dial <- 99
        } else if(dial == 100) {
            dial <- 0
        }
        
        if(dial == 0) {
            cnt <- cnt + 1
        }
    }
}

print(cnt)
