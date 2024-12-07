
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2016/09", parse = F, user = "blakeebets", cache = T)

inp <- "X(8x2)(3x3)ABCY"
out <- c()

ss <- strsplit(inp, "")[[1]]
i <- 1

while(i <= length(ss)) {
    if(ss[i] == "(") {
        w <- which(ss == ")")
        end_i <- w[w > i][1]
        
        dim <- as.numeric(strsplit(paste(ss[(i+1):(end_i-1)], collapse = ""), "x")[[1]])
        out <- c(out, rep(paste(ss[(end_i+1):(end_i+dim[1])], collapse = ""), dim[2]))
        
        i <- end_i + dim[1] + 1
    } else {
        out <- c(out, ss[i])
        i <- i + 1
    }
}

nchar(paste(out, collapse = ""))

# 7:10 (10th)


# Part 2 ------------------------------------------------------------------

ss <- strsplit(inp, "")[[1]]
reps <- setNames(rep(1, length(ss)), ss)

w_open <- which(ss == "(")
w_close <- which(ss == ")")

for(i in 1:length(w_open)) {
    dim <- as.numeric(strsplit(paste(ss[(w_open[i]+1):(w_close[i]-1)], collapse = ""), "x")[[1]])
    nc <- dim[1]
    amt <- dim[2]
    
    reps[(w_close[i]+1):(w_close[i]+nc)] <- reps[(w_close[i]+1):(w_close[i]+nc)] * amt
}

I <- 1:length(ss)
sum(reps[setdiff(I, unlist(sapply(1:length(w_open), function(i) w_open[i]:w_close[i])))])

# 15:24 (20th)