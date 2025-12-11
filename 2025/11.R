
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/11", parse = F, user = "bjebert", cache = F)
inx <- strsplit("svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out", "\n")[[1]]

# Part 1 ------------------------------------------------------------------

inp <- inx
inp <- inr

device <- sapply(strsplit(inp, ": "), function(x) x[1])
outputs <- strsplit(sapply(strsplit(inp, ": "), function(x) x[2]), " ")
devices <- setNames(outputs, device)

Q <- list(list(pos = "svr", path = NULL))
cnt <- 0

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    if(curr[["pos"]] == "out") {
        if(all(c("dac", "fft") %in% curr[["path"]])) {
            cnt <- cnt + 1
        }
        next
    }
    
    nb <- devices[[curr[["pos"]]]]
    for(n in nb) {
        Q[[length(Q) + 1]] <- list(pos = n, path = c(curr[["path"]], n))
    }
    
    print(length(Q))
}


# Part 2 ------------------------------------------------------------------

inx <- strsplit("svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out", "\n")[[1]]

inp <- inx
inp <- inr

devices <- setNames(strsplit(sapply(strsplit(inp, ": "), function(x) x[2]), " "), 
                    sapply(strsplit(inp, ": "), function(x) x[1]))

ways <- memoise(function(pos, end) {
    if(pos == end) return(1)
    if(pos == "out") return(0)
    Reduce(`+`, sapply(devices[[pos]], function(x) ways(x, end)))
})

ways("you", "out")
ways("svr", "fft") * ways("fft", "dac") * ways("dac", "out")
