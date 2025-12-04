
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/24", parse = F, user = "bjebert", cache = F)
inp

# inp <- strsplit("x00: 1
# x01: 0
# x02: 1
# x03: 1
# x04: 0
# y00: 1
# y01: 1
# y02: 1
# y03: 1
# y04: 1
# 
# ntg XOR fgs -> mjb
# y02 OR x01 -> tnw
# kwq OR kpj -> z05
# x00 OR x03 -> fst
# tgd XOR rvg -> z01
# vdt OR tnw -> bfw
# bfw AND frj -> z10
# ffh OR nrd -> bqk
# y00 AND y03 -> djm
# y03 OR y00 -> psh
# bqk OR frj -> z08
# tnw OR fst -> frj
# gnj AND tgd -> z11
# bfw XOR mjb -> z00
# x03 OR x00 -> vdt
# gnj AND wpb -> z02
# x04 AND y00 -> kjc
# djm OR pbm -> qhw
# nrd AND vdt -> hwm
# kjc AND fst -> rvg
# y04 OR y02 -> fgs
# y01 AND x02 -> pbm
# ntg OR kjc -> kwq
# psh XOR fgs -> tgd
# qhw XOR tgd -> z09
# pbm OR djm -> kpj
# x03 XOR y03 -> ffh
# x00 XOR y04 -> ntg
# bfw OR bqk -> z06
# nrd XOR fgs -> wpb
# frj XOR qhw -> z04
# bqk OR frj -> z07
# y03 OR x01 -> nrd
# hwm AND bqk -> z03
# tgd XOR rvg -> z12
# tnw OR pbm -> gnj", "\n")[[1]]  # Example

w <- which(inp == "")

inits <- strsplit(inp[1:(w-1)], ": ")
wires <- setNames(as.numeric(sapply(inits, function(x) x[2])), sapply(inits, function(x) x[1]))
is <- strsplit(inp[(w+1):length(inp)], " ")

wire <- sapply(is, function(x) x[5])
wire <- setNames(rep(NA, length(wire)), wire)

for(w in names(wires)) {
    wire[[w]] <- wires[[w]]
}
# 
# while(any(is.na(wire))) {
#     for(i in 1:length(is)) {
#         # can solve
#         line <- is[[i]]
#         vals <- c(wire[[line[1]]], wire[[line[3]]])
#         
#         if(any(is.na(vals))) {
#             next
#         }
#         
#         if(line[2] == "AND") {
#             res <- as.numeric(vals[1] == 1 && vals[2] == 1)
#         } else if(line[2] == "OR") {
#             res <- as.numeric(vals[1] == 1 || vals[2] == 1)
#         } else if(line[2] == "XOR") {
#             res <- as.numeric(vals[1] != vals[2])
#         }
#         
#         wire[[line[[5]]]] <- res
#     }
#     
# }
# 
# z <- wire[substr(names(wire), 1, 1) == "z"]
# z <- rev(z[order(names(z))])
# 
# as.bigz(sprintf("0b%s", paste(z, collapse = "")))


# part 2 ------------------------------------------------------------------

w <- which(inp == "")
inits <- strsplit(inp[1:(w-1)], ": ")
init_vals <- setNames(as.numeric(sapply(inits, function(x) x[2])), sapply(inits, function(x) x[1]))
instr <- strsplit(inp[(w+1):length(inp)], " ")

wires <- sapply(is, function(x) x[5])
wires <- setNames(rep(NA, length(wires)), wires)

for(wname in names(init_vals)) {
    wires[[wname]] <- init_vals[[wname]]
}


solve <- function(wires, instr) {
    solved <- sum(is.na(wires))
    while(any(is.na(wires))) {
        for(i in 1:length(instr)) {
            # can solve
            line <- instr[[i]]
            vals <- c(wires[[line[1]]], wires[[line[3]]])
            
            if(any(is.na(vals))) {
                next
            }
            
            if(line[2] == "AND") {
                res <- as.numeric(vals[1] == 1 && vals[2] == 1)
            } else if(line[2] == "OR") {
                res <- as.numeric(vals[1] == 1 || vals[2] == 1)
            } else if(line[2] == "XOR") {
                res <- as.numeric(vals[1] != vals[2])
            }
            
            wires[[line[[5]]]] <- res
        }
        if(sum(is.na(wires)) == solved) {
            return(NA)
        }
        solved <- sum(is.na(wires))
    }
    
    z_wires <- wires[substr(names(wires), 1, 1) == "z"]
    z_wires <- rev(z_wires[order(names(z_wires))])
    actual_out <- as.bigz(sprintf("0b%s", paste(z_wires, collapse = "")))
    
    # return(actual_out)
    return(z_wires)
}


desired <- function(wires) {
    x_in <- as.bigz(sprintf("0b%s", paste(rev(wires[substr(names(wires), 1, 1) == "x"]), collapse = "")))
    y_in <- as.bigz(sprintf("0b%s", paste(rev(wires[substr(names(wires), 1, 1) == "y"]), collapse = "")))
    x_in + y_in
}


for(k in 0:44) {
    new_wires <- copy(wires)
    new_wires[substr(names(new_wires), 1, 1) == "x"] <- 0
    new_wires[substr(names(new_wires), 1, 1) == "y"] <- 0
    
    if(k < 10) {
        new_wires[[sprintf("x0%s", k)]] <- 1
        new_wires[[sprintf("y0%s", k)]] <- 0
    } else {
        new_wires[[sprintf("x%s", k)]] <- 1
        new_wires[[sprintf("y%s", k)]] <- 0
    }
    
    bin_out <- solve(new_wires, instr)
    bin_des <- binrep(desired(new_wires), "35184372088832")[[1]][[1]]  # this num ensures 46-length output string
    
    bin_des <- setNames(bin_des, names(bin_out))
    
    if(!identical(bin_out, bin_des)) {
        print(sprintf("%s/%s", k, which(bin_out != bin_des)))
    }
}


# for k = 13 (y013=1)
bin_out[c("z14", "z13")]
bin_des[c("z14", "z13")]
# x13 AND y13 -> z13
# this must be swapped, as

# desired: x13=0, y13=0    --> 0,0
#          x13=1, y13=0    --> 0,1
#          x13=0, y13=1    --> 0,1
#          x13=1, y13=1    --> 1,0
# needs to be an XOR result.  whether this can be swapped directly, not sure.
# looks like it can be swapped with mks.

new_instr <- copy(instr)
new_instr[[which(outs == "z13")]][5] <- "vcv"
new_instr[[which(outs == "vcv")]][5] <- "z13"

new_instr[[which(outs == "z19")]][5] <- "vwp"
new_instr[[which(outs == "vwp")]][5] <- "z19"

new_instr[[which(outs == "z25")]][5] <- "mps"
new_instr[[which(outs == "mps")]][5] <- "z25"

new_instr[[which(outs == "cqm")]][5] <- "vjv"
new_instr[[which(outs == "vjv")]][5] <- "cqm"

as.bigz(sprintf("0b%s", paste(solve(wires, new_instr), collapse = "")))
desired(wires)

paste(sort(c("vcv", "z13", "vwp", "z19", "mps", "z25", "vjv", "cqm")), collapse = ",")
