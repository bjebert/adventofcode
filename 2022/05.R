rm(list=ls())
source("utilities.R")
inp <- get_input("2022/5", parse = F, deesblake = F, cache = F)

# [C]         [N] [R]    
# [J] [T]     [H]         [P] [L]    
# [F] [S] [T] [B]         [M] [D]    
# [C] [L] [J] [Z] [S]     [L] [B]    
# [N] [Q] [G] [J] [J]     [F] [F] [R]
# [D] [V] [B] [L] [B] [Q] [D] [M] [T]
# [B] [Z] [Z] [T] [V] [S] [V] [S] [D]
# [W] [P] [P] [D] [G] [P] [B] [P] [V]

crates <- c("JFCNDBW", "TSLQVZP", "TJGBZP", "CHBZJLTD", "SJBVG", "QSP", "NPMLFDVB", "RLDBFMSP", "RTDV")
instr <- inp[11:length(inp)]

crates <- c("NZ", "DCM", "P")

crates <- strsplit(crates, "")



instr <- strsplit("move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2", "\n")[[1]]

i <- 1
for(i in 1:length(instr)) {
    is <- strsplit(instr[i], " ")[[1]]
    
    amt <- as.numeric(is[2])
    from <- as.numeric(is[4])
    to <- as.numeric(is[6])
    
    crates[[to]] <- c(crates[[from]][1:amt], crates[[to]])
    
    if(amt >= length(crates[[from]])) {
        crates[[from]] <- character(0)
    } else {
        crates[[from]] <- crates[[from]][(amt+1):length(crates[[from]])]
    }
}

paste(sapply(crates, function(x) x[1]), collapse = "")
