input <- readLines("19.txt")

rules <- input[1:139]
messages <- input[141:length(input)]
# 
# rules <- strsplit('0: 4 1 5
# 1: 2 3 | 3 2
# 2: 4 4 | 5 5
# 3: 4 5 | 5 4
# 4: "a"
# 5: "b"', '\n')[[1]]


# Establish rules list ----------------------------------------------------


rules_list <- lapply(1:length(rules), function(x) NA)
names(rules_list) <- sapply(strsplit(rules, ":"), function(x) x[1])

for(i in 1:length(rules)) {
    rs <- strsplit(rules[i], ": ")[[1]]
    
    if(grepl("\\|", rs[2]) == FALSE) {
        special <- strsplit(gsub('"', '', rs[2]), " ")[[1]]
        if(any(grepl("[a-z]", special)) == FALSE) {
            special <- as.numeric(special)
        }
        rules_list[[i]] <- list(special)
    } else {
        or_statements <- strsplit(rs[2], " \\| ")[[1]]
        rules_list[[i]] <- lapply(strsplit(or_statements, " "), as.numeric)
    }
}


# Begin resolving list ----------------------------------------------------

while(any(sapply(rules_list, function(x) any(sapply(x, function(y) is.numeric(y)))))) {
    
    resolved <- names(rules_list)[sapply(rules_list, function(x) is.character(x[[1]]))]
    can_resolve <- names(rules_list)[sapply(rules_list, function(x) all(unlist(x) %in% resolved))]
    
    print(sprintf("%d / %d", length(resolved), length(rules_list)))
    
    for(rule in can_resolve) {
        
        all_combos <- c()
        for(potentials in rules_list[[rule]]) {
            expansion <- expand.grid(lapply(lapply(potentials, function(x) rules_list[[as.character(x)]]), unlist), stringsAsFactors = FALSE)
            combos <- sapply(1:nrow(expansion), function(x) paste(expansion[x,], collapse = ""))
            
            all_combos <- c(all_combos, combos)            
        }
        
        rules_list[[rule]] <- as.list(all_combos)
    }    
}

combos_42 <- unlist(rules_list[["42"]])
combos_31 <- unlist(rules_list[["31"]])

message_passes <- function(message) {
    match_42 <- sapply(seq(1, nchar(message), 8), function(x) substr(message, x, x+7)) %in% combos_42
    match_31 <- sapply(seq(1, nchar(message), 8), function(x) substr(message, x, x+7)) %in% combos_31
    
    msg_str <- paste(sapply(1:length(match_42), function(x) ifelse(match_42[x] == TRUE, "x", ifelse(match_31[x] == TRUE, "y", "z"))), collapse = "")
    
    return(grepl("^^[x]+(xy|xxyy|xxxyyy|xxxxyyyy|xxxxxyyyyy|xxxxxxyyyyyy)$", msg_str))
}

sum(sapply(messages, message_passes))
