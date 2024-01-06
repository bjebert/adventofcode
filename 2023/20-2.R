inp <- readLines("2023/20.txt")

module_names <- sapply(strsplit(inp, " -> "), function(x) x[1])
module_targets <- strsplit(sapply(strsplit(inp, "-> "), function(x) x[2]), ", ")

modules <- setNames(module_targets, module_names)
partial_names <- substr(module_names, 2, nchar(module_names))

states <- setNames(lapply(1:length(modules), function(i) {
    if(grepl("%", names(modules)[i])) {
        return("off") 
    } else if(grepl("&", names(modules)[i])) {
        # Find inputs
        module_name <- substr(names(modules)[i], 2, nchar(names(modules)[i]))
        input_modules <- gsub("%|&", "", names(modules)[sapply(modules, function(x) module_name %in% x)])
        
        return(setNames(rep("low", length(input_modules)), input_modules))
    } else {
        return(NULL)
    }
}), names(modules))


# Part 2 ------------------------------------------------------------------


push <- function(states) {
    pulses <- lapply(1:length(modules[["broadcaster"]]), function(i) {
        list(sender = "broadcaster",
             target = modules[["broadcaster"]][[i]],
             pulse = "low")
    })
    
    while(length(pulses) > 0) {
        future <- list()

        # First: update state based on pulse --------------------------------------

        for(pulse in pulses) {
            for(target in pulse[["target"]]) {
                if(target %in% partial_names) {
                    target_module <- modules[which(partial_names == target)]
                    is_flipflop <- grepl("%", names(target_module))
                    is_conjunction <- grepl("&", names(target_module))
                    
                    if(is_flipflop) {
                        if(pulse[["pulse"]] == "low") {
                            if(states[[names(target_module)]] == "off") {
                                states[[names(target_module)]] <- "on"
                            } else {
                                states[[names(target_module)]] <- "off"
                            }
                        }
                    } else if(is_conjunction) {
                        states[[names(target_module)]][[pulse[["sender"]]]] <- pulse[["pulse"]]
                    }
                } else {
                    states[[target]] <- pulse[["pulse"]]
                }
            }
        }

        # Second: send out new pulses from each recipient -------------------------
        
        for(pulse in pulses) {
            for(target in pulse[["target"]]) {
                outgoing <- NULL
                
                if(target %in% partial_names) {
                    target_module <- modules[which(partial_names == target)]
                    is_flipflop <- grepl("%", names(target_module))
                    is_conjunction <- grepl("&", names(target_module))
                    
                    if(is_flipflop) {
                        if(pulse[["pulse"]] == "low") {
                            if(states[[names(target_module)]] == "off") {
                                outgoing <- lapply(1:length(target_module), function(i) {
                                    list(sender = target,
                                         target = target_module[[i]],
                                         pulse = "low")
                                })
                            } else {
                                outgoing <- lapply(1:length(target_module), function(i) {
                                    list(sender = target,
                                         target = target_module[[i]],
                                         pulse = "high")
                                })
                            }
                        }
                    } else if(is_conjunction) {
                        if(all(states[[names(target_module)]] == "high")) {
                            outgoing <- lapply(1:length(target_module), function(i) {
                                list(sender = target,
                                     target = target_module[[i]],
                                     pulse = "low")
                            })
                            
                        } else {
                            outgoing <- lapply(1:length(target_module), function(i) {
                                list(sender = target,
                                     target = target_module[[i]],
                                     pulse = "high")
                            })
                        }
                    }
                }
                
                future <- c(future, outgoing)
            }
        }
        
        pulses <- copy(future)
    }
    
    return(states)
}


i <- 1
tq <- data.table()
pf <- data.table()
kx <- data.table()
rj <- data.table()

while(i < 10000) {
    tq <- rbind(tq, as.data.table(t(states[["&tq"]])))
    pf <- rbind(pf, as.data.table(t(states[["&pf"]])))
    kx <- rbind(kx, as.data.table(t(states[["&kx"]])))
    rj <- rbind(rj, as.data.table(t(states[["&rj"]])))
    
    states <- push(states)
    i <- i + 1
}

which(rowSums(tq == "low") == ncol(tq))
which(rowSums(pf == "low") == ncol(pf))
which(rowSums(kx == "low") == ncol(kx))
which(rowSums(rj == "low") == ncol(rj))

# cycle lengths at which a low signal is sent out:
# tq: 3767
# pf: 3779 
# kx: 4057
# rj: 3889

3767*3779*4057*3889
options(scipen=99)
