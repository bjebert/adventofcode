rm(list=ls())
source("utilities.R")
inp <- get_input("2022/19", parse = F, deesblake = F, cache = T)



ore_ore <- as.numeric(sapply(strsplit(inp, " "), function(x) x[7]))
clay_ore <- as.numeric(sapply(strsplit(inp, " "), function(x) x[13]))

obsidian_ore <- as.numeric(sapply(strsplit(inp, " "), function(x) x[19]))
obsidian_clay <- as.numeric(sapply(strsplit(inp, " "), function(x) x[22]))

geode_ore <- as.numeric(sapply(strsplit(inp, " "), function(x) x[28]))
geode_obsidian <- as.numeric(sapply(strsplit(inp, " "), function(x) x[31]))


# EXAMPLE
# ore_ore <- c(4, 2)
# clay_ore <- c(2, 3)
# obsidian_ore <- c(3, 3)
# obsidian_clay <- c(14, 8)
# geode_ore <- c(2, 3)
# geode_obsidian <- c(7, 12)

N <- length(ore_ore)

blueprints <- lapply(1:N, function(x) {
    list(ore_ore = ore_ore[x],
         clay_ore = clay_ore[x],
         obsidian_ore = obsidian_ore[x],
         obsidian_clay = obsidian_clay[x],
         geode_ore = geode_ore[x],
         geode_obsidian = geode_obsidian[x])
})


qualities <- c()
for(j in 1:N) {
    # Blueprint i - search each minute all possible combinations of robots
    
    states <- data.table(ore = 0, clay = 0, obs = 0, geode = 0, oreR = 1, clayR = 0, obsR = 0, geodeR = 0)
    
    for(minute in 1:23) {
        print(minute)
        
        # Collect resources -------------------------------------------------------
        
        states[, ore := ore + oreR]
        states[, clay := clay + clayR]
        states[, obs := obs + obsR]
        states[, geode := geode + geodeR]
        
        # Build potential robots --------------------------------------------------
        
        s1 <- states[(ore - oreR) >= blueprints[[j]][["ore_ore"]]][, .(ore = ore - blueprints[[j]][["ore_ore"]], clay, obs, geode, 
                                                             oreR = oreR + 1, clayR, obsR, geodeR)]
        
        s2 <- states[(ore - oreR) >= blueprints[[j]][["clay_ore"]]][, .(ore = ore - blueprints[[j]][["clay_ore"]], clay, obs, geode, 
                                                              oreR, clayR = clayR + 1, obsR, geodeR)]  
        
        s3 <- states[(ore - oreR) >= blueprints[[j]][["obsidian_ore"]] & (clay - clayR) >= blueprints[[j]][["obsidian_clay"]]][, .(ore = ore - blueprints[[j]][["obsidian_ore"]], 
                                                                                                              clay = clay - blueprints[[j]][["obsidian_clay"]], obs, geode, 
                                                                                                              oreR, clayR, obsR = obsR + 1, geodeR)]
        
        s4 <- states[(ore - oreR) >= blueprints[[j]][["geode_ore"]] & (obs - obsR) >= blueprints[[j]][["geode_obsidian"]]][, .(ore = ore - blueprints[[j]][["geode_ore"]], 
                                                                                                           clay, obs = obs - blueprints[[j]][["geode_obsidian"]], geode, 
                                                                                                           oreR, clayR, obsR, geodeR = geodeR + 1)]
        
        # If last 2 minutes, only build a geode robot
        if(minute >= 22 && nrow(s4) > 0) {
            potentials <- copy(s4)
        } else {
            potentials <- rbindlist(list(s1, s2, s3, s4))
        }
        
        if(minute <= 17) {
            # Filter out dominated potentials
            states <- states[order(-geode, -obs, -clay, -ore)]
            tops <- states[, .SD[1], by = .(oreR, clayR, obsR, geodeR)]
            bad_idx <- unlist(sapply(1:nrow(tops), function(i) {
                potentials[oreR == tops[["oreR"]][i] & clayR == tops[["clayR"]][i] & obsR == tops[["obsR"]][i] & geodeR == tops[["geodeR"]][i] &
                               ore <= tops[["ore"]][i] & clay <= tops[["clay"]][i] & obs <= tops[["obs"]][i] & geode <= tops[["geode"]][i], which = T]
            }))
            
            if(length(bad_idx) > 0) {
                pot_filt <- potentials[-bad_idx]
            } else {
                pot_filt <- potentials
            }
            
            states <- rbind(states, pot_filt)
        } else {
            states <- rbind(states, potentials)
        }
        
        states <- unique(states)
    }
    
    max_geodes <- states[, max(geode + geodeR)]
    quality <- max_geodes * j
    
    qualities <- c(qualities, quality)
    print(sprintf("%s|%s", j, quality))
}

sum(qualities)

# Part 2

blueprints <- blueprints[1:3]
for(j in 1:N) {
    # Blueprint i - search each minute all possible combinations of robots
    
    states <- data.table(ore = 0, clay = 0, obs = 0, geode = 0, oreR = 1, clayR = 0, obsR = 0, geodeR = 0)
    
    for(minute in 1:31) {
        print(minute)
        
        # Collect resources -------------------------------------------------------
        
        states[, ore := ore + oreR]
        states[, clay := clay + clayR]
        states[, obs := obs + obsR]
        states[, geode := geode + geodeR]
        
        # Build potential robots --------------------------------------------------
        
        s1 <- states[(ore - oreR) >= blueprints[[j]][["ore_ore"]]][, .(ore = ore - blueprints[[j]][["ore_ore"]], clay, obs, geode, 
                                                                       oreR = oreR + 1, clayR, obsR, geodeR)]
        
        s2 <- states[(ore - oreR) >= blueprints[[j]][["clay_ore"]]][, .(ore = ore - blueprints[[j]][["clay_ore"]], clay, obs, geode, 
                                                                        oreR, clayR = clayR + 1, obsR, geodeR)]  
        
        s3 <- states[(ore - oreR) >= blueprints[[j]][["obsidian_ore"]] & (clay - clayR) >= blueprints[[j]][["obsidian_clay"]]][, .(ore = ore - blueprints[[j]][["obsidian_ore"]], 
                                                                                                                                   clay = clay - blueprints[[j]][["obsidian_clay"]], obs, geode, 
                                                                                                                                   oreR, clayR, obsR = obsR + 1, geodeR)]
        
        s4 <- states[(ore - oreR) >= blueprints[[j]][["geode_ore"]] & (obs - obsR) >= blueprints[[j]][["geode_obsidian"]]][, .(ore = ore - blueprints[[j]][["geode_ore"]], 
                                                                                                                               clay, obs = obs - blueprints[[j]][["geode_obsidian"]], geode, 
                                                                                                                               oreR, clayR, obsR, geodeR = geodeR + 1)]
        
        # If last 2 minutes, only build a geode robot
        if(minute >= 30 && nrow(s4) > 0) {
            potentials <- copy(s4)
        } else {
            potentials <- rbindlist(list(s1, s2, s3, s4))
        }
        
        if(minute <= 19) {
            # Filter out dominated potentials
            states <- states[order(-geode, -obs, -clay, -ore)]
            tops <- states[, .SD[1], by = .(oreR, clayR, obsR, geodeR)]
            bad_idx <- unlist(sapply(1:nrow(tops), function(i) {
                potentials[oreR == tops[["oreR"]][i] & clayR == tops[["clayR"]][i] & obsR == tops[["obsR"]][i] & geodeR == tops[["geodeR"]][i] &
                               ore <= tops[["ore"]][i] & clay <= tops[["clay"]][i] & obs <= tops[["obs"]][i] & geode <= tops[["geode"]][i], which = T]
            }))
            
            if(length(bad_idx) > 0) {
                pot_filt <- potentials[-bad_idx]
            } else {
                pot_filt <- potentials
            }
            
            states <- rbind(states, pot_filt)
        } else {
            states <- rbind(states, potentials)
        }
        
        x <- as.numeric(Zseq::Triangular(50))
        
        rem <- 32 - minute
        
        # Assume we built a geode robot every minute until the end - is that enough to catch the current top scorer?
        states[, max_geode_robots := rem - 1]
        states[max_geode_robots > 0, max_extra_geodes := x[max_geode_robots + 1]]
        states[max_geode_robots == 0, max_extra_geodes := 0]
        
        states[, geode_proj := geode + geodeR * rem]
        states[, geode_cap := geode_proj + max_extra_geodes]
        
        max_proj <- states[, max(geode_proj)]
        states <- states[geode_cap >= max_proj]
        
        states[, c("max_geode_robots", "max_extra_geodes", "geode_cap", "geode_proj") := NULL]
        states <- unique(states)
    }
    
    max_geodes <- states[, max(geode + geodeR)]
    print(sprintf("%s|%s", j, max_geodes))
}
