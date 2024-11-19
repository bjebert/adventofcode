
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2015/22", parse = F, user = "blakeebets", cache = T)



# BFS

boss_hp <- 58
boss_dmg <- 9

hp <- 50
mana <- 500

cost <- c("missile" = 53, "drain" = 73, "shield" = 113, "poison" = 173, "recharge" = 229)

Q <- list(list(hp = hp, mana = mana, armor = 0, spent = 0, boss_hp = boss_hp, boss_dmg = boss_dmg, 
               effects = c("shield" = 0, "poison" = 0, "recharge" = 0)))

min_mana <- Inf

while(length(Q) > 0) {
    state <- Q[[1]]
    Q <- Q[-1]
    
    if(state[["boss_hp"]] < 0) {
        if(state[["spent"]] < min_mana) {
            min_mana <- state[["spent"]]    
            print(min_mana)
        }
        next
    } else if(state[["hp"]] <= 1) {  # Part 2: drain
        next
    } else if(state[["spent"]] > min_mana) {
        next
    }
    
    effects <- state[["effects"]]
    state[["hp"]] <- state[["hp"]] - 1  # Part 2: drain
    
    # Player turn
    
    # Apply effects to us
    if(effects[["poison"]] > 0) {
        state[["boss_hp"]] <- state[["boss_hp"]] - 3
    }
    
    if(effects[["recharge"]] > 0) {
        state[["mana"]] <- state[["mana"]] + 101
    }
    
    effects <- setNames(pmax(0, effects - 1), names(effects))
    state[["effects"]] <- copy(effects)
    
    # Try casting each spells
    new_states <- list()
    
    for(spell in names(cost)) {
        if(state[["mana"]] >= cost[[spell]]) {
            if(!(spell %in% names(effects)) || effects[[spell]] == 0) {
                if(spell == "missile") {
                    new_state <- copy(state)
                    
                    new_state[["boss_hp"]] <- new_state[["boss_hp"]] - 4
                    new_state[["mana"]] <- new_state[["mana"]] - cost[[spell]]
                    new_state[["spent"]] <- new_state[["spent"]] + cost[[spell]]
                    
                    new_states[[length(new_states) + 1]] <- new_state
                } else if(spell == "drain") {
                    new_state <- copy(state)
                    
                    new_state[["hp"]] <- new_state[["hp"]] + 2
                    new_state[["boss_hp"]] <- new_state[["boss_hp"]] - 2
                    new_state[["mana"]] <- new_state[["mana"]] - cost[[spell]]
                    new_state[["spent"]] <- new_state[["spent"]] + cost[[spell]]
                    
                    new_states[[length(new_states) + 1]] <- new_state
                } else if(spell == "shield") {
                    new_state <- copy(state)
                    
                    new_state[["mana"]] <- new_state[["mana"]] - cost[[spell]]
                    new_state[["spent"]] <- new_state[["spent"]] + cost[[spell]]
                    new_state[["effects"]][["shield"]] <- 6
                    
                    new_states[[length(new_states) + 1]] <- new_state
                } else if(spell == "poison") {
                    new_state <- copy(state)
                    
                    new_state[["mana"]] <- new_state[["mana"]] - cost[[spell]]
                    new_state[["spent"]] <- new_state[["spent"]] + cost[[spell]]
                    new_state[["effects"]][["poison"]] <- 6
                    
                    new_states[[length(new_states) + 1]] <- new_state
                } else if(spell == "recharge") {
                    new_state <- copy(state)
                    
                    new_state[["mana"]] <- new_state[["mana"]] - cost[[spell]]
                    new_state[["spent"]] <- new_state[["spent"]] + cost[[spell]]
                    new_state[["effects"]][["recharge"]] <- 5
                    
                    new_states[[length(new_states) + 1]] <- new_state
                }
            }
        }
    }
    
    # Boss turn
    for(ns in new_states) {
        effects <- ns[["effects"]]
        
        # Apply effects to boss
        if(effects[["poison"]] > 0) {
            ns[["boss_hp"]] <- ns[["boss_hp"]] - 3
        }
        
        if(effects[["recharge"]] > 0) {
            ns[["mana"]] <- ns[["mana"]] + 101
        }
        
        if(effects[["shield"]] > 0) {
            ns[["armor"]] <- 7
        } else {
            ns[["armor"]] <- 0
        }
        
        ns[["effects"]] <- setNames(pmax(ns[["effects"]] - 1, 0), names(ns[["effects"]]))
        
        effects <- pmax(0, effects - 1)
        
        # Deal damage
        ns[["hp"]] <- ns[["hp"]] - pmax(1, ns[["boss_dmg"]] - ns[["armor"]])
        
        Q <- c(Q, list(ns))
    }
    
    Q <- Q[order(sapply(Q, function(x) x[["hp"]] - x[["boss_hp"]]))]
}

# 25:08 (3rd)

# 27:12 (4th)

