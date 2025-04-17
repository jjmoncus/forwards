library(tidyverse)
library(here)
library(glue)

folder <- here("Downloads")
file <- list.files(folder) %>% str_subset("data") %>% str_subset("rds")
data <- readRDS(glue("{folder}/{file}"))


library(ellmer)
find_unique_id_var <- function(data) {
  
  # subset to variables that mathematically could be it
  candidates <- keep(data,
       function(x) {
         
         length(unique(x)) == length(x)
       }) %>%
    names()
  
  
  # if just one candidate, return
  if (length(candidates) == 1) {
    return(candidates)
  } else {
    
    # if more than one candidate, ask LLM which of the candidates is good
    chat <- chat_github(
      system_prompt = glue("The user will pass you the description of a variable, \\
                         and you evaluate whether or not the variable is meant to \\
                         represent a unique identifer. If yes, return TRUE, and if \\
                         not, return FALSE")
    )
    
    trial <- meta(data) %>% filter(var %in% candidates)
    keep(trial$label,
        ~chat$chat(.x, echo = FALSE) %>%
          as.logical()
        ) %>%
      names()
  }
}

find_unique_id_var(data)

find_batteries <- function(data) {
  
  # the old heuristic was to assume batteries all shared a prefix
  # we can no longer rely on that
  # 
  # I think we now need to identify groups of vars that share the same levels maybe
  # or just pass variable name, label, and levels to it, and have it guess?
  
  chat <- chat_github(
    system_prompt = glue("The user will pass you a sequence of variable names, \\
    their labels, and the value levels that the variable can take, and I want you \\
    to identify which groups of variables ostensibly belong to the same battery. \\
    
                         
                         Generate the data in a JSON format. Each element should just contain \\
                         the names of variables which belong to each battery. Do not include any \\
                         variable labels or value levels in the output.
                         
                         The most important characteristic for deciding whether variables \\
                         belong in a battery together is if they share the same value levels. \\
                         Two variables cannot be in the same battery if they do not have \\
                         the same value levels. Next most important is if the variables \\
                         have similar names.
                         
                         Not every variable will correspond to a battery. If you find that a variable \\
                         doesn't belong in any battery, exclude it from the output.
                         
                         Do not include any text formatting in the data (e.g., no backticks).
                         
                         Do not include any other information with the dataset.")
  )
  
  
  single_var_info <- function(var, label, levels) {
    
    if (is.null(label)) label <- "MISSING LABEL"
    if (is.null(levels)) levels <- "MISSING LEVELS"
    levels_statement <- str_c(levels, collapse = ", ")
    
    
    glue("Variable Name: {var}
         Variable Label: {label}
         The variable's value levels: {levels_statement}
         
         ",
         trim = FALSE)
  }
  
  out <- meta(data) %>% filter(class == "factor")
  
  submission <- glue("{pmap(.l = list(a = out$var, b = out$label, c = out$levels),
       .f = function(a, b, c) single_var_info(var = a, label = b, levels = c))}")
  
  
  response <- chat$chat(submission, echo = FALSE)
  parse_json(response)
}


find_batteries(data[210:230])
