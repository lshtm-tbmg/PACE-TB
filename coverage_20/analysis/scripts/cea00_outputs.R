#------------------------------------------------------------------------------#
# PACE-TB - CEA outputs                                                        #
# Extract outputs for cost-effectiveness analysis                              #
# Code by Lara Goscé (lara.gosce@lshtm.ac.uk)                                  #
# Last updated 2025-03-01 by ASC                                               #
#------------------------------------------------------------------------------#

# List available files
files <- list.files(here("outputs", "cea"), full.names = FALSE)
files <- files[grepl(iso, files, fixed = TRUE)]

# Extract intervention types
intvs <- unique(sapply(files, function(f) strsplit(f, "_")[[1]][2]))
df <- list() # Single-run data

# Loop through each intervention type
for (intv in 1:length(intvs)) {
  intv <- intvs[intv]
  runs <- grep(intv, files, value = TRUE)
  
  # Loops through each model run
  for (j in 1:length(runs)) {
    file <- runs[j]
    uid <- sub("\\.RData", "", strsplit(file, "_")[[1]][4])
    
    # Loads objects in file and creates list
    load(here("outputs", "cea", paste0(file)))
    loaded <- ls()
    loaded <- loaded[sapply(loaded, function(v) exists(v) && is.data.frame(get(v)))]
    
    # Places loaded objects in list organised by UID
    df[[iso]][[uid]] <- list()
    for (var in loaded) {
      df[[iso]][[uid]][[var]] <- get(var)
    }
  }
  
  for (uid in names(df[[iso]])) {
    run <- match(uid, names(df[[iso]]))
    
    for (var in names(df[[iso]][[uid]])) {
      
      # Creates grouping data frames dynamically
      if (!is.data.frame(data[[iso]][[intv]][[var]])) {
        nrows <- nrow(df[[iso]][[uid]][[var]])
        data[[iso]][[intv]][[var]] <- data.frame(matrix(NA_real_, nrow = nrows, ncol = length(df[[iso]])))
        colnames(data[[iso]][[intv]][[var]]) <- paste0("run", seq_len(length(runs)))
      }
      
      # Adds data to combined-run dataset
      data[[iso]][[intv]][[var]][, run] <- df[[iso]][[uid]][[var]][, "val"]
    }
  }
}

rm(list = setdiff(ls(), c("data", "isos")))
