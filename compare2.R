## function to determine names of BFD based on lookup tables provided by Tracy Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare <- function (forams.sample, micro = FALSE) {
  # load lookup tables
  library("openxlsx")
  forams.tracy <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "tracystax")
  aze.species <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "Tracysspecies")
  forams.lookup <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "foramslookup")
  lookup.species <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "lookupwodup")
  forams.lookup.micro <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "foramslookup_micro")
  lookup.species.micro <- read.xlsx("C:/Users/eart0475/Documents/IF Dropbox/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx", sheet = "lookupwodup_micro")
  
  # create the matching function
  unitestfun <- function (sample, forams, lookup) {
    if (is.na(match(sample, forams$Species)) == FALSE) {
      return(as.vector(forams$Full[match(sample,  forams$Species)]))
    } else {
      if (is.na(match(sample, lookup$Neptune.names)) == FALSE) {
        return(as.vector(lookup$Tracy.names[match(sample, lookup$Neptune.names)]))
      } else { 
        return("unknown")
      }
    }
  }
  
  # create the compare function
  comp.func <- function(forams.sample, micro) {
    forams.sample <- as.character(forams.sample) ## make sure it is not a factor
    split <- strsplit(forams.sample, " ") [[1]] [2] # take the second name
    spec <- strsplit(forams.sample, " ") [[1]] [1] # and the first name
    if (is.na(split) == FALSE) {
      # if there is only a species name
      species <- unitestfun(forams.sample, forams.tracy, forams.lookup)
    } else {
      # if the name is genus and species
      species <- unitestfun(spec, aze.species, lookup.species)    
    }
    # if we should resolve microperforate names
    if (micro & species == "Micro") {
      if (is.na(split) == FALSE) {
        # if there is only a species name
        species <- unitestfun(forams.sample, forams.tracy, forams.lookup.micro)
      } else {
        # if the name is genus and species
        species <- unitestfun(spec, aze.species, lookup.species.micro)    
      }
    }
    return(species)
  }
  species.list <- sapply(forams.sample, comp.func, USE.NAMES = FALSE, micro = micro) 
  return(species.list)
}

## example
# sample.names <- sapply(as.character(forams.bfd[, 1]), compare, USE.NAMES = FALSE)
