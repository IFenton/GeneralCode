## function to determine names of BFD based on lookup tables provided by Tracey Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare <- function (forams.sample, micro = FALSE) {
  # load lookup tables
  forams.tracey <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/traceystax.csv")
  aze.species <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/Traceysspecies.csv")
  forams.lookup <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/foramslookup.csv")
  lookup.species <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/lookupwodup.csv")
  forams.lookup.micro <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/foramslookup_micro.csv")
  lookup.species.micro <- read.csv("C:/Users/isabf/Dropbox/Documents/PhD/Project/Foraminifera/Data/lookupwodup_micro.csv")
  
  # create the matching function
  unitestfun <- function (sample, forams, lookup) {
    if (is.na(match(sample, forams$Species)) == FALSE) {
      return(as.vector(forams$Full[match(sample,  forams$Species)]))
    } else {
      if (is.na(match(sample, lookup$Neptune.names)) == FALSE) {
        return(as.vector(lookup$Tracey.names[match(sample, lookup$Neptune.names)]))
      } else { 
        return("unknown")
      }
    }
  }
  
  # run that function
  forams.sample <- as.character(forams.sample) ## make sure it is not a factor
  split <- strsplit(forams.sample, " ") [[1]] [2] # take the second name
  spec <- strsplit(forams.sample, " ") [[1]] [1] # and the first name
  if (is.na(split) == FALSE) {
    # if there is only a species name
    species <- unitestfun(forams.sample, forams.tracey, forams.lookup)
  } else {
    # if the name is genus and species
    species <- unitestfun(spec, aze.species, lookup.species)    
  }
  # if we should resolve microperforate names
  if (micro & species == "Micro") {
    if (is.na(split) == FALSE) {
      # if there is only a species name
      species <- unitestfun(forams.sample, forams.tracey, forams.lookup.micro)
    } else {
      # if the name is genus and species
      species <- unitestfun(spec, aze.species, lookup.species.micro)    
    }
  } 
  return(species)
}

## example
# sample.names <- sapply(as.character(forams.bfd[, 1]), compare, USE.NAMES = FALSE)
