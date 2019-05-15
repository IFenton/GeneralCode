## function to determine names of BFD based on lookup tables provided by Tracy Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare <- function (forams.sample, micro = FALSE) {
  # load lookup tables
  library("openxlsx")
  path <- gsub("/[^/]*(Box|Dropbox).*/.*$", "", getwd())
  forams.lookup <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx"), sheet = "foramslookup")
  forams.lookup <- forams.lookup[,1:3]
  
  # create a version of the lookup table which only has the species names
  species.nm <- forams.lookup
  tmp <- species.nm[!duplicated(species.nm$AcceptedName),]
  tmp$Synonym <- tmp$AcceptedName
  species.nm <- rbind(species.nm, tmp)
  # remove the first name (genus)
  species.nm$Synonym <- gsub("^[^ ]* ", "", species.nm$Synonym)
  # remove duplicates from mispelled genus 
  species.nm <- species.nm[!duplicated(species.nm[,1:2]),]
  # where there are still duplicates, set these as "Unsure"
  spp.dup <- species.nm$Synonym[duplicated(species.nm$Synonym) & species.nm$AcceptedName != "Unsure"]
  species.nm$AcceptedName[species.nm$Synonym %in% unique(spp.dup)] <- "Unsure"
  
  # create versions of the lookup table / species list with abbreviated genus names
  abb.nm <- forams.lookup
  abb.nm <- rbind(abb.nm, tmp)
  # remove the first name (genus)
  abb.nm$Synonym <- gsub("^(.).* ", "\\1\\. ", abb.nm$Synonym)
  # remove duplicates from mispelled genus 
  abb.nm <- abb.nm[!duplicated(abb.nm[,1:2]),]
  # where there are still duplicates, set these as "Unsure"
  abb.dup <- abb.nm$Synonym[duplicated(abb.nm$Synonym) & abb.nm$AcceptedName != "Unsure"]
  abb.nm$AcceptedName[abb.nm$Synonym %in% unique(abb.dup)] <- "Unsure"
  
  # merge these three dataframes
  forams.lookup <- rbind(forams.lookup, species.nm, abb.nm)
  
  # if not micro
  if (!micro) {
    forams.lookup$AcceptedName[forams.lookup$Micro == "Yes"] <- "Micro"
  }
  
  # create the compare function
  comp.func <- function(forams.sample) {
    forams.sample <- as.character(forams.sample) ## make sure it is not a factor
    species <- forams.lookup$AcceptedName[match(forams.sample, forams.lookup$AcceptedName)]
    if (is.na(species)) {
      species <- forams.lookup$AcceptedName[match(forams.sample, forams.lookup$Synonym)]
      if (is.na(species))
        species <- "unknown"
    }
    return(species)
  }
  
  species.list <- sapply(forams.sample, comp.func, USE.NAMES = FALSE) 
  
  # add in a check for "Unsure" based on ages of everything else
  return(species.list)
}

# old version of the function
# compare <- function (forams.sample, micro = FALSE) {
#   # load lookup tables
#   library("openxlsx")
#   path <- gsub("/[^/]*(Box|Dropbox).*/.*$", "", getwd())
#   forams.tracy <- read.xlsx(paste0(path, "/", grep("ropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "tracystax")
#   aze.species <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "Tracysspecies")
#   forams.lookup <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "foramslookup")
#   lookup.species <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "lookupwodup")
#   forams.lookup.micro <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "foramslookup_micro")
#   lookup.species.micro <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms_old.xlsx"), sheet = "lookupwodup_micro")
#   
#   # create the matching function
#   unitestfun <- function (sample, forams, lookup) {
#     if (is.na(match(sample, forams$Species)) == FALSE) {
#       return(as.vector(forams$Full[match(sample, forams$Species)]))
#     } else {
#       if (is.na(match(sample, lookup$Neptune.names)) == FALSE) {
#         return(as.vector(lookup$Tracy.names[match(sample, lookup$Neptune.names)]))
#       } else { 
#         return("unknown")
#       }
#     }
#   }
#   
#   # create the compare function
#   comp.func <- function(forams.sample, micro) {
#     forams.sample <- as.character(forams.sample) ## make sure it is not a factor
#     split <- strsplit(forams.sample, " ") [[1]] [2] # take the second name
#     spec <- strsplit(forams.sample, " ") [[1]] [1] # and the first name
#     if (is.na(split) == FALSE) {
#       # if the name is genus and species
#       species <- unitestfun(forams.sample, forams.tracy, forams.lookup)
#     } else {
#       # if there is only a species name
#       species <- unitestfun(spec, aze.species, lookup.species)    
#     }
#     # if we should resolve microperforate names
#     if (micro & species == "Micro") {
#       if (is.na(split) == FALSE) {
#         # if the name is genus and species
#         species <- unitestfun(forams.sample, forams.tracy, forams.lookup.micro)
#       } else {
#         # if there is only a species name
#         species <- unitestfun(spec, aze.species, lookup.species.micro)    
#       }
#     }
#     return(species)
#   }
#   species.list <- sapply(forams.sample, comp.func, USE.NAMES = FALSE, micro = micro) 
#   return(species.list)
# }
# 
## example
# sample.names <- sapply(as.character(forams.bfd[, 1]), compare, USE.NAMES = FALSE)
# 
# 
