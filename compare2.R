## function to determine names of BFD based on lookup tables provided by Tracy Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare <- function (forams.sample, micro = FALSE, st.age = NA, en.age = NA, age.check = FALSE) {
  # st.age - sample age(s), or oldest age
  # en.age - youngest age (where applicable)
  
  # load lookup tables
  library("openxlsx")
  path <- gsub("/[^/]*(Box|Dropbox).*/.*$", "", getwd())
  forams.lookup <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE), "/Documents/PhD/Project/Foraminifera/Data/ForamSynonyms.xlsx"), sheet = "foramslookup")
  forams.ages <- read.xlsx(paste0(path, "/", grep("dropbox", list.files(path), value = TRUE, ignore.case = TRUE),"/Documents/PhD/Project/Foraminifera/Data/IF functional data.xlsx"))
  forams.lookup <- forams.lookup[,1:3]
  
  lookup.table <- function(lookup) {
    # create a version of the lookup table which only has the species names
    species.nm <- lookup
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
    abb.nm <- lookup
    abb.nm <- rbind(abb.nm, tmp)
    # remove the first name (genus)
    abb.nm$Synonym <- gsub("^(.)[^ ]* ", "\\1\\. ", abb.nm$Synonym)
    # remove duplicates from mispelled genus, where the accepted name is the same
    abb.nm <- abb.nm[!duplicated(abb.nm[,1:2]),]
    # where there are still duplicates, set these as "Unsure"
    abb.dup <- abb.nm$Synonym[duplicated(abb.nm$Synonym) & abb.nm$AcceptedName != "Unsure"]
    abb.nm$AcceptedName[abb.nm$Synonym %in% unique(abb.dup)] <- "Unsure"
    
    # merge these three dataframes
    return(rbind(lookup, species.nm, abb.nm))
  }
  
  full.lookup <- lookup.table(forams.lookup)
  
  # if not micro
  if (!micro) {
    full.lookup$AcceptedName[full.lookup$Micro == "Yes"] <- "Micro"
  }
  
  # create the compare function
  comp.func <- function(forams.sample, lookup) {
    forams.sample <- as.character(forams.sample) ## make sure it is not a factor
    species <- lookup$AcceptedName[match(forams.sample, lookup$AcceptedName)]
    if (is.na(species)) {
      species <- lookup$AcceptedName[match(forams.sample, lookup$Synonym)]
      if (is.na(species))
        species <- "unknown"
    }
    return(species)
  }
  
  species.list <- sapply(forams.sample, comp.func, full.lookup, USE.NAMES = FALSE) 
  
  # add in a check for "Unsure" based on ages of everything else
  if ((!is.na(st.age[1]) | age.check) & any(species.list == "Unsure")) {
    # calculate max / min ages
    if (is.na(st.age[1])) {
      # if these aren't already specified, then base it on the species list
      st.age <- max(forams.ages$mSt_CK1995[forams.ages$specName %in% species.list])
      en.age <- min(forams.ages$mEn_CK1995[forams.ages$specName %in% species.list])
    }
    beg.age <- max(st.age, en.age, na.rm = TRUE)
    end.age <- min(st.age, en.age, na.rm = TRUE)
    age.spp <- forams.ages$specName[forams.ages$mEn_CK1995 <= beg.age & forams.ages$mSt_CK1995 >= end.age]
    age.forams.lookup <- forams.lookup[forams.lookup$AcceptedName %in% age.spp,]
    
    # recalulate the lookup table, so that Unsure is only specified within the ages
    age.full.lookup <- lookup.table(age.forams.lookup)
    # if not micro
    if (!micro) {
      age.full.lookup$AcceptedName[age.full.lookup$Micro == "Yes"] <- "Micro"
    }
    
    species.list[species.list == "Unsure"] <- ifelse(sapply(forams.sample[species.list == "Unsure"], comp.func, age.full.lookup, USE.NAMES = FALSE) == "unknown", "Unsure", sapply(forams.sample[species.list == "Unsure"], comp.func, age.full.lookup, USE.NAMES = FALSE))
  }
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
