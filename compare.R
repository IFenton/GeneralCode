## function to determine names of BFD based on lookup tables provided by Tracey Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare<-function (forams.sample) {
  forams.tracey<-read.csv("C:/Documents/Science/PhD/Project/Foraminifera/Data/traceystax.csv")
  aze.species<-read.csv("C:/Documents/Science/PhD/Project/Foraminifera/Data/Traceysspecies.csv")
  forams.lookup<-read.csv("C:/Documents/Science/PhD/Project/Foraminifera/Data/foramslookup.csv")
  lookup.species<-read.csv("C:/Documents/Science/PhD/Project/Foraminifera/Data/lookupwodup.csv")
  
  
  unitestfun<-function (sample,forams,lookup) {
    if (is.na(match(sample,forams$Species)) == FALSE) {
      return(as.vector(forams$Full[match(sample,forams$Species)]))
    } else {
      if (is.na(match(sample,lookup$Neptune.names)) == FALSE) {
        return(as.vector(lookup$Tracey.names[match(sample,lookup$Neptune.names)]))
      } else { 
        return("unknown")
      }
    }
  }
  split<- strsplit (forams.sample," ") [[1]] [2]
  spec<- strsplit (forams.sample," ") [[1]] [1]
  if (is.na(split) == FALSE) {
    unitestfun(forams.sample,forams.tracey,forams.lookup)
  } else {
    unitestfun(spec,aze.species,lookup.species)    
  }
}

## example
# sample.names<-sapply(as.character(forams.bfd[,1]),compare,USE.NAMES=FALSE)
