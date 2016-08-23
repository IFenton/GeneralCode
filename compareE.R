## function to determine names of BFD based on lookup tables provided by Tracey Aze
## identifies correct names for macroperforate planktonic foraminifera
## see working in BFD notes

## input - species names (can be binomial or just species name)
## output - corrected version of that name if unknown

compare<-function (forams.sample) {
  # load lookup tables
  forams.tracey<-read.csv("E:/Science/PhD/Project/Foraminifera/Data/traceystax.csv")
  aze.species<-read.csv("E:/Science/PhD/Project/Foraminifera/Data/Traceysspecies.csv")
  forams.lookup<-read.csv("E:/Science/PhD/Project/Foraminifera/Data/foramslookup.csv")
  lookup.species<-read.csv("E:/Science/PhD/Project/Foraminifera/Data/lookupwodup.csv")
  
  # create the matching function
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
  
  # run that function
  forams.sample <- as.character(forams.sample) ## added this line
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
