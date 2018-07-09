library(sunburstR)
hierarchy <- read.table("sunburstICD10.csv",sep = "\t", header=T, 
                        quote="",fileEncoding = "UTF-8")
save(hierarchy, file="hierarchy.rdata")
eventCount <- data.frame(className=c("Y118","Z613"), size=c(10,15))
bool <- nrow(eventCount) == 0
## we keep only codes with count != 0 
bool <- hierarchy$code %in% eventCount$className
hierarchicalData <- subset (hierarchy, bool)
hierarchicalData <- merge (hierarchy, eventCount, by.x="code", by.y="className")
colnames(hierarchicalData) <- c("code","label","hierarchy","size")

sunburstData <- subset(hierarchicalData, select=c("hierarchy","size"))
sunburstR::sunburst(sunburstData, count=T,legend = list(w=200))
