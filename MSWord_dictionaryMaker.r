if (!requireNamespace("arcgisbinding", quietly=TRUE)) install.packages("arcgisbinding")
require(arcgisbinding)
if (!requireNamespace("tidyr", quietly=TRUE)) install.packages("tidyr")
require(tidyr)
if (!requireNamespace("hunspell", quietly=TRUE)) install.packages("hunspell")
require(hunspell)
if (!requireNamespace("here", quietly=TRUE)) install.packages("here")
require(here)

arc.check_product()
arc.check_portal()  # may need to update bridge to most recent version if it crashes: https://github.com/R-ArcGIS/r-bridge/issues/46

# get the element tracking list
bioticsFeatServ_path <- "https://maps.waterlandlife.org/arcgis/rest/services/PNHP/Biotics/FeatureServer"
ET <- arc.open(paste0(bioticsFeatServ_path,"/5"))  # 5 is the number of the ET
ET <- arc.select(ET, c("SNAME","SCOMNAME"))

# make a list of scientific names
ET_SNAME <- ET["SNAME"]
ET_SNAME <- separate_rows(ET_SNAME, SNAME)
ET_SNAME <- unique(ET_SNAME)
ET_SNAME <- ET_SNAME[which(ET_SNAME!="var." & ET_SNAME!="sp." & ET_SNAME!="ssp." & ET_SNAME!="pop." & ET_SNAME!="nr."),] # remove infrataxa modifiers
ET_SNAME <- ET_SNAME[!ET_SNAME$SNAME %in% 1:100,] # remove any numbers (eg. 'Cottus sp. 7')
ET_SNAME <- ET_SNAME[which(ET_SNAME!=""&ET_SNAME!=" "),] # remove blanks
ET_SNAME <- ET_SNAME[!ET_SNAME$SNAME %in% letters,]
e <- hunspell(ET_SNAME$SNAME)
e <- unlist(e[lapply(e,length)>0])
ET_SNAME <- ET_SNAME[ET_SNAME$SNAME %in% e,]

# make a list of common names
ET_SCOMNAME <- ET["SCOMNAME"]
ET_SCOMNAME <- separate_rows(ET_SCOMNAME, SCOMNAME)
ET_SCOMNAME <- unique(ET_SCOMNAME)

ET_SCOMNAME <- ET_SCOMNAME[which(ET_SCOMNAME!=""&ET_SCOMNAME!=" "),] # remove blanks
ET_SCOMNAME <- ET_SCOMNAME[!ET_SCOMNAME$SCOMNAME %in% letters,]
ET_SCOMNAME <- ET_SCOMNAME[!ET_SCOMNAME$SCOMNAME %in% LETTERS,]

e <- hunspell(ET_SCOMNAME$SCOMNAME)
e <- unlist(e[lapply(e,length)>0])
ET_SCOMNAME <- ET_SCOMNAME[ET_SCOMNAME$SCOMNAME %in% e,]

# need to fin all the genus names and remove the lowercase ones from the common name list
f <- regmatches(ET_SNAME$SNAME, gregexpr("\\b[A-Z]\\w+", ET_SNAME$SNAME))
f <- unlist(f[lapply(f,length)>0])
f <- tolower(f)
ET_SCOMNAME <- ET_SCOMNAME[!ET_SCOMNAME$SCOMNAME %in% f,]

# additional words
addWords_mod <- c("sp.","spp.","var.","pop.","nr.")
addWords_org <- c("PNHP","NatureServe","DCNR","PGC","PFBC","USFS","USFWS","FWS","USDA","WPC")
addWords_tools <- c("Biotics","iMap","iMapInvasives","COA") 
addWords_sciencewords <- c("infrataxa","heliophytic")
#addWords_dashedspecies <- c("y-inversum","x-punctatus","u-album","l-album") ### NOTE: Word doens't accept compound words in spell check...
addWords <- c(addWords_mod, addWords_org, addWords_tools,addWords_sciencewords) #,addWords_dashedspecies
rm(addWords_mod, addWords_org, addWords_tools,addWords_sciencewords) #,addWords_dashedspecies

# # GNIS names for PA, so we can spellcheck place names and such
fn <- "PA_Features.zip"
download.file(url="https://geonames.usgs.gov/docs/stategaz/PA_Features.zip", destfile = here::here(fn)) # https://www.usgs.gov/core-science-systems/ngp/board-on-geographic-names/download-gnis-data
unzip(here::here(fn))
# find a the most recent text file
gnis <- read.delim(here::here("PA_Features_20210825.txt"), sep="|", stringsAsFactors=FALSE)
gnis <- gnis[c("FEATURE_NAME","FEATURE_CLASS","COUNTY_NAME","MAP_NAME")]
gnis <- gnis[which(gnis$FEATURE_CLASS!="Building" & gnis$FEATURE_CLASS!="Hospital" & gnis$FEATURE_CLASS!="School" & gnis$FEATURE_CLASS!="Gut" & gnis$FEATURE_CLASS!="Airport" & gnis$FEATURE_CLASS!="Military" & gnis$FEATURE_CLASS!="Oilfield" & gnis$FEATURE_CLASS!="Tower"),] # clear out some names we're likely not to use
#Delete zipfile if it exists
if (file.exists(here::here(fn))) { #Check its existence
  file.remove(here::here(fn))
}

# get feature names
gnis_feature <- gnis['FEATURE_NAME']
gnis_feature <- separate_rows(gnis_feature, FEATURE_NAME)
gnis_feature <- unique(gnis_feature)
e <- hunspell(gnis_feature$FEATURE_NAME)
e <- unlist(e[lapply(e,length)>0])
gnis_feature <- gnis_feature[gnis_feature$FEATURE_NAME %in% e,]

# get topo maps names
gnis_mapname <- gnis['MAP_NAME']
gnis_mapname <- separate_rows(gnis_mapname, MAP_NAME)
gnis_mapname <- unique(gnis_mapname)
e <- hunspell(gnis_mapname$MAP_NAME)
e <- unlist(e[lapply(e,length)>0])
gnis_mapname <- gnis_mapname[gnis_mapname$MAP_NAME %in% e,]

# get county names since we're here...
gnis_countyname <- gnis['COUNTY_NAME']
gnis_countyname <- separate_rows(gnis_countyname, COUNTY_NAME)
gnis_countyname <- unique(gnis_countyname)
e <- hunspell(gnis_countyname$COUNTY_NAME)
e <- unlist(e[lapply(e,length)>0])
gnis_countyname <- gnis_countyname[gnis_countyname$COUNTY_NAME %in% e,]

# assemble all the bits from above
final_dictionary <- c(ET_SCOMNAME$SCOMNAME, ET_SNAME$SNAME, addWords, gnis_feature$FEATURE_NAME, gnis_mapname$MAP_NAME, gnis_countyname$COUNTY_NAME)
final_dictionary <- unique(final_dictionary)
final_dictionary <- sort(final_dictionary)
final_dictionary <- as.data.frame(final_dictionary)
names(final_dictionary) <- "DIC"

write.csv(final_dictionary, "PNHP.dic", row.names=FALSE, quote=FALSE, fileEncoding="UTF-16LE") # has to be in a unicode compatible format for Office 365

