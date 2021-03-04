arc.dataconvert <- function(json,export=FALSE){

oldw <- getOption("warn")
options(warn = -1)

library(gdata) # for the trim function
library(jsonlite)
library(knitr)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)

options(warn = oldw)

dataARC_raw_input <- fromJSON(json, simplifyDataFrame = TRUE)

#read in features as a flattened dataframe
flt_input <- jsonlite::flatten(as.data.frame(dataARC_raw_input$features))

flt_combinators <- jsonlite::flatten(as.data.frame(dataARC_raw_input$combinators))
tbl_comb <- as_tibble(flt_combinators)
tbl_comb_2 <- rename(tbl_comb, combinatorid = `_id`)
reduced_comb_data <- tbl_comb_2 %>% dplyr::select("combinatorid", "dataset", "features", "concepts")

flt_concepts <- jsonlite::flatten(as.data.frame(dataARC_raw_input$concepts))
tbl_concepts <- as_tibble(flt_concepts)
tbl_concepts_2 <- rename(tbl_concepts, conceptid = `_id`)


#make flat feature dataframe into manipulable tibble
tbl_flt <- as_tibble(flt_input)

#select desired input fields from tibble
reduced_data <- tbl_flt %>% dplyr::select("_id", "dataset", location.coordinates, starts_with("properties"), -"properties.finds")

#unnest coordinates within reduced dataset - this flattens deeply listed
#data from the tephrabase and jardabok datasets, among others
unnested_coords <- reduced_data %>% mutate(values = purrr::map(location.coordinates, setNames, c("long", "lat"))) %>% unnest_wider(values)
unnested_coords_2 <- unnested_coords %>% mutate(values = unnested_coords$properties.data) %>% unnest_wider(values)
unnested_coords_3 <- unnested_coords_2 %>% mutate(values = unnested_coords_2$properties.relationships) %>% unnest_wider(values)
unnested_coords_4 <- unnested_coords_3 %>% mutate(values = unnested_coords_3$properties.tephra) %>% unnest_wider(values)
unnested_coords_5 <- unnested_coords_4 %>% select(-"location.coordinates", -"properties.data", -"properties.relationships", -"properties.tephra")

#turns tibble into a data.table for more manipulation
dt <- as.data.table(unnested_coords_5)

#determines if columns are all NA
dt[, "SUM" := rowSums(!is.na(.SD))]

#and deletes them if so
dt <- dt[SUM > 0,]

#recouperates resource list from Farm Histories Database 
recs <- dt[, list(relationships_lookup_resource_en = 
            as.character(unlist(relationships_lookup_resource_en))), 
   by = "_id"]
dt[, relationships_lookup_resource_en := NULL]

#joins the data back into the manipulated datatable
dt <- recs[dt,on="_id",all=TRUE]

oldw <- getOption("warn")
options(warn = -1)

#break the data apart using the ID and dataset variables as the ID variables
#keeping only properties and the resources
dt1 <- melt(dt, id.vars=c("_id","dataset"),
            measure.vars = names(dt)[str_detect(names(dt),
                                                "properties|relationships_lookup_resource_en")])

options(warn = oldw)
#remove "properties" prefix from subfields
dt1[, variable := str_replace(variable,"properties.","")]
#remove NA rows from the datatable
dt1 <- na.omit(dt1)

#name columns appropriately
cols = c("FID","FEATURE","Y","X","DATASET","CATEGORY","ENTRY")
#recreates lat/long from original datatable
dt2 <- dt[,.(`_id`,lat,long)]

#merges lat/long into dt1 datatable
dt1 <- dt1[unique(dt2),on="_id"]
#creates sequential Feature IDs ("FIDs") for
dt1[, FID := 1:nrow(dt1)]

#sets column names appropriately
setnames(dt1,c("_id","long","lat","dataset","variable","value"),
         cols[2:length(cols)])
#orders the columns by the cols variable in the data table dt1
#assigns them to a new data table "out"
out <- dt1[,..cols]

#Bring in combinator (r) and concepts/contextual/related (s) dataframes
r <- as.data.table(reduced_comb_data)
s <- as.data.table(tbl_concepts_2)[,.(conceptid,contextual,related)]

#converts context/related lists into hash strings with colon separators
cols <- c('contextual','related')
s[, (cols) := lapply(.SD, function(x) {x <- x[[1]]
paste(unique(x),collapse=":")}),
.SDcols = cols]
setnames(s,"conceptid","concepts")

#Unlists the concepts and associated features from the combinators datatable
r1 <- r[, .(features = unlist(features)), by = combinatorid]
r2 <- r[, .(concepts = unlist(concepts)), by = combinatorid]

#joins the appropriate related and context to each concepts
r2 <- r2[s,on="concepts"]

#combines then onto the features by combinator
#before converting them to hash strings with colon separators by feature
r <- r1[r2,on='combinatorid', all=TRUE
][,.(concepts = paste(unique(concepts),collapse=":"),
     combinators = paste(unique(combinatorid),collapse=":"),
     related = paste(unique(related),collapse=":"),
     context = paste(unique(contextual),collapse=":")), 
  by=features]

#set the output order, convert to capital
cols <- c("feature","concepts","combinators","related","context")
setnames(r,"features","feature")
setnames(r, cols, toupper(cols))

#combines the concepts and data with its original X and Y coordinates
output <- out[r,on="FEATURE",all=TRUE]

#makes hashes uppercase
hashcols <- c("FEATURE","DATASET","CONCEPTS","CATEGORY", "COMBINATORS")
output[, (hashcols) := lapply(.SD,toupper),.SDcols = hashcols]

#select export columns, drop NAs in data columns 
cols = c("FID","FEATURE","X","Y","DATASET","CATEGORY","ENTRY",
         "CONCEPTS","COMBINATORS","RELATED","CONTEXT")
output <- na.omit(output, cols=c("X","Y","ENTRY"))

#export
if (is.character(export)) {
  fwrite(output[,..cols], file=paste0(export,".gz"))
} else {
  output
}
}