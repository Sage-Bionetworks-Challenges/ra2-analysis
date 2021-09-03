use_condaenv("ra2dream", required = T)
synapseclient <- reticulate::import('synapseclient')
syn <- synapseclient$Synapse()

##login to service account
syn$login("ra2dreamservice", "####")

make_public <- function(id){
  #set registered synapse users to view, download
  syn$setPermissions(entity = id,
                     principalId = "273948", 
                     accessType = list("READ","DOWNLOAD"))
  
  #set public to view
  syn$setPermissions(entity = id,
                     principalId = "273949", 
                     accessType = list("READ"))
}


leaderboard <- syn$tableQuery("select * from syn22214742 where status = 'ACCEPTED'")$asDataFrame()
final <- syn$tableQuery("select * from syn22236264 where status = 'ACCEPTED'")$asDataFrame()
validation_1 <- syn$tableQuery("select * from syn25186123 where status = 'ACCEPTED'")$asDataFrame()
validation_2 <- syn$tableQuery("select * from syn25710243 where status = 'ACCEPTED'")$asDataFrame()
validation_3 <- syn$tableQuery("select * from syn25875469 where status = 'ACCEPTED'")$asDataFrame()

ids <- c(leaderboard$prediction_fileid, final$prediction_fileid, validation_1$prediction_fileid, validation_2$prediction_fileid, validation_3$prediction_fileid)

sapply(ids, make_public)
