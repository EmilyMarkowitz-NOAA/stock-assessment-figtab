# This has a specific username and password because I DONT want people to have access to this!
source("Z:/Projects/ConnectToOracle.R")

# I set up a ConnectToOracle.R that looks like this: 
#   
#   PKG <- c("RODBC")
# for (p in PKG) {
#   if(!require(p,character.only = TRUE)) {  
#     install.packages(p)
#     require(p,character.only = TRUE)}
# }
# 
# channel<-odbcConnect(dsn = "AFSC",
#                      uid = "USERNAME", # change
#                      pwd = "PASSWORD", #change
#                      believeNRows = FALSE)
# 
# odbcGetInfo(channel)

for (i in 1:length(locations)){
  print(locations[i])
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  write.csv(x = a, 
            paste0(dir_data,
                   tolower(gsub(pattern = '\\"', 
                                replacement = "", 
                                x = strsplit(x = locations[i], 
                                             split = ".", 
                                             fixed = TRUE)[[1]][2], 
                                perl = TRUE)),
                   ".csv"))
  remove(a)
}
