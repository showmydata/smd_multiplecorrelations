get_data_from_url <- function (data,session,datalink) { 
  
  data2return=data;
  
  urlparameters <- parseQueryString(session$clientData$url_search)        # Get any passed parameters from url
  if (!is.null(urlparameters[['datalink']])) {                            # If there's a datalink in the passed parameters
    datalink2read=urlparameters["datalink"]                               # then grab it. 
    datalink2read=gsub("goosheet", "docs.google.com/spreadsheets", datalink2read, fixed=TRUE); # Change goosheet back to full google URL
    if (grepl("google.com/spreadsheets", datalink2read, fixed = TRUE)) {  # If there's google sheet info somewhere in the data link
      hash <- parseQueryString(session$clientData$url_hash)               # then grab any hash in url
      if (!is.null(hash[['#gid']])) {                                     # If that hash grab produces a #gid (thus indicating that the url points to a specific tab, or sheet, within the google sheet)
        url_hash=session$clientData$url_hash                              # then get the hash
        datalink2read=paste0(datalink2read,url_hash)                      # and add it at the end of the url
      }
      data2return <-gsheet2tbl(datalink2read)                             # Get data from google sheet. 
    } else {
      if (grepl("dropbox", datalink2read, fixed = TRUE) & !grepl("raw=1", datalink2read, fixed = TRUE)) datalink2read = paste0(datalink2read, "&raw=1") 
      data2return <-read_csv(as.character(datalink2read))
    }
  }
  if (datalink!="") {                                                     # Also, if there is a data link in the box
    datalink2read=datalink;                                               # then get it. 
    if (grepl("google.com/spreadsheets", datalink2read, fixed = TRUE)) data2return <-gsheet2tbl(datalink2read) # Get data from google sheet. 
    else if (grepl(".csv", datalink2read, fixed = TRUE)) {
      if (grepl("dropbox", datalink2read, fixed = TRUE) & !grepl("raw=1", datalink2read, fixed = TRUE)) datalink2read = paste0(datalink2read, "&raw=1") 
      data2return <-read.csv(datalink2read)}
    else if (grepl(".xlsx", datalink2read, fixed = TRUE)) data2return <- read_excel(datalink2read)
    else data2return=data}
  
  return(data2return)
}