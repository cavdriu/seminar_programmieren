###########################
### daten herunterladen ###
###########################
# source: https://h4sci.github.io/h4sci-book/case-studies.html#build-your-own-api-wrapper
# source: https://bookdown.org/joone/ComputationalMethods/apis.html#api-anfragen-in-r-ausf%C3%BChren-mit-httr
# source: https://handbook.opendata.swiss/de/content/nutzen/api-nutzen.html

### daten via api von opendata.swiss herunterladen


## api search wrapper
# search_data <- function(search_string){
#   
#   url <- sprintf("https://opendata.swiss/api/3/action/package_search?q=%s", URLencode(keyword))
#   fromJSON(url)
# }

## api search wrapper with httr
search_data2 <- function(search_string){
  
 url <-  httr::GET(url = "https://opendata.swiss",
                   path = "/api/3/action/package_search",
                   query = list(q = search_string))
}

resp <- search_data2("bfs iv")
stop_for_status(url)
str(resp, max.level = 1)
resp$headers$`content-type`
resp_content <- content(url, "text")
parsed_content <- jsonlite::fromJSON(resp_content)
str(parsed_content, max.level = 1)
x <- parsed_content$result$results$resources#$download_url
head(str(x))
length(x)
x[[2]]$download_url
x[1]



## keywords
# sozialhilfe / wsh
# invalidenversicherung / iv
# sozialen sicherheit
# sozialleistungen
# leistungssystem

# download_data_by_id <- functions(ids,
#                                  download = ??) {
#   
#   # beschreibende metadaten abrufen
#   obj_list <- lapply(ids, function(x){
#     req <- download.file(sprintf("https://opendata.swiss/api/3/action/package_show?id=%s", x), 
#                          destfile = "temp.json")
#     fromJSON("temp.json")
#   })
# 
#   #
#   data_urls <- lapply(obj_list, "[[", download)
# }
# 
# 
# 
# # Obtain meta description objects from MET API
# obj_list <- lapply(ids, function(x) {
#   req <- download.file(sprintf("https://collectionapi.metmuseum.org/public/collection/v1/objects/%s",
#                                x),destfile = "temp.json")
#   fromJSON("temp.json")
# })

