#######################
# daten herunterladen #
#######################

### daten via api von opendata.swiss herunterladen

## api wrapper
search_data <- function(keyword){
  
  url_tag <- sprintf("https://opendata.swiss/api/3/action/package_search?fq=%s&groups=social-security&organization=bundesamt-fur-statistik-bfs", URLencode(keyword))
  fromJSON(url_tag)
}

## keywords
# sozialhilfe / wsh
# invalidenversicherung / iv
# sozialen sicherheit
# sozialleistungen
# leistungssystem