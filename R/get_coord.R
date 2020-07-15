#' Get coordiante from address
#' Take in address and return the coordinate
#' @param address address
#' @param city the city of the address, optional
#' @param output should be "json" or "xml", the type of the result
#' @param formatted logical value, return the coordinates or the original results
#' @param limit integer value.If the length of address exceeded limit, function will run in parallel
#' 
#' @return A vector contains the  corresponding coordiante. If "formatted=TRUE", 
#' return the numeric coordinates, otherwise return json or xml type result, 
#' depents on the argument "output". If the length of address is larger than 1, 
#' the result is a matrix.
#' 
#' @references
#' 1. http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-geocoding
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @import magrittr glue
#' @export
get_coord <- function(address, city=NULL, map_ak='', limit=600, .parallel = NULL){
    map_ak %<>% check_mapkey()

    # FUN = function(x) get_coord.core(x, city, map_ak)
    if (is.null(.parallel)) .parallel = ifelse(length(address) < limit, FALSE, TRUE)
    
    res <- parLapply2(X = address, get_coord.core, 
        city = city, map_ak = map_ak, .parallel = .parallel)
    res
}

get_coord.core <- function(address, city=NULL, map_ak = ''){
    if (any(grepl(' |#', address))) warning('address should not have blank character!')
    address = gsub(' |#', '', address)
    
    str_city <- ifelse(is.null(city), "", paste0("&city=", city))
    url <- glue('http://api.map.baidu.com/geocoding/v3/?address={address}{str_city}&output=json&ak={map_ak}')

    tryCatch({
        content(GET(url)) %>% fromJSON() %>% .$result %>% 
            {c(.$location, .[-(1:2)])} %>% as.data.frame()
    }, error = function(e) {
        message(sprintf('%s', e))
    })
}
