#' Get location from coordinate
#' Take in coordiantes and return the location
#' @param location longtitude and latitude
#' @param limit integer value.If the number of row exceeded limit, function will run in parallel
#' @return the corresponding locations
#' 
#' @references
#' 1. http://lbs.baidu.com/index.php?title=webapi/guide/webservice-geocoding-abroad
#' 
#' @examples
#' \dontrun{  
#' ## get one location 
#' location_one = get_location(c(118.12845, 24.57742))
#' 
#' ## vectorization
#' loc = matrix(c(117.93780, 24.55730, 117.93291, 24.57745, 117.23530, 24.64210, 117.05890, 24.74860), byrow=T, ncol=2)
#' get_location(loc)
#' }
#' @export
get_location = function (location, limit=600, map_ak = '') {
    map_ak %<>% check_mapkey()
    FUN = function(x) get_location.core(x, map_ak)
    .parallel = ifelse(NROW(location) < limit, FALSE, TRUE)
    res <- parApply2(location, FUN, .parallel = .parallel)
    res
}

get_location.core = function(location, map_ak){    
    if (!class(location) %in% c('matrix', 'data.frame')){
        location = matrix(location, ncol=2, byrow=T)
    }
    lon = location[, 1]
    lat = location[, 2]
    
    url = sprintf("http://api.map.baidu.com/reverse_geocoding/v3/?ak=%s&output=json&coordtype=wgs84ll&location=%s,%s", 
                  map_ak, lat, lon)
    ans = GET(url) %>% content() %>% fromJSON() %>% .$result
    ans %<>% {c(.[[1]], .$addressComponent[-(1:4)], .[-c(1,4)])} %>% 
        fix_null %>% as.data.frame()
    ans
}
