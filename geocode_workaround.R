#https://github.com/ropensci/rtweet/issues/391#issuecomment-786942538

lookup_coords_nominatim <- function(address, ...) {
  if (missing(address)) stop("must supply address", call. = FALSE)
  stopifnot(is.atomic(address))
  place <- address
  if (grepl("^us$|^usa$|^united states$|^u\\.s",
            address,
            ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -124.848974,
      sw.lat = 24.396308,
      ne.lng = -66.885444,
      ne.lat = 49.384358
    )
    point <- c(
      lat = 36.89,
      lng = -95.867
    )
  } else if (grepl("^world$|^all$|^globe$|^earth$",
                   address,
                   ignore.case = TRUE
  )) {
    boxp <- c(
      sw.lng = -180,
      sw.lat = -90,
      ne.lng = 180,
      ne.lat = 90
    )
    point <- c(
      lat = 0,
      lng = 0
    )
  } else {
    ## encode address
    address <- gsub(" ", "+",  address)
    ## compose query
    params <- list(
      q = address,
      format = "json",
      limit = 1
    )
    params <- params[!vapply(params, is.null, logical(1))]
    params <- paste0(
      mapply(
        function(x, y) paste0(x, "=", y),
        names(params), params
      ),
      collapse = "&"
    )
    ## build URL - final name in English
    geourl <- paste0(
      "https://nominatim.openstreetmap.org/search?",
      params,
      "&accept-language=en"
    )
    ## read and convert to list obj
    r <- jsonlite::fromJSON(geourl)
    ## extract and name box and point data frames
    bbox <- as.double(unlist(r$boundingbox))
    boxp <- c(
      sw.lng = bbox[3],
      sw.lat = bbox[1],
      ne.lng = bbox[4],
      ne.lat = bbox[2]
    )
    point <- c(
      lat = as.double(r$lat),
      lng = as.double(r$lon)
    )
    # Full name from Nominatim
    place <- r$display_name
  }
  rtweet:::as.coords(place = place, box = boxp, point = point) # need to make this call for the reprex
}
