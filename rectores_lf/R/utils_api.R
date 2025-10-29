# R/utils_api.R
safe_lib <- function(pkgs){
  invisible(lapply(pkgs, function(p) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)))
  invisible(lapply(pkgs, library, character.only=TRUE))
}

scto_download_json_wide <- function(server, formid, email, password, since=0, max_attempts=10){
  safe_lib(c("httr","jsonlite"))
  url <- sprintf("https://%s.surveycto.com/api/v2/forms/data/wide/json/%s?date=%s", server, formid, since)
  attempt <- 1
  repeat {
    resp <- try(httr::POST(url,
                           config = httr::authenticate(email, password),
                           httr::add_headers("Content-Type"="application/json"),
                           encode="json",
                           timeout(120)), silent=TRUE)
    ok <- !(inherits(resp,"try-error")) && httr::status_code(resp) %in% c(200,201)
    if(ok){
      df <- jsonlite::fromJSON(rawToChar(resp$content), flatten=TRUE)
      if(is.data.frame(df)) return(df)
    }
    if(attempt >= max_attempts) stop("API SurveyCTO: no se logró obtener un data.frame válido.")
    Sys.sleep(5*attempt); attempt <- attempt + 1
  }
}

normalize_multi <- function(df, base_names){
  safe_lib(c("dplyr","stringr"))
  out <- df
  for (var in base_names){
    var_cols <- names(out)[startsWith(names(out), paste0(var, "_")) & !grepl("_o$", names(out))]
    if(length(var_cols) > 0){
      out <- out |>
        dplyr::rowwise() |>
        dplyr::mutate(!!var := {
          vals <- dplyr::c_across(dplyr::all_of(var_cols))
          if(all(is.na(vals))) NA_character_ else {
            activos <- which(vals == 1)
            if(!length(activos)) NA_character_ else {
              seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
              paste(seleccionados, collapse=",")
            }
          }
        }) |>
        dplyr::ungroup()
    }
  }
  out
}
