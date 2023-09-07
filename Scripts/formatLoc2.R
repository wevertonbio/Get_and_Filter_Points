fixLoc2 <- function (x, loc.levels = c("country", "stateProvince", "municipality", 
                            "locality"), scrap = TRUE, to.lower = TRUE) 
{
  if (!class(x)[1] == "data.frame") 
    stop("input object needs to be a data frame!")
  na.strings <- c("", " ", "NA", NA)
  if ("countryCode" %in% names(x) & "country" %in% names(x)) {
    ids <- !x$countryCode %in% na.strings & x$country %in% 
      na.strings
    x$country[ids] <- x$countryCode[ids]
  }
  if ("countryCode" %in% names(x) & !"country" %in% names(x)) 
    colnames(x)[which(colnames(x) == "countryCode")] <- "country"
  if ("municipality" %in% names(x) & "county" %in% names(x)) {
    ids <- !x$county %in% na.strings & x$municipality %in% 
      na.strings
    x$municipality[ids] <- x$county[ids]
  }
  if (!"municipality" %in% names(x) & "county" %in% names(x)) 
    colnames(x)[which(colnames(x) == "county")] <- "municipality"
  if ("locality" %in% names(x) & "verbatimLocality" %in% names(x)) {
    ids <- !x$verbatimLocality %in% na.strings & x$locality %in% 
      na.strings
    x$locality[ids] <- x$verbatimLocality[ids]
  }
  if (!"locality" %in% names(x) & "verbatimLocality" %in% 
      names(x)) 
    colnames(x)[which(colnames(x) == "verbatimLocality")] <- "locality"
  if (!any(c("country", "stateProvince", "municipality", "locality") %in% 
           colnames(x))) 
    stop("input object needs to have at least one of the following fields: country/countryCode, stateProvince, municipality/county and locality/verbatimLocality")
  x1 <- x[, match(loc.levels, colnames(x)), drop = FALSE]
  enc <- c("a", "a", "a", "a", "c", "e", "e", "e", "o", "o", "o",
           "i", "i", "u", "u")
  names(enc) <- c("á", "ã", "â", "ä", "ç", "é", "ê", "ë", "ö", "ó", "ô",
           "í", "ï", "ú", "ü")
  dic <- plantR:::replaceNames
  missLocs <- plantR:::missLocs
  wordsForSearch <- plantR:::wordsForSearch
  for (i in 1:length(enc)) x1[] <- lapply(x1, gsub, pattern = names(enc)[i], 
                                          replacement = enc[i], ignore.case = TRUE, perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "­", replacement = "", 
                 perl = TRUE)
  x1[] <- lapply(x1, gsub, pattern = "\nO", replacement = "", 
                 perl = TRUE)
  if (any(c("country", "countryCode") %in% loc.levels)) {
    x1[, "country"] <- prepCountry(x1[, "country"])
    pattern <- paste(missLocs, collapse = "|")
    x1[, "country"] <- gsub(pattern, NA, x1[, "country"], 
                            perl = TRUE)
    x1[, "country"][grepl("desconhecid|unknown", x1[, "country"], 
                          perl = TRUE)] <- NA
    bracks <- grepl("^\\[", x1[, "country"], perl = TRUE) & 
      grepl("\\]$", x1[, "country"], perl = TRUE)
    if (any(bracks)) 
      x1[bracks, "country"] <- gsub("^\\[|\\]$", "", x1[bracks, 
                                                        "country"], perl = TRUE)
    tmp1 <- dic[dic$class %in% "country" & apply(is.na(dic[, 
                                                           2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\\\", "", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\(", "\\\\(", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\)", "\\\\)", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\[", "\\\\[", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\]", "\\\\]", names(tmp2), perl = TRUE)
    x1[, "country"] <- stringr::str_replace_all(x1[, "country"], 
                                                tmp2)
    tmp1 <- dic[dic$class %in% "country" & dic$condition2 %in% 
                  "not_is.na", ]
    reps <- unique(tmp1$replace)
    if (all(c("stateProvince", "municipality") %in% names(x1)) & 
        any(reps %in% unique(x1[, "country"]))) {
      reps1 <- reps[reps %in% unique(x1[, "country"])]
      for (i in 1:length(reps1)) {
        x1$country[is.na(x1[, "country"]) & !is.na(x1[, 
                                                      "municipality"]) & x1[, "stateProvince"] %in% 
                     tmp1$condition1[tmp1$replace %in% reps1[i]]] <- reps1[i]
      }
    }
  }
  if ("stateProvince" %in% loc.levels) {
    x1[, "stateProvince"] <- tolower(rmLatin(x1[, "stateProvince"]))
    pattern <- paste(missLocs, collapse = "|")
    x1[, "stateProvince"] <- gsub(pattern, NA, x1[, "stateProvince"], 
                                  perl = TRUE)
    x1[, "stateProvince"][grepl("desconhecid|unknown", x1[, 
                                                          "stateProvince"], perl = TRUE)] <- NA
    pattern <- paste(wordsForSearch, collapse = "|")
    x1[, "stateProvince"] <- gsub(pattern, "", x1[, "stateProvince"], 
                                  perl = TRUE)
    tmp1 <- dic[dic$class %in% "stateProvince" & !is.na(dic[, 
                                                            2]), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    cond0 <- unique(tmp1$condition0)
    if ("country" %in% names(x1)) {
      if (any(cond0 %in% unique(x1[, "country"]))) {
        cond1 <- cond0[cond0 %in% unique(x1[, "country"])]
        for (i in 1:length(cond1)) {
          tmp2.i <- tmp2[tmp1$condition0 %in% cond1[i]]
          x1[x1[, "country"] %in% cond1[i], "stateProvince"] <- stringr::str_replace_all(x1[x1[, 
                                                                                               "country"] %in% cond1[i], "stateProvince"], 
                                                                                         tmp2.i)
        }
      }
    }
  }
  if ("municipality" %in% loc.levels) {
    x1[, "municipality"] <- tolower(rmLatin(x1[, "municipality"]))
    pattern <- paste(missLocs, collapse = "|")
    x1[, "municipality"] <- gsub(pattern, NA, x1[, "municipality"], 
                                 perl = TRUE)
    x1[, "municipality"][grepl("desconhecid|unknown", x1[, 
                                                         "municipality"], perl = TRUE)] <- NA
    tmp1 <- dic[dic$class %in% "county" & apply(is.na(dic[, 
                                                          2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    x1[, "municipality"] <- stringr::str_replace_all(x1[, 
                                                        "municipality"], tmp2)
    tmp1 <- dic[dic$class %in% "locality1" & apply(is.na(dic[, 
                                                             2:4]), 1, all), ]
    tmp2 <- rep("", dim(tmp1)[1])
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\(", "\\\\(", names(tmp2), perl = TRUE)
    x1[, "municipality"] <- stringr::str_replace_all(x1[, 
                                                        "municipality"], tmp2)
  }
  if (any(c("locality") %in% loc.levels)) {
    x1[, "locality"] <- tolower(rmLatin(x1[, "locality"]))
    tmp1 <- dic[dic$class %in% "locality1" & apply(is.na(dic[, 
                                                             2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\(", "\\\\(", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\)", "\\\\)", names(tmp2), perl = TRUE)
    x1[, "locality"] <- stringr::str_replace_all(x1[, "locality"], 
                                                 tmp2)
    x1[, "locality"] <- gsub(" de de | de of ", " de ", 
                             x1[, "locality"], perl = TRUE)
    x1[, "locality"] <- gsub(" de do ", " do ", x1[, "locality"], 
                             perl = TRUE)
    tmp1 <- dic[dic$class %in% "locality2" & apply(is.na(dic[, 
                                                             2:4]), 1, all), ]
    tmp2 <- tmp1$replace
    names(tmp2) <- tmp1$pattern
    names(tmp2) <- gsub("\\.", "\\\\.", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\(", "\\\\(", names(tmp2), perl = TRUE)
    names(tmp2) <- gsub("\\)", "\\\\)", names(tmp2), perl = TRUE)
    x1[, "locality"] <- stringr::str_replace_all(x1[, "locality"], 
                                                 tmp2)
  }
  if (c("locality") %in% loc.levels & scrap) {
    n4 <- strsplit(x1[, "locality"], ",|\\.|:|\\(|\\)|;", 
                   perl = TRUE)
    n4 <- sapply(n4, function(x) gsub("^ | $", "", x, perl = TRUE))
    n4.2 <- as.character(sapply(n4, function(x) paste(unique(gsub("^ | $", 
                                                                  "", x[grepl("municipio|municipality|county|provincia|village", 
                                                                              x, perl = TRUE)], perl = TRUE)), collapse = "|")))
    n4.2 <- gsub("^ | $", "", n4.2, perl = TRUE)
    n4.2 <- gsub("municipio de |municipality of |municipio do |^county of |^provincia de|^provincia of|village of", 
                 "", n4.2, perl = TRUE)
    n4.2 <- gsub("municipio |municipality |^county |^provincia |village ", 
                 "", n4.2, perl = TRUE)
    n4.2[n4.2 %in% ""] <- NA
    n4.2.1 <- as.character(sapply(n4, function(x) x[1]))
    n4.2.1 <- gsub("^ | $", "", n4.2.1, perl = TRUE)
    n4.2.1[n4.2.1 %in% ""] <- NA
    locais <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|estrada|rodovia|road|^sitio|^mata|^horto|^jardim|campus|^pico|^serra|^sierra|^morro|^chapada"
    n4.3 <- as.character(sapply(n4, function(x) paste(unique(gsub("^ | $", 
                                                                  "", x[grepl(locais, x, perl = TRUE)], perl = TRUE)), 
                                                      collapse = ", ")))
    n4.3 <- gsub("^ | $", "", n4.3, perl = TRUE)
    n4.3[n4.3 %in% ""] <- NA
    n4.3[is.na(n4.3) & !is.na(x1[, "locality"])] <- as.character(sapply(n4[is.na(n4.3) & 
                                                                             !is.na(x1[, "locality"])], function(x) utils::head(x[!grepl("municipio|municipality|county|provincia|village", 
                                                                                                                                         x, perl = TRUE)], n = 1)))
    if (any(c("municipality", "county") %in% loc.levels)) {
      x1[, "municipality"][is.na(x1[, "municipality"]) & 
                             !is.na(n4.2)] <- gsub("^ | $", "", n4.2[is.na(x1[, 
                                                                              "municipality"]) & !is.na(n4.2)], perl = TRUE)
      x1[, "municipality"][is.na(x1[, "municipality"]) & 
                             is.na(n4.2)] <- gsub("^ | $", "", n4.2.1[is.na(x1[, 
                                                                               "municipality"]) & is.na(n4.2)], perl = TRUE)
    }
    x1[, "locality"][!is.na(n4.3)] <- n4.3[!is.na(n4.3)]
    x1[, "locality"][is.na(n4.3)] <- as.character(sapply(n4[is.na(n4.3)], 
                                                         function(x) x[1]))
  }
  for (i in 1:length(loc.levels)) x1[, i] <- as.character(gsub("^ | $", 
                                                               "", x1[, i], perl = TRUE))
  for (i in 1:length(loc.levels)) x1[, i] <- gsub("^-$", NA, 
                                                  x1[, i], perl = TRUE)
  tmp <- apply(x1[, loc.levels, drop = FALSE], 1, function(x) which(is.na(x))[1] - 
                 1)
  tmp[tmp %in% 0] <- length(loc.levels) + 1
  tmp[is.na(tmp)] <- length(loc.levels)
  resol.orig <- c(loc.levels, "no_info")[tmp]
  if (length(loc.levels) == 1) {
    res <- as.vector(x1[, 1])
    if (!to.lower) 
      res <- stringr::str_to_title(res)
  }
  else {
    names(x1) <- paste0(names(x1), ".new")
    res <- as.data.frame(x1)
    if (c("locality.new") %in% names(x1) & scrap) 
      res$locality.scrap <- n4.2.1
    if (!to.lower) {
      names.res <- names(res)
      for (i in 1:length(names.res)) res[, names.res[i]] <- stringr::str_to_title(res[, 
                                                                                      names.res[i]])
    }
    res$resol.orig <- resol.orig
  }
  result <- cbind.data.frame(x, res, stringsAsFactors = FALSE)
  return(result)
}

function (x, select.cols = c("loc", "loc.correct", "latitude.gazetteer", 
  "longitude.gazetteer", "resolution.gazetteer"), loc.levels = c("country", 
  "stateProvince", "municipality", "locality"), scrap = TRUE, 
  adm.names = c("country.new", "stateProvince.new", "municipality.new"), 
  loc.names = c("locality.new", "locality.scrap", "resol.orig"), 
  str.names = c("resol.orig", "loc.string", "loc.string1", 
    "loc.string2"), gazet = "plantR", gazet.names = c("loc", 
    "loc.correct", "latitude.gazetteer", "longitude.gazetteer", 
    "resolution.gazetteer"), orig.names = FALSE) 
{
  if (!class(x) == "data.frame") 
    stop("input object needs to be a data frame!")
  if (dim(x)[1] == 0) 
    stop("Input data frame is empty!")
  x1 <- fixLoc(x, loc.levels, scrap)
  locs <- strLoc(x1, adm.names, loc.names)
  locs$loc.string <- prepLoc(locs$loc.string)
  if ("loc.string1" %in% names(locs)) 
    locs$loc.string1 <- prepLoc(locs$loc.string1)
  if ("loc.string2" %in% names(locs)) 
    locs$loc.string2 <- prepLoc(locs$loc.string2)
  locs <- getLoc(x = locs, str.names, gazet, gazet.names, 
    orig.names)
  colunas <- select.cols
  colunas <- colunas[colunas %in% names(locs)]
  x1 <- cbind.data.frame(x1, locs[, colunas], stringsAsFactors = FALSE)
  return(x1)
}

formatLoc2 <- function (x, select.cols = c("loc", "loc.correct", "latitude.gazetteer", 
                                           "longitude.gazetteer", "resolution.gazetteer"), loc.levels = c("country", 
                                                                                                          "stateProvince", "municipality", "locality"), scrap = TRUE, 
                        adm.names = c("country.new", "stateProvince.new", "municipality.new"), 
                        loc.names = c("locality.new", "locality.scrap", "resol.orig"), 
                        str.names = c("resol.orig", "loc.string", "loc.string1", 
                                      "loc.string2"), gazet = "plantR", gazet.names = c("loc", 
                                                                                        "loc.correct", "latitude.gazetteer", "longitude.gazetteer", 
                                                                                        "resolution.gazetteer"), orig.names = FALSE) 
{
  if (!class(x) == "data.frame") 
    stop("input object needs to be a data frame!")
  if (dim(x)[1] == 0) 
    stop("Input data frame is empty!")
  x1 <- fixLoc2(x, loc.levels, scrap)
  locs <- strLoc(x1, adm.names, loc.names)
  locs$loc.string <- prepLoc(locs$loc.string)
  if ("loc.string1" %in% names(locs)) 
    locs$loc.string1 <- prepLoc(locs$loc.string1)
  if ("loc.string2" %in% names(locs)) 
    locs$loc.string2 <- prepLoc(locs$loc.string2)
  locs <- getLoc(x = locs, str.names, gazet, gazet.names, 
                 orig.names)
  colunas <- select.cols
  colunas <- colunas[colunas %in% names(locs)]
  x1 <- cbind.data.frame(x1, locs[, colunas], stringsAsFactors = FALSE)
  return(x1)
}
