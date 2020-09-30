preprocList<-list(
  onNewRow= function( pt, context, WH, keys){
  value <- getAttrValue()
  value <- paste0(sample(letters, 8, replace = TRUE), collapse = "")
  tibs <- setAttrValue(value = value, context = context)
  tibs
  },
  onChangeRow= function( pt, context, WH, keys){
  value <- getAttrValue()
  if (!is.null(keys$keycode)) {
      tib <- context$tibs[["links"]]
      row <- getLastRow(tib)
      if (keys$keycode == 65) { # key 'a' depressed
          row$fromId <- value
          tib <- appendLastRow(tib, row)
      }
      if (keys$keycode == 66) { # key 'b' depressed
          row$toId <- value
          tib <- replaceLastRow(tib, row)
      }
      context$tibs[["links"]] <- tib
      tibs <- context$tibs
  } else {
      tibs <- setAttrValue(value = value, context = context)
  }
  tibs
  }
)
