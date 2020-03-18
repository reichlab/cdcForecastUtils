

format_entry_bins <- function(entry_bins_unformatted,target){
  if (grepl("wk ahead",target) | grepl("Peak height",target)){
    entry_bins <- unlist(lapply(entry_bins_unformatted,function(x){
      if (!is.na(x)){
        if (nchar(x) <=2){
          return (paste0(x,".0"))
        }else{
          return (x)
        }
      }else{
        return (x)
      }
    }))
  } else if (grepl("Below baseline for 3 weeks",target)){
    entry_bins <- tolower(entry_bins_unformatted)
  } else{
    entry_bins <- entry_bins_unformatted
  }
  return (entry_bins)
}