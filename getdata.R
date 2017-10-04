
## website:  Blognone.com #
library(plyr)
library(rvest)
library(curl)
library(methods)

path <- ""
setwd(path)

#### function ####

scrap.webdata <- function (url, round) {
  
  #get  loop round
  i <- round
  
  # Get Webpage Data
  #node[[i]] <- read_html(iconv(url, from = "UTF-8", to = "UTF-8"), encoding = "utf8")
  readurl <- tryCatch({read_html(curl(url, handle = new_handle("useragent" = "Mozilla/5.0")))}, 
                         error=function(err) "Error 403")
  
  
  if ("Error 403" %in% readurl == FALSE){
    
    
    title <- html_text(html_nodes(readurl, css = ".content-title-box"))
    
    
    tag.x <- html_nodes(readurl, css = ".field-item")
    tag <- html_attr(html_nodes(tag.x, "a"), "href")
    
    
    content <- html_text(html_nodes(readurl, css = ".node-content"))
    
    author <- html_text(html_nodes(readurl, css = ".submitted"))
    
    
    
    #comments user (all include submitted meta user)
    comment <- html_text(html_nodes(readurl, css = ".username"))
    
    
    # Comment number
    comment_tar <- html_text(html_nodes(readurl, css = ".comment-target"))
    
    # Comment Meta & User's comment
    comment_info <- html_text(html_nodes(readurl, css = ".comment-info"))
    
    # Comment content
    comment_con <- html_text(html_nodes(readurl, css = ".comment-content"))
    
    
    
    
    #comment info (all user)
    comment_conx <- html_text(html_nodes(readurl, css = ".comment-info"))
    
    
    
    
    lst <- list(title, tag, content, author, comment, comment_tar, comment_info, comment_con)
    
    
    
  }else{
    
    lst <- "Error 403"
    
  }
  
  
  return(lst)
  
}


#### Target url ####
### url structure -- https://www.blognone.com/node/90000

## there are 90,009 nodes (12 Feb 2017)
node_num <- 1:90009

## Pre-allocate list
node <- vector(mode = "list", length = max(node_num))


## Pause every X loop
pause <- seq(1, length(node_num), by= 100)

for (i in 1:length(node_num)) {
  
  url <- paste0("https://www.blognone.com/node/", i)
  
  
  
  node[[i]] <- scrap.webdata(url, i)
  
  
  ### Display Status ###
  # Get Data Size
  ns <- object.size(node)
  
  # Print
  cat("\r", paste0("Downloading:", i, "/", length(node_num)), 
      "--size", format(object.size(node), quote = FALSE, units = "KB"))
  
  #rest every X time
  #rest every X time
  if(i %in% pause == TRUE){
    
    #Sys.sleep(1)
    save(node, file = paste0(path, "BN_Webdat.Rdata")) #save backup
    
  }
  
  
  # Export data when finish (mean while you should go to bed)
  if (i == length(node_num)) {save(node, file = paste0(path, "BN_Webdat.Rdata"))}
  
}
