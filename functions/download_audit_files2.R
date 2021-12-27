download_audit_files_2 <- function(form_id, output_dir, user, pass) {
  if (!"stringr" %in% installed.packages()) 
    stop("Could not find the package stringr!")
  
  if (!"dplyr" %in% installed.packages()) 
    stop("Could not find the package dplyr!")
  
  if (!"httr" %in% installed.packages()) 
    stop("Could not find the package httr!")
  
  if (!"jsonlite" %in% installed.packages()) 
    stop("Could not find the package jsonlite!")
  
  if (is.na(output_dir) || output_dir == "") 
    stop("The path for storing audit files can't be empty!")
  
  if (is.na(user) || user == "") 
    stop("Username can't be empty!")
  
  if (is.na(pass) || pass == "") 
    stop("Password can't be empty!")
  
  require(httr)
  require(dplyr)
  require(jsonlite)
  require(stringr)
  
  # checking if the output directory is already available
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
    if (dir.exists(output_dir)) {
      cat("Attention: The audit file directory was created in", output_dir,"\n")
    }
  }
  
  # checking if creating output directory was successful
  if (!dir.exists(output_dir))
    stop("download_audit_fils was not able to create the output directory!")
  
  # listing audit files that are already available
  available_audits <- dir(output_dir)
  
  # getting form url
  form_url <- GET("https://kc.humanitarianresponse.info/api/v1/data", authenticate(user, pass), timeout(1000)) %>% 
    content(., "text", encoding = "UTF-8") %>%
    fromJSON(., flatten = T) %>% 
    filter(id_string == form_id) %>% 
    select(url) %>% 
    unlist()
  
  if (length(form_url) > 0) {
    
    # getting attachment links of uuids that their audit is already downloaded
    form_data <- GET(form_url, authenticate(user, pass), timeout(1000)) %>% 
      content(., "text", encoding = "UTF-8") %>%
      fromJSON(., flatten = T) %>% 
      as.data.frame() %>% 
      filter(!`_uuid` %in% available_audits)
    
    attachment_url <- form_data[["_attachments"]]
    audit_file_links <- unlist(attachment_url)[names(unlist(attachment_url)) == "download_large_url"] %>% unname()
    
    # uuids <- form_data[["_uuid"]][lapply(attachment_url, length) %>% unlist() != 0]
    # uuids <- str_extract(audit_file_links, "(?<=%2F)[a-z-0-9]*(?=%2Faudit.csv$)")
    # names(audit_file_links) <- uuids
    
    download_audit <- function(audit_link, user, pass, output_dir, uuid) {
      uuid <- str_extract(audit_link, "(?<=%2F)[a-z-0-9]*(?=%2Faudit.csv$)")
      audit_file <- GET(audit_link, authenticate(user, pass), timeout(1000), progress()) %>% 
        content(., "text", encoding = "UTF-8")
      
      cat("Downloading audit file for", uuid, "\n")
      dir.create(paste0(output_dir, "/", uuid))
      output_file_name <- paste0(output_dir, "/", uuid,"/audit.csv")
      # write.csv(audit_file, output_file_name)
      
      if (!is.na(audit_file)) {
        if (length(audit_file) > 2) {
          write.csv(audit_file, output_file_name, row.names = F)
        }else if(!audit_file == "Attachment not found"){
          if (grepl("[eventnodestartend]", audit_file)) {
            write.table(audit_file, output_file_name, row.names = F, col.names = FALSE, quote = F)
          }
        }
      }
    }
    
    downloaded_uuids <- sapply(audit_file_links, download_audit, user = user, pass = pass, output_dir = output_dir)
  } else{
    cat("Attention: All audit files for given form is downloaded!")
  }
}

