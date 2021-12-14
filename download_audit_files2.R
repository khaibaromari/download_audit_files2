library(httr)
library(dplyr)
library(jsonlite)

form_name <- "ameM9v8nctyy8MRHMJfRX7"
output_dir <- "input/audit_files/"
user <- Sys.getenv("KOBO_USR")
pass <- Sys.getenv("KOBO_PSW")


# checking if the output directory is already available
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  if (dir.exists(output_dir)) {
    cat("Attention: The audit file directory was created in", output_dir,"\n")
  }
}

# checking if creating output directory was successful
if (!dir.exists(audit_dir))
  stop("download_audit_fils was not able to create the output directory!")
# checking if uuid column exists in data set
if (!uuid_column %in% names(df))
  stop("The column ", uuid_column, " is not available in data set.")
# checking if column audit_URL exists in data set
if (!uuid_column %in% names(df))
  stop("The column ", uuid_column, " is not available in data set.")
if (!"audit_URL" %in% names(df))
  stop("Error: the column audit_URL is not available in data set.")

available_audits <- dir(output_dir)

# getting form url
form_url <- GET("https://kc.humanitarianresponse.info/api/v1/data", authenticate(user, pass), timeout(1000)) %>% 
  content(., "text", encoding = "UTF-8") %>%
  fromJSON(., flatten = T) %>% 
  filter(id_string == form_name) %>% 
  select(url) %>% 
  unlist()

# getting attachment links of uuids that their audit is already downloaded
form_data <- GET(form_url, authenticate(user, pass), timeout(1000)) %>% 
  content(., "text", encoding = "UTF-8") %>%
  fromJSON(., flatten = T) %>% 
  as.data.frame() %>% 
  filter(!`_uuid` %in% available_audits)

attachment_url <- form_data[["_attachments"]]
audit_file_links <- unlist(attachment_url)[names(unlist(attachment_url)) == "download_url"] %>% unname()

download_audit <- function(audit_link, user, pass, output_dir) {
  audit_file <- GET(audit_link, authenticate(user, pass), timeout(1000), progress()) %>% 
    content(., "text", encoding = "UTF-8") %>% 
    fromJSON(., flatten = T)
  
  uuid <- 
  output_file_name <- paste0(output_dir, )
  dir.create(output_file_name)
  write.csv(audit_file, output_dir)
}

sapply(audit_file_links, download_audit)


