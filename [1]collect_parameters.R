library(tidyverse)
library(rvest)

prospectus_rcp_no <- read_csv("input/rcp_no.csv", col_types = "cn")

# Let's make the function.
url_parameter_collect <- function(firm, rcp_no){
  Sys.sleep(2)
  # Not to be blocked from DART, 2-seconds break per one access is necessary.
  print(paste0("This function is now dealing with firm ", firm))
  # To check whether our function is running during the loop.
  
  url <- paste0("http://dart.fss.or.kr/dsaf001/main.do?rcpNo=", rcp_no)
  html <- read_html(url)
  
  text <- html %>%
    html_nodes("script") %>%
    html_text() %>%
    str_replace_all("\\s+", " ") %>%
    str_split("\\//") %>%
    unlist()
  
  tree_mask <- str_detect(text, "treeNode\\d{1} \\= new Tree\\.TreeNode")
  tree <- text[tree_mask]
  
  section_regex <- regex("(?<=text: \")[[:print:]]+?(?=\"\\,)")
  section <- tree %>%
    str_extract_all(section_regex) %>%
    unlist()
  
  params_regex <- regex("(?<=\\{viewDoc\\()[[:print:]]+?(?=\\))")
  params <- tree %>%
    str_extract_all(params_regex) %>%
    unlist() %>%
    str_replace_all("'", "") %>%
    str_trim("both")
  
  data_frame("id" = firm,
             "section" = section,
             "params" = params) %>%
    separate(params, into = c("rcpNo", "dcmNo", "eleId", "offset", "length", "dtd"), sep=", ")
}

prospectus_url_parameter <- map2_dfr(prospectus_rcp_no$id, prospectus_rcp_no$prospectus, url_parameter_collect)
#print(prospectus_url_parameter)

# Now you collect parameters for every section in every document.

write_csv(prospectus_url_parameter, "temp/prospectus_url_parameter.csv")
