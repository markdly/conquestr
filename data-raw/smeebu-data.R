
# library(jsonlite)
# library(tidyverse)
# getwd()
# smeebu_path <- "../smeebu/firebase-2017-03/crackling-heat-5124-export.json"
#
# smeebu_json <- read_file(smeebu_path)
#
# # NB: this takes a while
# # each element of the list is one of the top level objects in firebase
# smeebu_list <- fromJSON(smeebu_json)
# names(smeebu_list)
#
# # example user results
# head(names(smeebu_list$userResults[[3]]))
# smeebu_list$userResults[[1098]]
#
# # or look at the user responses (long format)
# smeebu_list$responses[[998]]
#
# # NB: this takes a while
# resps <- smeebu_list$responses %>% map_df(as_tibble)
# save(resps, file = "smeebu_responses.RData")

library(tidyverse)
library(stringr)

load("../R-general/smeebu_responses.RData")

smeebu_test_long <- resps %>%
  filter(str_detect(testname, "Grammar-test-7A-001")) %>%
  mutate(item = dense_rank(itemID)) %>%
  mutate(pid = str_c("1", str_pad(dense_rank(userID), 4, "left", pad = "0"))) %>%
  group_by(item) %>%
  mutate(resp = dense_rank(response)) %>%
  select(pid, item, resp, itemscore)

short_test <- smeebu_test_long %>% ungroup() %>%
  select(-itemscore) %>%
  distinct(pid, item, .keep_all = TRUE) %>%
  mutate(item = str_c("q", item)) %>%
  spread(item, resp)

short_test_score_key <- smeebu_test_long %>%
  filter(itemscore == 1) %>%
  distinct(item, resp)


devtools::use_data(short_test_score_key, overwrite = TRUE)
devtools::use_data(short_test, overwrite = TRUE)


# cq fixed width data file for analysis
data(short_test)
data(short_test_score_key)

item_key <- short_test_score_key %>%
  arrange(item) %>%
  .$resp %>%
  paste0(., collapse = "")

short_test %>%
  mutate_all(as.character) %>%
  mutate_all(funs(if_else(is.na(.), "9", .))) %>%
  unite(cq_resp, q1:q9, sep = "") %>%
  select(cq_resp, pid) %>%
  as.data.frame() %>%
  gdata::write.fwf("./inst/extdata/ex1.txt", colnames = FALSE, formatInfo = TRUE) %>%
  write_csv("./inst/extdata/ex1_dict.csv")

# create cq label file
# Using writeLines. readr::write_lines should work when new version of readr is on CRAN
# https://github.com/tidyverse/readr/issues/665
writeLines(c("===> item", paste0(1:9, " q", 1:9)), "./inst/extdata/ex1.lab")


# create cq syntax file
cq_name <- "ex1"
cqc <- glue::glue("/*  example analysis  */",
                  "reset;",
                  "export logfile >> {cq_name}.cql;",
                  "Title {cq_name};   ",
                  "data {cq_name}.txt;",
                  "format pid 11-15 responses 1-9;",
                  "codes 1,2,3,4,5,9;",
                  "label << {cq_name}.lab;",
                  "key {item_key} ! 1;",
                  "model item;",
                  "estimate ! nodes = 30;",
                  "show !estimate=latent >> {cq_name}.shw;",
                  "itanal >> ex1.itn;", .sep = "\r\n")
cqc
write_file(cqc, "./inst/extdata/ex1.cqc")

