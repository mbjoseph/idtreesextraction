
library(idtreesextraction)
library(tidyverse)
library(sf)

# We need to crop the train and test data for every site. 
combos <- tibble(site = c("MLBS", "OSBS", "MLBS", "OSBS", "TALL"), 
                 group = c("train", "train", "test", "test", "test"))

train_data <- st_read("data/train/ITC/train_MLBS.shp") %>%
  rbind(st_read("data/train/ITC/train_OSBS.shp")) %>%
  left_join(read_csv("data/train/Field/train_data.csv"))


for (i in 1:nrow(combos)) {
  crop_rgb(site = combos$site[i], group = combos$group[i])
}

# Move the training data into species specific directories -----------


unlink("out/train/rgb_chips")
train_chips <- tibble(path = list.files(path = "out/train", pattern = "*.jpg$", 
                                        recursive = TRUE, full.names = TRUE)) %>%
  mutate(indvdID = basename(path), 
         indvdID = gsub(pattern = ".jpg", "", indvdID))

train <- train_data %>%
  left_join(train_chips) %>%
  mutate(new_dir = here::here("out", "train", "rgb_chips", taxonID, 
                              paste0(indvdID, ".jpg"))) %>%
  filter(!is.na(taxonID))


dir.create("out/train/rgb_chips")
for (i in 1:nrow(train)) {
  dir.create(dirname(train$new_dir[i]), showWarnings = FALSE, recursive = TRUE)
  file.copy(train$path[i], train$new_dir[i], overwrite = TRUE)
}


# Move the test data into one dir -----------------------------------------

unlink("out/test/rgb_chips")
test <- tibble(path = list.files(path = "out/test", pattern = "*.jpg$", 
                                 recursive = TRUE, full.names = TRUE)) %>%
  mutate(indvdID = basename(path), 
         indvdID = gsub(pattern = ".jpg", "", indvdID), 
         new_path = here::here("out", "test", "rgb_chips", basename(path)))

dir.create("out/test/rgb_chips")
for (i in 1:nrow(test)) {
  file.copy(test$path[i], test$new_path[i], overwrite = TRUE)
}



# Extract the hyperspectral data ------------------------------------------

hs_test <- combos %>%
  filter(group == "test") %>%
  pmap(extract_hs) %>%
  bind_rows

hs_train <- combos %>%
  filter(group == "train") %>%
  pmap(extract_hs) %>%
  bind_rows %>%
  left_join(distinct(as_tibble(train_data), indvdID, taxonID)) %>%
  distinct(indvdID, .keep_all = TRUE) %>%
  filter(hs_train$indvdID %in% train_data$indvdID)

write_csv(hs_test, "out/hyperspectral_test.csv")
write_csv(hs_train, "out/hyperspectral_train.csv")
