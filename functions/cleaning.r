# Cleans column names, removes empty rows, removes columns called comment and delta

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

# Subset the data to only include the penguins that are not NA for the culmen depth, as well as filtering for the Gentoo species  

remove_empty_culmen_depth <- function(data_clean){
  data_clean %>%
    filter(!is.na(culmen_depth_mm)) %>%
    select(species, culmen_depth_mm, body_mass_g) %>% 
    filter(species == "Gentoo penguin (Pygoscelis papua)")
}
