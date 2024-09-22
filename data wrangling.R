f <- function(a, b) {
  return(a^2 + b)
}
print(f(3,1))
print(1 %>% f(3))

penguinsv2 %>% mutate(filter_bill_ratio=flipper_length_mm / bill_length_mm)
penguinsv2 %>% rename(f_l_m = flipper_length_mm)

penguinsv2 %>% arrange(desc(bill_length_mm))

penguinsv2 %>% summarize(num_rows=n(), avg_weight_kg=mean(body_mass_g/1000, 
              na.rm = TRUE), 
              avg_flipper_bill_ratio=mean(flipper_length_mm/bill_length_mm,
              na.rm = TRUE))

penguinsv2 %>% 
  group_by(species) %>%
  summarize(num_rows=n(), avg_weight_kg=mean(body_mass_g/1000, 
                                                          na.rm = TRUE), 
                         avg_flipper_bill_ratio=mean(flipper_length_mm/bill_length_mm,
                                                     na.rm = TRUE))

Num_NAs <- penguinsv2 %>% summarise(species=sum(is.na(species)), 
                                    bill_length_mm=sum(is.na(bill_length_mm)),
                                    body_mass_g=sum(is.na(body_mass_g)),
                                    flipper_length_mm=sum(is.na(flipper_length_mm)))
Num_NAs

Num_NAs2 <- penguinsv2 %>% summarise(across(everything(), 
                                            ~sum(is.na(.x))))
Num_NAs2

penguinsv2 %>% summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

penguinsv2 %>%
  select(-bill_length_mm) %>%
  group_by(species) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

penguins_bill_length_df <- penguinsv2 %>%
  arrange(desc(bill_length_mm)) %>%
  select(species, bill_length_mm)

penguins_bill_length_df

species <- unique(penguinsv2$species)
species

latin_name <- c('Pygoscelis adeliae', 'Pygoscelis papua', 'Pygoscelis antar')
latin_name_df <- data.frame(species, latin_name)
latin_name_df

inner_join(penguins_bill_length_df, latin_name_df)
full_join(penguins_bill_length_df, latin_name_df)
