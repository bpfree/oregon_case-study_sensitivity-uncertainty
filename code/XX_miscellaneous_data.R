## Miscellaneous code

# X - PACPARS

d13_coastal_table <- data.frame(table[[2]]) %>%
  # clean table
  janitor::row_to_names(row_number = 2,
                        # remove row after setting as column names
                        remove_row = TRUE,
                        # remove above rows
                        remove_rows_above = TRUE) %>%
  # clean column names
  janitor::clean_names() %>%
  # place NA into cell in row 5, column 6
  dplyr::na_if(.[5,6]) %>%
  # place NA into cell in row 6, column 6
  dplyr::na_if(.[6,6]) %>%
  # split out column to be two columns
  tidyr::separate(latitude_longitude, into=c("latitude2", "longitude2"), sep=" ", remove=T, convert = T) %>%
  # split out column to be two columns
  tidyr::separate(latitude_longitude_2, into=c("latitude3", "longitude3"), sep=" ", remove=T, convert = T) %>%
  # split the data across columns by 3 (point, lat, lon)
  split.default(., (seq_along(.)-1) %/% 3) %>%
  # recombine so in long format (all features share same fields)
  dplyr::bind_rows()

