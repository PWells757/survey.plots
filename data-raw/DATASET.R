## code to prepare `DATASET` dataset goes here
Soton <- sf::st_read(dsn = "Local_authority_districts_Soton", layer = "Soton")
usethis::use_data(Soton)
