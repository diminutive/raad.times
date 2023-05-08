
files <- raadfiles::oisst_daily_files() |> dplyr::transmute(date,
                                                            file = stringr::str_replace(fullname, sprintf("%s/", root), ""))

usethis::use_data(files, overwrite = TRUE)
