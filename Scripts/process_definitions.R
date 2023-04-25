# ------------------------------------------------------------------------------
# Definitions of process criteria:
# * years
# * breaks and labels for data
# * titles for maps and figures
# ------------------------------------------------------------------------------

# Alligator
gator_varname <- "Habitat_Suitability"
gator_years <- paste0("X", c("1978", "1989", "1995"))
gator_year_labels <- c("Average Year (1978)",
                       "Dry Year (1989)",
                       "Wet Year (1995)")

gator_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
gator_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                      "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                      "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90",
                      "0.91 - 1.00")

gator_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
gator_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                       "-0.101 to -0.325", "0.099 to -0.100", "0.100 to 0.324",
                       "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
gator_title <- "Alligator Habitat Suitability Index"


# Snail Kite
snki_varname <- "SNKI_HSI"
snki_years <- paste0("X", c("1989.04.20" , "1995.04.20"))
snki_year_labels <- c("Dry Year (April 20, 1989)", "Wet Year (April 20, 1995)")

snki_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
snki_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                     "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                     "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90", "0.91 - 1.00")

snki_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
snki_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                      "-0.101 to -0.325", "0.099 to -0.100", "0.100 to 0.324",
                      "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
snki_title <- "Snail Kite Nesting Relative Selection"


# Days Since Dry
dsd_varname <- "dsd"
dsd_years <- paste0("X", c("1978.06.01", "1989.06.01", "1995.06.01"))
dsd_year_labels <- c("Average Year (June 1, 1978)", "Dry Year (June 1, 1989)",
                     "Wet Year (June 1, 1995)")

dsd_ind_cuts <- c(0.0, 110,  219,  329,  438,  548,  657,  767,  876,  986, Inf)
dsd_ind_labels <- c("0 - 109", "110 - 218", "219 - 328", "329 - 437",
                    "438 - 547", "548 - 656", "657 - 766", "767 - 875",
                    "876 - 985", "986 - 1,095+")

dsd_diff_cuts <- seq(from = -1095, to = 1095, length.out = 10)
dsd_diff_cuts <- c(-Inf, -852, -608, -365, -122, 122, 365, 608, 852, Inf)
dsd_diff_labels <- c("-853 to -1,095+", "-609 to -852", "-366 to -608",
                     "-123 to -365", "121 to -122", "122 to 364", "365 to 607",
                     "608 to 851", "852 to 1,095+") 
dsd_title <- "Days Since Drydown"


# Apple Snail
apsn_varname <- "snailPopulationAdults"
apsn_years <- paste0("X", c("1989.04.20" , "1995.04.20"))
apsn_year_labels <- c("Dry Year (April 20, 1989)", "Wet Year (April 20, 1995)")

apsn_ind_cuts <- c(-Inf, 1, 15001, 30001, 45001, 60001, 75001, 90001,
                   105001, 120001, Inf)
apsn_ind_labels <- c("0", "1 - 15,000", "15,001 - 30,000", "30,001 - 45,000",
                     "45,001 - 60,000", "60,001 - 75,000", "75,001 - 90,000",
                     "90,001 - 105,000", "105,001 - 120,000",
                     "120,001 - 140,000+")

apsn_diff_cuts <- c(-140000, -107500, -75000, -42500, -10000, 10001, 42501,
                    75001, 107501, 140000)
apsn_diff_labels <- c("-107,501 to -140,000", "-75,001 to -107,500",
                      "-42,501 to 75,000", "-10,001 to -42,500",
                      "10,000 to -10,000", "10,001 to 42,500",
                      "42,501 to 75,000", "75,001 to 107,500",
                      "107,501 to 140,000")
apsn_title <- "Adult Apple Snail Population"


# EverWaders
waders_varname <- "_Occupancy"
waders_years <- paste0("X", c("1978", "1989", "1995"))
waders_year_labels <- c("Average Year (1978)", "Dry Year (1989)",
                        "Wet Year (1995)")

waders_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
waders_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                       "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                       "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90",
                       "0.91 - 1.00")

waders_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
waders_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                        "-0.101 to -0.325", ".099 to -0.100", "0.100 to 0.324",
                        "0.325 to 0.549", ".550 to .774", "0.775 to 1") 

waders_sp_abr <- c("GBHE", "GLIB", "GREG", "LBHE", "ROSP", "WHIB", "WOST")
waders_sp_name <- c("Great Blue Heron", "Glossy Ibis", "Great Egret",
                    "Little Blue Heron", "Roseate Spoonbill",
                    "White Ibis", "Wood Stork")
names(waders_sp_abr) <- waders_sp_name

# Marl Prairie
marl_ind_cuts <- seq(from = 0, to = 100, by = 10)
marl_ind_labels <- c("0 - 10%", "11 - 20%", "21 - 30%",
                     "31 - 40%", "41 - 50%", "51 - 60%",
                     "61 - 70%", "71 - 80%", "81 - 90%", "91 - 100%")

marl_diff_cuts <- c(-100, -78, -55, -33, -10, 11, 34, 56, 79, 100)
marl_diff_labels <- c("-79 to -100", "-56 to -78", "-34 to -55",
                      "-11 to -33", "10 to -10", "11 to 33",
                      "34 to 55", "56 to 78", "79 to 100") 
marl_title <- "Marl Prairie Score: Percent to Target"

