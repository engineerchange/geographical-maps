library(tidyverse)
library(readxl)
library(janitor)

rm(list=ls())

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# https://data.census.gov/cedsci/table?g=0400000US24%246200000&y=2020&tid=DECENNIALPL2020.P1
race = read_csv("data/MD-RACE.csv")

# https://data.census.gov/cedsci/table?g=0400000US24%246200000&y=2020&tid=DECENNIALPL2020.P2
hisp = read_csv("data/MD-HISPANIC.csv")

# https://data.census.gov/cedsci/table?g=0400000US24%246200000&y=2020&tid=DECENNIALPL2020.P3
adult = read_csv("data/MD-OVER18.csv")

# https://elections.maryland.gov/press_room/2020_stats/Eligible%20Active%20Voters%20by%20Legislative%20-%20PG20.xlsx
voters = read_excel("data/Eligible Active Voters by Legislative - PG20.xlsx")
voters = voters %>% slice(6:nrow(.))
names(voters) = voters %>% slice(1) %>% unlist()
voters = voters %>% clean_names() %>% select(-starts_with("na"))
voters = voters %>% slice(2:nrow(.))



district = c("district 31B,")
district2 = c("31B")
pattern = paste(district, collapse="|")
pattern2 = paste(district2, collapse="|")
voters = voters %>% dplyr::filter(grepl(pattern2,district))

#names(race) = race %>% slice(1) %>% unlist()
race = race %>% janitor::clean_names()
race = race %>% dplyr::filter(!is.na(total))
race_df = race %>% dplyr::filter(grepl(pattern,label_grouping))

# check filters
cat("Filter 1\n")
race %>% dplyr::filter(grepl(pattern,label_grouping)) %>%
  select(label_grouping)
cat("Filter 2\n")
voters %>% dplyr::filter(grepl(pattern2,district)) %>%
  select(district)

voters %>% mutate(total=as.numeric(dem)) %>% summarise(sum=sum(total)) -> total_dem;total_dem

voters %>% mutate(total=as.numeric(rep)) %>% summarise(sum=sum(total)) -> total_rep;total_rep

voters %>% mutate(total=as.numeric(una)) %>% summarise(sum=sum(total)) -> total_unaffil;total_unaffil

voters %>% mutate(total=as.numeric(total)) %>% summarise(sum=sum(total)) -> total_reg;total_reg

voters %>% mutate(total=as.numeric(oth)) %>% summarise(sum=sum(total)) -> total_other;total_other

total_unaffil/total_reg -> ratio_unaffil;ratio_unaffil

total_rep/total_reg -> ratio_rep;ratio_rep

total_dem/total_reg -> ratio_dem;ratio_dem

total_other/total_reg -> ratio_other;ratio_other

adult = adult %>% janitor::clean_names()
adult = adult %>% dplyr::filter(!is.na(total))
adult_df = adult %>% dplyr::filter(grepl(pattern,label_grouping))

hisp = hisp %>% janitor::clean_names()
hisp = hisp %>% dplyr::filter(!is.na(total))
hisp_df = hisp %>% dplyr::filter(grepl(pattern,label_grouping))

# total pop
race_df %>% mutate(total=as.numeric(total)) %>% summarise(sum=sum(total)) -> total_pop;total_pop

# voting age
adult_df %>% mutate(total=as.numeric(total)) %>% summarise(sum=sum(total)) -> total_votingage;total_votingage

# ratio
total_votingage/total_pop -> votingage_ratio;votingage_ratio

# white pop
race_df %>% mutate(total=as.numeric(total_population_of_one_race_white_alone)) %>% summarise(sum(total)) -> total_whitepop;total_whitepop

# ratio
total_whitepop/total_pop -> white_ratio;white_ratio

# black pop
race_df %>% mutate(total=as.numeric(total_population_of_one_race_black_or_african_american_alone)) %>% summarise(sum(total)) -> total_blackpop;total_blackpop

# ratio
total_blackpop/total_pop -> black_ratio;black_ratio

# native american pop
race_df %>% mutate(total=as.numeric(total_population_of_one_race_american_indian_and_alaska_native_alone)) %>% summarise(sum(total)) -> total_nativepop;total_nativepop

# ratio
total_nativepop/total_pop -> native_ratio;native_ratio

# asian pop
race_df %>% mutate(total=as.numeric(total_population_of_one_race_asian_alone)) %>% summarise(sum(total)) -> total_asianpop;total_asianpop

# ratio
total_asianpop/total_pop -> asian_ratio;asian_ratio

# pacific pop
race_df %>% mutate(total=as.numeric(total_population_of_two_or_more_races_population_of_two_races_white_native_hawaiian_and_other_pacific_islander)) %>% summarise(sum(total)) -> total_pacificpop;total_pacificpop

# ratio
total_pacificpop/total_pop -> pacific_ratio;pacific_ratio


# other pop
race_df %>% mutate(total=as.numeric(total_population_of_one_race_some_other_race_alone)) %>% summarise(sum(total)) -> total_otherpop;total_otherpop

# ratio
total_otherpop/total_pop -> other_ratio;other_ratio

# 2 or more pop
race_df %>% mutate(total=as.numeric(total_population_of_two_or_more_races)) %>% summarise(sum(total)) -> total_twomorepop;total_twomorepop

# ratio
total_twomorepop/total_pop -> twomorepop_ratio;twomorepop_ratio

# hisp pop
hisp_df %>% mutate(total=as.numeric(total_hispanic_or_latino)) %>% summarise(sum=sum(total)) -> total_hisp;total_hisp

# ratio
total_hisp/total_pop -> hisp_ratio;hisp_ratio

tribble(
  ~Item,~Stat,~Ratio,
  "Pop",total_pop %>% pull(),1,
  "Voting Age",total_votingage %>% pull(),round2(votingage_ratio %>% pull() * 100,1),
  "White",total_whitepop %>% pull(),round2(white_ratio %>% pull() * 100,1),
  "Black",total_blackpop %>% pull(),round2(black_ratio %>% pull() * 100,1),
  "Native",total_nativepop %>% pull(),round2(native_ratio %>% pull() * 100,1),
  "Asian",total_asianpop %>% pull(),round2(asian_ratio %>% pull() * 100,1),
  "Pacific",total_pacificpop %>% pull(),round2(pacific_ratio %>% pull() * 100,1),
  "Other",total_otherpop %>% pull(),round2(other_ratio %>% pull() * 100,1),
  "Two Or More",total_twomorepop %>% pull(),round2(twomorepop_ratio %>% pull() * 100,1),
  "Hisp",total_hisp %>% pull(),round2(hisp_ratio %>% pull() * 100,1),
  "Registered",total_reg %>% pull(),1,
  "Unaffil",total_unaffil %>% pull(),round2(ratio_unaffil %>% pull() * 100,1),
  "Rep",total_rep %>% pull(),round2(ratio_rep %>% pull() * 100,1),
  "Dem",total_dem %>% pull(),round2(ratio_dem %>% pull() * 100,1),
  "Other",total_other %>% pull(),round2(ratio_other %>% pull() * 100,1)
)

tribble(
  ~Item,~Stat,
  "Dem",round2(ratio_dem %>% pull() * 100,1),
  "Rep",round2(ratio_rep %>% pull() * 100,1),
  "Unaffil",round2(ratio_unaffil %>% pull() * 100,1),
  "White",round2(white_ratio %>% pull() * 100,1),
  "Black",round2(black_ratio %>% pull() * 100,1),
  "Native",round2(native_ratio %>% pull() * 100,1),
  "Asian",round2(asian_ratio %>% pull() * 100,1),
  "Pacific",round2(pacific_ratio %>% pull() * 100,1),
  "Other",round2(other_ratio %>% pull() * 100,1),
  "Two More",round2(twomorepop_ratio %>% pull() * 100,1),
  "Hisp",round2(hisp_ratio %>% pull() * 100,1),
  "Pop",total_pop %>% pull(),
  "Voting Age",total_votingage %>% pull(),
  "Registered",total_reg %>% pull()
)

