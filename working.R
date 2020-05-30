library(dplyr)
library(sunburstR)
library(d3r)
library(tidyr)
library(stringr)
library(treemap)
library(highcharter)
# Prepare data

gdp_df <- read.csv("gdp-per-worker.csv", header=T)
employment_df <- read.csv("employment.csv", header=T)
output_mult_df <- read.csv("output-multiplier.csv", header=T)
value_added_mult_df <- read.csv("value-added-multiplier.csv", header=T)

names(gdp_df) <- c("sectors", "units", "y2015", "y2016", "y2017", "y2018", "y2019")
names(employment_df) <- c("sector", "units", "y2015","y2016","y2017","y2018", "y2019")

sector_categories <- levels(gdp_df$sectors)
multiplier_categories <- levels(output_mult_df$Sector)


# prep some sample data with sampled categories and multiplier categories
set.seed(20)
sample_cat <- sample(sector_categories,3)
sample_mult <- sample(multiplier_categories,3)
sample_worker_affected <- sample(seq(100,10000,by=50),3)
sample_period_impairment <- sample(seq(10,200, by=10),3)

df <- cbind(sample_cat, sample_mult, sample_worker_affected, sample_period_impairment)
df <- data.frame(df, stringsAsFactors = F)
names(df) <- c("Sector", "MultiplierSector", "no_of_workers_affected", "period_of_impairment")


# Get initial dataframe

df <- df %>% 
  mutate(gdp_per_worker = pull(subset(gdp_df, sectors %in% df[["Sector"]], select = c('y2019')))) %>%
  mutate(employment = pull(subset(employment_df, sector %in% df[["Sector"]], select = c("y2019")))*1000) %>%
  mutate(mult_direct = pull(subset(output_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Direct.impact")))) %>%
  mutate(mult_indirect = pull(subset(output_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Indirect.impact")))) %>%
  mutate(mult_induced = pull(subset(output_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Induced.impact")))) %>%
  mutate(va_mult_direct = pull(subset(value_added_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Direct.impact")))) %>%
  mutate(va_mult_indirect = pull(subset(value_added_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Indirect.impact")))) %>%
  mutate(va_mult_induced = pull(subset(value_added_mult_df, Sector %in% df[["MultiplierSector"]], select= c("Induced.impact"))))
  
  
# Calculate the outputs and value added

df <- df %>%
  mutate(initial_calc = as.numeric(gdp_per_worker) * as.numeric(no_of_workers_affected) * (as.numeric(period_of_impairment) / 365))

# Build whole dataframe

df <- df %>%
  mutate(direct_impact = mult_direct * initial_calc) %>%
  mutate(indirect_impact = mult_indirect * initial_calc) %>%
  mutate(induced_impact = mult_induced * initial_calc) %>%
  mutate(va_direct_impact = va_mult_direct * initial_calc) %>%
  mutate(va_indirect_impact = va_mult_indirect * initial_calc) %>%
  mutate(va_induced_impact = va_mult_induced * initial_calc)

# Tidying up and add a column to identify "output" changes vs "value_added" changes
output_df <- df %>% 
  select(Sector, MultiplierSector, direct_impact, indirect_impact, induced_impact) %>%
  gather(calculated,value, 3:5 ) %>%
  mutate(typeOfChanges="output")

output_df <- output_df[, c(1,2,5,3,4)]
output_df <- output_df[order(output_df$Sector),]  

va_df <- df %>% 
  select(Sector, MultiplierSector, va_direct_impact, va_indirect_impact, va_induced_impact) %>%
  gather(calculated,value, 3:5 ) %>%
  mutate(typeOfChanges="value_added")

va_df <- va_df[, c(1,2,5,3,4)]
va_df <- va_df[order(va_df$Sector),]  


# Building dataframe to be output in sunburst chart
# Need to have 2 columns only, "Agri - Paddy - .... - .... " , value

combined_df <- rbind(output_df, va_df)
combined_df_cls <- combined_df %>%
  mutate_all(funs(str_replace(.,"-",".")))
df_united <- unite(combined_df_cls, "V1", c(Sector, MultiplierSector, typeOfChanges, calculated),remove=FALSE, sep=" - ")
sb_output <- df_united %>%
  select(V1, value)

# breakdown by category and subcategory with direct, indirect, induced using sunburst
sunburst(sb_output) 

# viz using treemap
treemap(combined_df,
        index=c("Sector","MultiplierSector","typeOfChanges","calculated"),
        vSize="value",
        vColor="value")

# viz using bar chart



# Select important columns
calc_df <- df %>%
  select("Sector","MultiplierSector","no_of_workers_affected","period_of_impairment","employment","initial_calc","direct_impact",
         "indirect_impact","induced_impact","va_direct_impact","va_indirect_impact","va_induced_impact")


# Changes in Output tab
sum_initial_impairment <- sum(calc_df$initial_calc)
sum_output_direct <- sum(calc_df$direct_impact)
sum_output_indirect <- sum(calc_df$indirect_impact)
sum_output_induced <- sum(calc_df$induced_impact)
total_changes_in_output <- sum_output_direct + sum_output_indirect + sum_output_induced


# Changes in Value Added tab
sum_va_direct <- sum(calc_df$va_direct_impact)
sum_va_indirect <- sum(calc_df$va_indirect_impact)
sum_va_induced <- sum(calc_df$va_induced_impact)
total_changes_in_valueAdd <- sum_va_direct + sum_va_indirect + sum_va_induced

# Aggregated Changes tab
sum_initial_impairment
total_changes_in_output
total_changes_in_valueAdd

