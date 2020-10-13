########################################################################
########################################################################
# Food Data:
# - Noncrowdsourced  
# - Recall period = week  
# - Calculated calories per food item(s) reported  
# - Calculated average calories (per hh member) per week by household  
# - Cleaned foods reported as counts 
# - Standardized calories to calories per gram 
########################################################################
########################################################################

#########
# load necessary packages
#########
requiredPackages = c('dplyr', # data manipulation
                     'readr', # read files
                     'ggplot2', # plots
                     'haven', # read dta
                     'tidyverse') # missing values using map
# only downloads packages if needed
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

#########
# import dta
#########

# set working directory
setwd("/Users/mr4909/bangladesh/data")

# read food info (calories, food names)
food_info <- read_csv("food_info_mari.csv")

# import food datasets (3)
# select noncrowdsourced data and recall period of a week

fd_1 <- read_dta("33.FoodDiary_level_1.dta")
fd_2 <- read_dta("34. FoodDiary_food_level_2.dta")
fd_3 <- read_dta("35. FoodDiary_food_foodtype_level_3.dta")

#########
# cleaning
#########

# remove crowdsourcing and set recall to week
fd_1 <- fd_1 %>% filter(crowdsource==0 & recall=="week") %>% 
  # select variables
  select(hh_ID, 
         food_1_ID,
         #submissiondate, 
         #recall, # month 516 season 272 week 4944
         week_number, #3:50 weeks
         food_list, # groups of foods consumed
         food_count) # number of food types reported

# import food types
fd_2 <- fd_2 %>% select(hh_ID, food_1_ID, food_2_ID, food_grp_namec) 

# merge food datasets / missing values for food_type = food ID number
fd_1_2 <- merge(fd_1,fd_2, by=c("hh_ID","food_1_ID"))

# keep unique values
fd_1_2 <- fd_1_2 %>% distinct()

# import food names and quantities
fd_3 <- fd_3 %>% select(hh_ID, 
                        food_2_ID, 
                        food_3_ID, 
                        food_ID = food_type_name, 
                        food_type_quant,
                        food_type_unit)
                                      # quant_pur, #How much consumed was purchased?
                                      # quant_own, #How much consumed was own production?
                                      # quant_other) #How much consumed was from other sources?

fd_all <- merge(fd_1_2,fd_3, by=c("hh_ID","food_2_ID"))

# remove unwanted variables
# fd_all <- fd_all %>% select(-food_type) # duplicate variable

# 118 food_type_name are missing
# remove missing values for now.
fd_all <- fd_all[complete.cases(fd_all),]

# remove unwanted variables
fd_all <- fd_all %>% select(hh_ID,
                            week_number,
                            food_ID,
                            food_grp_namec,
                            food_type_quant,
                            food_type_unit)

#########
# Calories
#########

# merge with food list names 
food <- merge(food_info, fd_all, by="food_ID")

# select/reorder variables
food <- food %>% select(hh_ID,
                        week_number,
                        food_name,
                        food_name_type,
                        food_type_quant,
                        food_type_unit,
                        calories_grams)

#########
# *Food Type Unit* - need to be standardized
#########
# 1	Kg          *1000
# 2	Grams
# 3	Liter       *1000
# 4	Milliliter  
# 5	Tablespoon  *15
# 6	Teaspoon    *4.2
# 7	Drops       *0.05
# 8	Count

# unit conversions
food <- food %>% 
  mutate(calories_total_grams = case_when((food_type_unit==1) ~ calories_grams*(food_type_quant*1000),
                                          (food_type_unit==2) ~ calories_grams*food_type_quant,
                                          (food_type_unit==3) ~ calories_grams*(food_type_quant*1000),
                                          (food_type_unit==4) ~ calories_grams*food_type_quant, 
                                          (food_type_unit==5) ~ calories_grams*food_type_quant,
                                          (food_type_unit==6) ~ calories_grams*(food_type_quant*4.2),
                                          (food_type_unit==7) ~ calories_grams*(food_type_quant*0.05)))

#########
## Fix Food Units Claimed As Counts
#########

# food unit type selected = 8
count <- food %>% filter(food_type_unit == 8) %>% arrange(desc(food_name_type))

# remove counts over 100 
count <- count %>% filter(food_type_quant < 100)

#########
# fix outliers
# convert to total calories reported in grams
#########

count$calories_total_grams = as.numeric(
  
  # cereals over 100, likely meant as grams
  # multiplies grams/creals x quantity reported
  ifelse(count$food_name_type %in% c('cereals') & count$food_type_quant > 100, 
         count$calories_grams*count$food_type_quant,
                       
                                 # grains (changed to serving size 100g)
                       case_when(count$food_name=="Parboiled rice (coarse)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Fine rice" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Atta" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Non-parboiled rice (coarse)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Semai/noodles" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chira (flattened rice)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Muri/Khoi (puffed rice)" ~ count$calories_grams*count$food_type_quant*100,
                                 # meats/eggs (changed to serving size 100g)
                                 count$food_name=="Beef/buffalo" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mutton" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chicken" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pigeon" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Fish egg" ~ count$calories_grams*count$food_type_quant, # egg roe 
                                 count$food_name=="Egg" ~ count$calories_grams*count$food_type_quant*100, # egg 
                                 count$food_name=="Duck" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Stomach of beef/goat" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried meat" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bids/bok/gughu" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Liver" ~ count$calories_grams*count$food_type_quant*100,
                                 # drinks/dairy (changed to serving size for can = 355oz / 355g)
                                 count$food_name=="Tea –prepared" ~ count$calories_grams*count$food_type_quant*355,
                                 count$food_name=="Coke/ Seven-up etc/Pepci/RC/Urocola etc" ~ count$calories_grams*count$food_type_quant*355,
                                 count$food_name=="Milk" ~ count$calories_grams*count$food_type_quant*355,
                                 count$food_name=="Milk in Tea" ~ count$calories_grams*count$food_type_quant*355,
                                 # dairy (changed to serving size 100g)
                                 count$food_name=="Condensed Milk" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Powdered Milk" ~ count$calories_grams*count$food_type_quant*100,# serving of powdered milk
                                 count$food_name=="Butter" ~ count$calories_grams*count$food_type_quant*100, # serving of butter
                                 count$food_name=="Yogurt" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cottage cheese or Paneer" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Buttermilk" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Solid Cheese Curds" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Ice Cream" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Ice cream" ~ count$calories_grams*count$food_type_quant*100,
                                 # fish small (changed to serving size 100g)
                                 count$food_name=="Gura mach" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Panch mishali" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Palshe" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Tatkeni" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Puti" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Tengra" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Karfu fish" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bata" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kajari" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Small prawn" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried small shrimp/prawn" ~ count$calories_grams*count$food_type_quant*100,
                                 # pulses (changed to serving size 100g)
                                 count$food_name=="Khesari" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried Lentil" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Shem bitchi" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Anchor daal" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried Chick pea" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mung" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Black gram" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried Pea" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Groundnut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Brazilnut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cashew Nut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Almond/Badam" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Hazelnut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chestnut" ~ count$calories_grams*count$food_type_quant*100,
                                 # fix vegetables (avg serving size per item)
                                 count$food_name=="Patal" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Okra" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Eggplant" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bitter gourd" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Radish" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pumpkin" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Green chili" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Potato" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Sweet potato" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Sheem" ~ count$calories_grams*count$food_type_quant*100, # broad bean
                                 count$food_name=="Onion" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Tomato" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Sweet gourd" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Garlic" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Water gourd" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Drum stick" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Carrot" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Green banana" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cauliflower" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Danta (amaranth)" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Green Papaya"~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Ash gourd" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cucumber" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kachu (arum)" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Green mango" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dhundal" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kachur lati" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Green pea" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Green jackfruit" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mete alu" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kakrol" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Soybean bori" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Beher gura" ~ count$calories_grams*count$food_type_quant*100, 
                                 count$food_name=="Shalgom" ~ count$calories_grams*count$food_type_quant*100, 
                                 # fish large (avg serving size per item)
                                 count$food_name=="Rui" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Telapia" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Swarputi" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Singi" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Grass Carp" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Taki" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pangash" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Silver Carp" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mrigel" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kalibaus" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Hilsa" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Katla" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Jatka" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Aair" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Boal" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dried fish" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mirror Carp" ~ count$calories_grams*count$food_type_quant*100,
                                 # spices (1 tablespoon	= 14.3 grams)
                                 count$food_name=="Dried chili" ~ count$calories_grams*count$food_type_quant*14.3,
                                 count$food_name=="Elachi" ~ count$calories_grams*count$food_type_quant*14.3,
                                 count$food_name=="Cinnamon" ~ count$calories_grams*count$food_type_quant*14.3,
                                 count$food_name=="Fresh Ginger" ~ count$calories_grams*count$food_type_quant*14.3,
                                 # other food (avg serving size per item)
                                 count$food_name=="Pitha" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Ruti/Parota" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Singara" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Rice/Jao" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Piaju" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cake" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Nimki" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Puri" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Alur chap" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Biscuit" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bonroti/paoroti" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Khichuri" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Polao/Biryani/Tehari" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chips" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Patties" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chewing gum" ~ count$calories_grams*count$food_type_quant*100,                                               
                                 # vegetables (avg serving size per item)
                                 count$food_name=="Lal Shak (red amaranth)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bathua" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pat Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dheki Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pui (Indian spinach)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dhania Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Lau Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Onion/garlic stalk" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kalmi Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Radish leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kachu Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Kalo kachu Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Cabbage" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Helencha" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Katanate" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Palang Shak (spinach)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pea leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Drumstick leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mustard leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mixed leafy vegetables" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dudhali Pata" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Khesari Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Swett gourd leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Black gram leaves" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Geema Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Neem Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Darkuni Shak" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bokful" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Litchis" ~ count$calories_grams*count$food_type_quant*100,
                                 # oil (serving size 1tbs 14g)  
                                 count$food_name=="Soybean" ~ count$calories_grams*count$food_type_quant*14,
                                 count$food_name=="Mustard" ~ count$calories_grams*count$food_type_quant*14,
                                 count$food_name=="Dalda/banspati" ~ count$calories_grams*count$food_type_quant*14,
                                 count$food_name=="Red Palm Oil" ~ count$calories_grams*count$food_type_quant*14,
                                 count$food_name=="Ghee" ~ count$calories_grams*count$food_type_quant*14,
                                 count$food_name=="Sesame oil" ~ count$calories_grams*count$food_type_quant*14, 
                                 count$food_name=="Yellow Palm Oil" ~ count$calories_grams*count$food_type_quant*14,
                                 # fruit(avg serving size) 
                                 count$food_name=="Apple" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Mango" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Guava" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Banana" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Lemon" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Karambola" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Grapes" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dates" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Jujube/dried jujube" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Coconut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Orange" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bel" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Tamarind" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Olive" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Chalta" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pomelo" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Jack Fruit" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pomelo" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Amra" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Papaya" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Black berry" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Green Coconut" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Dalim" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Tarmuj (Water melon)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Ata (bullock's heart)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Bangi (Musk melon)" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Pine apple" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Sobeda" ~ count$calories_grams*count$food_type_quant*100,
                                 count$food_name=="Jaamrul" ~ count$calories_grams*count$food_type_quant*100)))

# check for outliers
summary(count$calories_total_grams)

#########
## Fix Food Units For No Counts
#########

# *Food Type Unit* - need to be standardized
# 1	Kg          *1000
# 2	Grams
# 3	Liter       *1000
# 4	Milliliter  
# 5	Tablespoon  *15
# 6	Teaspoon    *4.2
# 7	Drops       *0.05
# 8	Count

# merge count (unit = 8) and other dataset count to get complete df
nocount <- food %>% filter(food_type_unit != 8)
# remove foods counted as "drops" but aren't liquids
drops <- nocount %>% filter(food_type_unit == 7 & food_name_type == "oil")
nodrops <- nocount %>% filter(food_type_unit != 7 & food_name_type != "oil")
nocount <- rbind(drops,nodrops)

# merge count and no count datasets
food <- rbind(nocount, count)

########################################################################
########################################################################
# FINAL FOOD DIARY DATAFRAME
# Each food item with calorie info including foods with food type unit = “count”.
# removed outliers
########################################################################
########################################################################

# remove missing data 
food <- food[complete.cases(food),]

# check for outliers for 13351 occurences
summary(food$calories_total_grams)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0     147     500       2606    1254      2210806 

# actual values of the outliers
boxplot(food$calories_total_grams)$out

# assign the outlier values into a vector
outliers <- boxplot(food$calories_total_grams, plot=FALSE)$out

# check the results
print(outliers)

# rows the outliers are
food[which(food$calories_total_grams %in% outliers),]

# remove the rows containing the outliers
food <- food[-which(food$calories_total_grams %in% outliers),]

# check now with boxplot, outliers are gone
boxplot(food$calories_total_grams)

# copy df
fd <- food

# calculate number of submissions
myfreqs <- fd %>% group_by(hh_ID, week_number) %>% summarise(num_submissions = n())

# merge frequencies with food data
fd <- merge(fd, myfreqs, by = c("hh_ID","week_number"))

#########
# calculate proportions
#########

# meat
fd <- fd %>% mutate(isMeat = ifelse(food_name_type == "meategg",1,0))
fd <- fd %>% mutate(meat_calories = ifelse(isMeat == 1,calories_total_grams,0))

# fish
fd <- fd %>% mutate(isFish = ifelse(food_name_type == "fishlarge"|food_name_type == "fishsmall",1,0))
fd <- fd %>% mutate(fish_calories = ifelse(isFish == 1,calories_total_grams,0))

# protein
fd <- fd %>% mutate(isProtein = ifelse(food_name_type == "fishlarge"|
                                                       food_name_type == "fishsmall"|
                                                       food_name_type == "meategg",1,0))
fd <- fd %>% mutate(protein_calories = ifelse(isProtein == 1,calories_total_grams,0))

# meat
pct_meat <- fd %>% group_by(hh_ID, week_number) %>% tally(isMeat)
pct_meat <- pct_meat %>% mutate(numMeat = n) %>% select(-n)
myfreqs <- fd %>% group_by(hh_ID, week_number) %>% summarise(freq = n())
pct_meat <- merge(myfreqs, pct_meat, by = c("hh_ID","week_number"))
pct_meat <- pct_meat %>% mutate(pct_meat = numMeat/freq) #%>% select(-numMeat, -freq)
fd <- merge(fd, pct_meat, by = c("hh_ID","week_number"))

# fish    
pct_fish <- fd %>% group_by(hh_ID, week_number) %>% tally(isFish)
pct_fish <- pct_fish %>% mutate(numFish = n) %>% select(-n)
pct_fish <- merge(myfreqs, pct_fish, by = c("hh_ID","week_number"))
pct_fish <- pct_fish %>% mutate(pct_fish = numFish/freq) #%>% select(-numFish, -freq)
fd <- merge(fd, pct_fish, by = c("hh_ID","week_number","freq"))

# protein
pct_protein <- fd %>% group_by(hh_ID, week_number) %>% tally(isProtein)
pct_protein <- pct_protein %>% mutate(numProtein = n) %>% select(-n)
pct_protein <- merge(myfreqs, pct_protein, by = c("hh_ID","week_number"))
pct_protein <- pct_protein %>% mutate(pct_protein = numProtein/freq) #%>% select(-numProtein, -freq)
fd <- merge(fd, pct_protein, by = c("hh_ID","week_number","freq"))

#########
# averages for the year / no weekly data
#########

temp <- hh_count %>% select(hh_ID, hh_members)
fd <- merge(temp, fd, by = c("hh_ID"))
# avg calories per hh member per food item 
fd <- fd %>% mutate(avg_calories_pp = calories_total_grams/hh_members)

#########
# totals by hh and week
#########

fish <- fd %>% group_by(hh_ID, week_number) %>% tally(fish_calories)
        fish <- fish %>% mutate(fish_calories_wk = n) %>% select(-n)
meat <- fd %>% group_by(hh_ID, week_number) %>% tally(meat_calories)
        meat <- meat %>% mutate(meat_calories_wk = n) %>% select(-n)
protein <- fd %>% group_by(hh_ID, week_number) %>% tally(protein_calories)
        protein <- protein %>% mutate(protein_calories_wk = n) %>% select(-n)

# merge fish meat protein        
weekly_protein <- merge(fish, meat, by = c("hh_ID","week_number"))
                  weekly_protein <- merge(weekly_protein, protein, by = c("hh_ID","week_number"))

# select variables
fd <- fd %>% select(hh_ID,
                    week_number,
                    num_submissions,
                    calories_total_grams,
                    avg_calories_pp,
                    pct_meat,
                    pct_fish,
                    pct_protein)

# find averages / totals
week_sums <- fd %>% select(hh_ID, week_number, calories_total_grams, avg_calories_pp)

week_sums <- week_sums %>% 
  group_by(hh_ID, week_number) %>% 
  summarise_each(funs(sum))                  

# get pcts
temp <- fd %>% select(-calories_total_grams,-avg_calories_pp) %>% distinct()

# merge pcts with other info                  
fooddairy_week <- merge(temp, week_sums, by = c("hh_ID","week_number"))

# merge with hh_info
fooddairy_week <- merge(hh_count, fooddairy_week, by = c("hh_ID"))

fooddiary <- fooddairy_week %>% mutate(calories_total = calories_total_grams,
                                       avg_calories_pp = avg_weekly_calories_pp) %>% select(-calories_total_grams, -avg_weekly_calories_pp)

# rename and reorder variabkes
fooddiary <- fooddiary %>% select(hh_ID,
                                  week_number,
                                  calories_total,
                                  member_1_ID, 
                                  hh_members, 
                                  avg_calories_pp,
                                  pct_fish,
                                  pct_meat,
                                  pct_protein,
                                  num_submissions)


# merge with basic info
# 176 households
final_data <- merge(summaries, hh_respondent, by = "hh_ID")

final_data <- merge(fooddiary, hh_respondent, by = c("hh_ID","hh_members"))
write.csv(final_data, "final_data.csv")

# selct variables
summaries <- fooddairy_week %>% select(hh_ID, 
                                       freq = num_submissions,
                                       pct_meat, 
                                       pct_fish, 
                                       pct_protein, 
                                       calories_total_grams,
                                       avg_calories_pp)
summaries <- merge(summaries, weekly_protein, by = "hh_ID")
summaries <- summaries %>% select(-week_number)
summaries <- summaries %>% group_by(hh_ID) %>% summarise_each(funs(mean, sd, var))
# merge with basic info
# 176 households
final_data <- merge(summaries, hh_respondent, by = "hh_ID")

# write csvs

write.csv(fooddiary, "fooddiary_week.csv")

write.csv(weekly_protein, "weekly_protein.csv")

write.csv(final_data, "final_data.csv")


