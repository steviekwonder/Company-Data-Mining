## Import data set from the environment section, automatically assigns it to refine_original. 

refine_original <- read.csv("~/Desktop/GRE Vocab Data Analytics/refine_original.csv")

## 1. Clean up 'company' column, so all the mispellings of the brand names are standardized.
comp_fixed <- refine_original$company

comp_fixed[comp_fixed == "Phillips" | comp_fixed == "phillips" | comp_fixed == "phllips" | comp_fixed == "phillps" | comp_fixed == "phillipS" | comp_fixed == "fillips" | comp_fixed == "phlips"] <- "philips"
comp_fixed[comp_fixed == "Akzo" | comp_fixed == "AKZO" | comp_fixed == "akz0" | comp_fixed == "ak zo"] <- "akzo"
comp_fixed[comp_fixed == "Van Houten" | comp_fixed == "van Houten"] <- "van houten"
comp_fixed[comp_fixed == "Unilever" | comp_fixed == "unilver"] <- "unilever"
comp_fixed <- factor(comp_fixed)

## 2. Separate the product code and number into separate columns i.e. add two new columns called product_code 
# ... and product_number, containing the product code and number respectively. 

product_code <- refine_original$Product.code...number
product_code <- sub("[-[:digit:]]+","",product_code)

product_number <- refine_original$Product.code...number
product_number <- sub(pattern = "^[pxvq]-", replacement = "", x = product_number)

## 3. Add product categories
product_category <- product_code
product_category[product_category == "p"] <- "Smartphone"
product_category[product_category == "v"] <- "TV"
product_category[product_category == "x"] <- "Laptop"
product_category[product_category == "q"] <- "Tablet"

## 4. Add full address for geocoding
adr <- refine_original$address
city <- refine_original$city
country <- refine_original$country
full_address <- 1:25
c <- 1
while(c <= 25) {
  full_address[c] <- print(paste(adr[c],",",city[c],",",country[c]))
  c <- c + 1
}

##  5. Create dummy variables for company and product category
# For each company
company_philips <- comp_fixed
company_philips <- sub("p.*s", 1 , company_philips)
company_philips <- sub("^[avu].*[onr]", 0 , company_philips)

company_akzo <- comp_fixed
company_akzo <- sub("a.*o", 1 , company_akzo)
company_akzo <- sub("^[pvu].*[snr]", 0 , company_akzo)

company_vanhouten <- comp_fixed
company_vanhouten <- sub("^v.*n" , 1, company_vanhouten)
company_vanhouten <- sub("^[pau].*[sor]", 0, company_vanhouten)

company_unilever <- comp_fixed
company_unilever <- sub("^u.*r", 1, company_unilever)
company_unilever <- sub("^[pav].*[son]", 0 , company_unilever)

# For each of the product categories
product_smartphone <- product_category
product_smartphone <- sub("Smartphone", 1 , product_smartphone)
product_smartphone <- sub("^[TL].*[Vpt]", 0 , product_smartphone)

product_tv <- product_category
product_tv <- sub("TV", 1 , product_tv)
product_tv <- sub("^[STL].*[etp]", 0 , product_tv)

product_laptop <- product_category
product_laptop <- sub("Laptop", 1, product_laptop)
product_laptop <- sub("^[ST].*[Vet]", 0, product_laptop)

product_tablet <- product_category
product_tablet <- sub("Tablet", 1, product_tablet)
product_tablet <- sub("^[STL].*[Vep]", 0, product_tablet)


## 6. Clean all the data up to get what you want. 
name <- refine_original$name

refine_clean <- refine_original %>% 
  select(-(company:name)) %>%
  mutate(comp_fixed, product_code, product_number, product_category, 
         full_address, company_philips, company_akzo, company_van_houten, 
         company_unilever, product_smartphone, product_tv, product_laptop, product_tablet, name) 