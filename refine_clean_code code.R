## Import data set from the environment section, automatically assigns it to refine_original. 

refine_original <- read.csv("refine_original.csv")

## 1. Clean up 'company' column, so all the mispellings of the brand names are standardized.
comp_fixed <- refine_original$company

comp <- refine_original$company
comp[comp == 'Phillips' | comp == "phillips" | comp == "phillps" | comp == "phillipS" | comp == "fillips" | comp == "phlips" | comp == "phllips"] <- 'philips'
# TODO refactor akzo vanhouten and unilever portions

# i <- 1
# while(i <= 25) {
#   if(comp_fixed[i] == "Phillips" | comp_fixed[i] == "phillips" | comp_fixed[i] == "phillps" | comp_fixed[i] == "phillipS" | comp_fixed[i] == "fillips" | comp_fixed[i] == "phlips" | comp_fixed[i] == "phllips") {
#     comp_fixed[i] <- "philips"
#   }
#   if(comp_fixed[i] == "Akzo" | comp_fixed[i] == "AKZO" | comp_fixed[i] == "akz0" | comp_fixed[i] == "ak zo") {
#     comp_fixed[i] <- "akzo"
#   } 
#   if(comp_fixed[i] == "Van Houten" | comp_fixed[i] == "van Houten") {
#     comp_fixed[i] <- "van houten"
#   }
#   if(comp_fixed[i] == "unilver" | comp_fixed[i] == "Unilever") {
#     comp_fixed[i] <- "unilever"
#   }
#   i <- i + 1 
# }

## 2. Separate the product code and number into separate columns i.e. add two new columns called product_code 
# ... and product_number, containing the product code and number respectively. 

product_code <- refine_original$Product.code...number
product_code <- gsub("[-[:digit:]]+","",product_code)

# product_code <- sub(pattern = "-1", replacement = "", x = product_code)
# product_code <- sub(pattern = "-2", replacement = "", x = product_code)
# product_code <- sub(pattern = "-3", replacement = "", x = product_code)
# product_code <- sub(pattern = "-4", replacement = "", x = product_code)
# product_code <- sub(pattern = "-5", replacement = "", x = product_code)
# product_code <- sub(pattern = "-6", replacement = "", x = product_code)
# product_code <- sub(pattern = "-7", replacement = "", x = product_code)
# product_code <- sub(pattern = "-8", replacement = "", x = product_code)
# product_code <- sub(pattern = "-9", replacement = "", x = product_code)

# product_code <- sub(pattern = "1$", replacement = "", x = product_code)
# product_code <- sub(pattern = "2$", replacement = "", x = product_code)
# product_code <- sub(pattern = "3$", replacement = "", x = product_code)
# product_code <- sub(pattern = "4$", replacement = "", x = product_code)
# product_code <- sub(pattern = "5$", replacement = "", x = product_code)
# product_code <- sub(pattern = "6$", replacement = "", x = product_code)
# product_code <- sub(pattern = "7$", replacement = "", x = product_code)
# product_code <- sub(pattern = "8$", replacement = "", x = product_code)
# product_code <- sub(pattern = "9$", replacement = "", x = product_code)
# product_code <- sub(pattern = "0$", replacement = "", x = product_code)

product_number <- refine_original$Product.code...number
product_number <- sub(pattern = "^[pxvq]-", replacement = "", x = product_number)
# product_number <- sub(pattern = "^x-", replacement = "", x = product_number)
# product_number <- sub(pattern = "^v-", replacement = "", x = product_number)
# product_number <- sub(pattern = "^q-", replacement = "", x = product_number)


## 3. Add product categories
product_category <- product_code
product_category[product_code == 'p','product_code'] <- 'Smartphone'
# TODO refactor for tv laptop and tablet
# product_category <- data.frame('code'=product_code)
# fullnames <- data.frame('abbreviation'=c('p','v','x','q'),
#                         'fullname'=c('Smartphone','TV','Laptop','Tablet'))
# merge(product_category,fullnames,by.x='code',by.y='abbreviation', sort=FALSE)
k <- 1
while(k <= 25) {
  if(product_category[k] == "p") {
    product_category[k] <- "Smartphone"
  }
  if(product_category[k] == "v") {
    product_category[k] <- "TV"
  }
  if(product_category[k] == "x") {
    product_category[k] <- "Laptop"
  }
  if(product_category[k] == "q") {
    product_category[k] <- "Tablet"
  }
  k <- k + 1
}

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
fac_comp_fixed <- factor(comp_fixed)
ref_vec <- fac_comp_fixed
levels(ref_vec) <- 0:3   # did this because, it keeps mentioning invalid factor level if I try to assign 0 or 1 to "philips"

company_philips <- ref_vec 
a <- 1
while(a <= 25) {
  if(company_philips[a] == 2 | company_philips[a] == 3) {
    company_philips[a] <- 0
  }
  a <- a + 1
}

company_akzo <- ref_vec
a <- 1
while(a <= 25) {
  if(company_akzo[a] == 0) {
    company_akzo[a] <- 1
  }
  else{company_akzo[a] <- 0}
  a <- a + 1
}

company_van_houten <- ref_vec
a <- 1
while(a <= 25) {
  if(company_van_houten[a] == 3) {
    company_van_houten[a] <- 1
  }
  else{company_van_houten[a] <- 0}
  a <- a + 1
}

company_unilever <- ref_vec
a <- 1
while(a <= 25) {
  if(company_unilever[a] == 2) {
    company_unilever[a] <- 1
  }
  else{company_unilever[a] <- 0}
  a <- a + 1
}

# For each of the product categories
product_smartphone <- product_category
b <- 1
while(b <= 25) {
  if(product_smartphone[b] == "Smartphone") {
    product_smartphone[b] <- 1
  }
  else{product_smartphone[b] <- 0}
  b <- b + 1
}

product_tv <- product_category
b <- 1
while(b <= 25) {
  if(product_tv[b] == "TV") {
    product_tv[b] <- 1
  }
  else{product_tv[b] <- 0}
  b <- b + 1
}

product_laptop <- product_category
b <- 1
while(b <= 25) {
  if(product_laptop[b] == "Laptop") {
    product_laptop[b] <- 1
  }
  else{product_laptop[b] <- 0}
  b <- b + 1
}

product_tablet <- product_category
b <- 1
while(b <= 25) {
  if(product_tablet[b] == "Tablet") {
    product_tablet[b] <- 1
  }
  else{product_tablet[b] <- 0}
  b <- b + 1
}

## 6. Clean all the data up to get what you want. 
name <- refine_original$name

refine_clean <- refine_original %>% 
  select(-(company:name)) %>%
  mutate(comp_fixed, product_code, product_number, product_category, 
         full_address, company_philips, company_akzo, company_van_houten, 
         company_unilever, product_smartphone, product_tv, product_laptop, product_tablet, name) 