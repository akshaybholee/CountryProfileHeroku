# Extract Reporter data

# df_country <-  dbGetQuery(
#   con,
#   "select
# 	  Country_ISO,
# 	  Country,
# 	  Region,
# 	  Continent,
# 	  Income_Group,
# 	  Development_Group,
# 	  Short_Name
# 	 from country where country_ISO in (select Reporter_ISO from Tracking_Trade_Data_Availability where latest_Year > 2013 )
#   order by Country"
# )
df_country <- read_feather("./global/df_country.feather")

#Extract product data
# df_product <- dbGetQuery(
#   con,
#   "select
# 	  distinct HS2_CODE code, HS2_DESCRIPTION description
#   from product
#   union
#   select
#   distinct HS4_CODE code, HS4_DESCRIPTION description
#   from product
#   union
#   select
#   distinct HS6_CODE code, HS6_DESCRIPTION description
#   from product",
#   as.is = TRUE
# )

df_product <- read_feather("./global/df_product.feather")