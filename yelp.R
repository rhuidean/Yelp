# Load all packages
list.of.packages <- c("RODBC", "data.table","yelpr","plyr","RSQLite")
new.packages <- list.of.packages[(!list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)#
lapply(list.of.packages, require, character.only  =TRUE)
rm(list.of.packages,new.packages)

# Set Default settings
options(stringsAsFactors=FALSE)

# Scientific Number format issue resolution
options(scipen=999,digits =7)


# Store client id and API key locally
client_id <-"An9XHmTne2YakvLt57jYtg"
key <-


### Create final data housing Dataframe ( one for all base info and one for categories which has different amount of rows per id )
cmb_df_base = data.frame()
cmb_df_categories = data.frame()

# Execute 20 loop of API data fetching to get all 1000 maximum records
for (i in 1:20) {
  cat("Processing group ",i,"\n")
  if( i ==1 ){
    business_ny <- business_search(api_key = key,
                                 location = 'Toronto',
                                 term = "restaurants",
                                 limit =50)
  } else {
      business_ny <- business_search(api_key = key,
                                 location = 'Toronto',
                                 term = "restaurants",
                                 limit =50, offset=50*(i-1)+1)
  }

  
  # Extract the main raw Data frame
  dat <-business_ny$businesses
  
  # Check if the main raw Data frame is indeed a dataframe type
  if( class(dat) == "data.frame" ){
    # if the main raw data frame is raw data frame type process
    
    # Extract core columns with none object type(list, dataframe and other wonky types)
    base_col = c("id","alias","name","image_url","is_closed","url","review_count","rating","price","phone","display_phone","distance")
    tdf = dat[base_col]
    
    # combine two dataframes together (core column dataframe and coordiantes dataframe since the row count always match)
    tdf = cbind(tdf, dat$coordinates)
    
    
    # Cleanup location (remove display location which list duplicated data in list form not necessary)
    temp_location_df = dat$location
    temp_location_df = temp_location_df[c("address1","address2","address3","city","zip_code","country","state")]
  
    # cobmine two dataframes column wise (similar to coordinates since they have the same row count)
    tdf = cbind(tdf, temp_location_df)
    
    # Final add loop instance dataframe into final dataframe for base info
    cmb_df_base = rbind(cmb_df_base,tdf)
    cat("   Process finished for base dataframe...\n")
    
    # Process Categories seperately (categories have multiple records per unique id thus must be processed seperately)
    temp_cmb_categories_df = data.frame()
    for(i in 1:length(dat$categories)){
      # Extract each id dataframe (they always have two columns to start with)
      cat("      Processing ",i," record categories...\n")
      temp_category_df = dat$categories[[i]] 
      
      # add id which distinguishes the records
      temp_category_df$id = dat[i,"id"]
      temp_cmb_categories_df = rbind(temp_cmb_categories_df, temp_category_df)
    }
    cmb_df_categories = rbind(cmb_df_categories,temp_cmb_categories_df)
    
  } else {
    # if main raw dataframe is not dataframe type then pass
    cat(class(dat),"\n")
    cat(str(dat),"\n")
  }
}

cat("Data call complete... Total number of records saved = ",nrow(cmb_df_base))


### Data Check
cat("Unique Id in categories ",nrow(unique(cmb_df_categories["id"])),"\n")
cat("Unique alias in categories ",nrow(unique(cmb_df_categories["alias"])),"\n")
cat("Unique Id / alias combo in categories ",nrow(unique(cmb_df_categories[c("id","alias")])),"\n")
cat("Total number of records in categories ",nrow(cmb_df_categories),"\n")



### Saving in MS SQL Server
#SQL Connection
connection_str<-"Driver={SQL Server Native Client 11.0};server=JASONSERVER\\TORONTOSQL;database=yelp;trusted_connection=no;uid=jason;pwd=lj7767"
dcon<-odbcDriverConnect(connection=connection_str)

sqlSave(dcon,cmb_df_base,tablename='rest_business',append=T,rownames=F,nastring=NULL,safer=T,fast=F)
sqlSave(dcon,cmb_df_categories,tablename='rest_catrgories',append=T,rownames=F,nastring=NULL,safer=T,fast=F)

odbcClose(dcon)


