library(tidyverse)
library(arrow)


#I realized that putting in all these files of this size would likely 
#cause issues with Github's storage limit. So would need to either investigate
#large file storage or parquet format. 

csv_dir <- 'C:/Users/ianch/Desktop/RStuff/DATA612/data/flt_datasets'


#To fix this, force Arrow to read the
#affected column as a string (utf8()) during dataset ingestion, 
#then clean and cast the data using dplyr after calling collect()


#data_dte	Year	Month	usg_apt_id	usg_apt	usg_wac	fg_apt_id	fg_apt	fg_wac	airlineid	carrier	carriergroup	type	Scheduled	Charter	Total	Primary_Key


my_schema <- schema(
  data_dte=string(),
  Year=int64(),
  Month=int64(),
  usg_apt_id=int64(),
  usg_apt=string(),
  usg_wac=int64(),
  fg_apt_id=int64(),
  fg_apt=string(),
  fg_wac=int64(),
  airlineid=int64(),
  carrier=string(),
  carriergroup=int64(),
  type=string(),
  Chartered= utf8(),
  Scheduled= utf8(), # Column with commas read as text
  Total= utf8(),
  Prinary_Key=utf8()
)

csv_dataset <- open_dataset(csv_dir, format='csv', schema=my_schema, skip=1)

csv_dataset <- csv_dataset |> 
  select(-Scheduled,-Chartered,-Prinary_Key) |>
  filter(carrier!='CP' & !is.na(Total)) |>  #CP carrier from 2016-2025 had some one-off flights causing issues.
  mutate(Total=as.numeric(str_replace(Total,',','')),
         data_dte=mdy(data_dte)) |> 
  collect()

#test to make sure everything is in there and looking fine
csv_dataset |> 
head()|>
  collect()


csv_dataset |> 
  tail() |> 
  collect()




write_parquet(csv_dataset, '../data_raw/flights_merged.parquet')
