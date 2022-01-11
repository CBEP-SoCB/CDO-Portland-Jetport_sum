
# Historical Weather Data from the Portland Jetport

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Data downloaded using noaaweatherdataGUI.py.
CBEP used a custom Python program to download data from NOAA's online data
repositories.

We  downloaded daily (GHCND) and annual (GSOY) weather summaries via API v2. 
Information on this API is available here:
https://www.ncdc.noaa.gov/cdo-web/webservices/v2

Documentation on specific datasets is available at
https://www.ncdc.noaa.gov/cdo-web/datasets

## Software Access
CBEP's python program helps manage data download and assembling of a continuous
data record through the API.  The code is not available in this repository. It 
is included in a companion detailed
[data archive](https://github.com/CBEP-SoCB-Details/CDO-Portland-Jetport.git),
along with rudimentary useage notes.  It is not a polished program, and may or 
may not work "out of the box." 

## Data Files
THe two data files are both in "long" data format, with the following data columns:

Variable Name |  Meaning                     
--------------|---------------------------------------------------------------
date          | date , YYYY-MM-DDT00:00:00 (January 1 of year for annual data)
datatype      | NOAA code for data attributes
value         | Numerical value, interpreted according to datatype
attributes    | NOAA data quality qualifiers
station       | Portland Jetport is GHCND:USW00014764

### `longdailydata.csv`
Contains "daily" summary data on weather conditions observed at the Portland 
Jetport beginning in 1940.  This data was used to calculate annual days of first
and last frost, length of growing season, and number of days with large (> 2 
inch) storm events.

### `longannualdata.csv`
Contains "annual" summary data on weather conditions observed at the Portland 
Jetport beginning in 1940. This file includes data on things like total annual 
precipitation, maximum annual temperature, minimum annual temperature, etc.

## Metadata
### `summary_of_monthly_and _annual_data_fields.docx`
We provide a word document containing a table with metadata defining the
data types contained in these files.  This file was derived and simplified from
metadata provided by NOAA.  Additional NOAA metadata iles are available at the
companion detailed 
[data archive](https://github.com/CBEP-SoCB-Details/CDO-Portland-Jetport.git), 
or directly from NOAA, at the URLs provided, above.

# Other Data Downloads
In searching for alternative data on which to base analysis of precipitation
"events" over 24 hours, we found this:
https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_map_cont.html?bkmrk=me
Which contains modeled return intervals, but no historical perspective.  The web
page refers to Climate Data Online as its source.

Returning to CDO, we searched through the following link
https://www.ncei.noaa.gov/access/search/data-search/global-hourly
Which appears to offer hourly data back to 1940 dates for Portland jetport. We
did not created python code to access these data,but that should be possible.

We submitted a request to CDO, requesting only precipitation data.  Original file
downloaded March 9, 2019. by Curtis C. Bohlen. Data arrived as "2070949.csv", and
was renamed "hourly_raw.csv."

This file is confusing in its layout, but it does relate to the descriptions in
the associated documentation.  It appears that when we selected "precipitation"
data, we got only precipitation, but many different versions, including
overlapping dates and sources. Not all data rows are independent. We used
the various available codes to interpret the data by SOURCE and REPORT TYPE, but
because of limited confidence in the results, we have not further analyzed these
data.
