
# Data downloaded using noaaweatherdataGUI.py.
CBEP uses a custom Python program to download data from NOAA's online data
repositories.  Specifically, data were accessed through NOAA's National Centers
for Environmental Information.

Here, we have downloaded daily (GHCND) , monthly (GSOM), and annual (GSOY)
weather summaries via API v2. Information on this API is available here:
https://www.ncdc.noaa.gov/cdo-web/webservices/v2

Documentation on specific datasets is available at
https://www.ncdc.noaa.gov/cdo-web/datasets

## Software Access
CBEP's python program is available in this repository. This is not a polished
program, and may or may not work "out of the box." Some functionality implied by
the User Interface is incomplete. This software is in ongoing development by
CBEP staff, but the version here will not be regularly updated.  For the most
up-to-date version, contact CBEP.

To use the program, you will need to get an access token from NOAA (see the
website describing the API, above), and modify the code slightly on line 43 to
set MYTOKEN equal to the value of the token you receive from NOAA.

The "wide" data formatter makes some assumptions about the data being downloaded
that does not hold for older data (principally that the data categories in the
first portion of the downloaded data represent all possible categories).  It is
thus safest with the daily data to download the data in "long" format, unless
you are downloading for relatively short periods of time when data categories
remained consistent.

The program is fairly slow for long data series, since  it usually submits HTTP
requests for data on monthly intervals.  In our experience, each request takes
from three to five seconds if the system is lightly loaded, and more if there
are delays.  That means a decade's worth of downloads takes on the order of
10*12 * 4 = 480 seconds ~ 8 minutes.  In practice we are often seeing longer
delays.

## Original Data File Names
Data was downloaded to five files:
longdailydata.csv
longmonthlydata.csv
widemonthlydata.csv
longannualdata.csv
wideannualdata.csv

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
