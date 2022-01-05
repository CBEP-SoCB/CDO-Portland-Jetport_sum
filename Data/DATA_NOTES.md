# Data preparation

##  Derivation of "hourly_clean.xlsx" from "hourly_raw.csv"
Original files have been deleted because of their size and because we did not
use them in any analyses.

This data was downloaded and examined, with the hope that we could generate a
dataset of true two-day storm totals, but we ran into technical challenges, and
decided it was not going to be worth the effort to solve them all.  These data
have not been used in further analyses.  These notes are here in case anyone
wants to look at hourly data in the future.

The downloaded CSV file is confusing in its layout, but it does link to the
descriptions in the associated documentation.  It appears that when I requested
"precipitation" data from CDO, that's all I got.  Yet there are a variety of
precipitation records from different sources included here, and it is not
immediately obvious how we should combine these different records.

We began with removal of data from 2020.  For simplicity, we cut off the record 
for our purposes Dec. 31, 2019.

Next, we removed MISSING data, so we deleted records without any data and data 
reporting a missing value (99) for the hours represented by the data.

We then removed data reporting precipitation totals for periods longer than 1 
hour (principally 3 hours, 6 hours, and 24 hours). We also removed a handful of 
records reporting other values for the hours represented, including one with 
zero hours, and one with 2 hours.

We then looked at things with a pivot table, to determine the structure of the
remaining one hour data.  There are 8760 hours in a (non-leap) year.  Prior to
1996, the records contain no more than the expected number of records,
suggesting records are not duplicates.  From 1996 on, we have too many records,
which suggests at least some duplication.

We also have only between 2000 and 3000 hourly observations from several years 
in the 1960s.

What we see in those years is the following:

1.   Time shift from splitting at even hours to splitting a XX:51

2.  SY_MT results begin to duplicate hourly values at 6 or 12 hour intervals.
    From 1996 through 2004, that code is ALMOST always associated with 0:00, 
    6:00, 12:00, or 18:00 observations.  There is a secondary peak of occurrence 
    around the three hour intervals as well, mostly 1997 through 2001.  A 
    scattering of other time stamps -- each represented by about 20 
    observations -- turn up beginning in 2002 and continuing to the present.

SAO appears to offer a complete record from 1948 through 1965.  From 1966 through 1972, it provided complete records, but only every three hours (which suggests other records may be needed for this period).  The record is again NEARLY complete, with missing observations around the 0,6,12, and 18 hour slots, through 1995.  About half way through 1996, the SAO data ends.

SY-SA data begins in 1973, and continues with (nearly) complete data from 0,6, 12, and 18 hours into (mid?) 1996.

FM15 looks complete or nearly so for (mid?) 1996 through 2019, but with holes in some years and some hours.

FM12 is too rare to affect things much.
AUTO might help, but it is mostly first half of the year 2003-2004 (with other scattered observations).

So, a collection of
SAO
SY-SA
AUTO
FM15 is nearly complete for most years

But we still have holes, especially in the latter half of the year for 1997-2002 or so.

So, we need to do this in a more sophisticated way, which means reading it into R and manipulating it there to look for duplicates...


