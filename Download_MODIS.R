## Downloading MODIS
https://oceancolor.gsfc.nasa.gov/forum/oceancolor/topic_show.pl?pid=12520


# K10 SST download
https://podaac.jpl.nasa.gov/dataset/NAVO-L4HR1m-GLOB-K10_SST?ids=Platform:ProcessingLevel&values=Aqua:*4* 
wget -q --post-data="sensor=aqua&sdate=2009-07-04&edate=2018-12-13&dtype=L4&addurl=1&results_as_file=1&search=*PODAAC-GHK10-41N01*" -O - https://oceandata.sci.gsfc.nasa.gov/api/file_search |wget -i -
  
  
# This wget script will download the MODIS-Aqua Chlorophyll Concentration, OCI Algorithm, 8-day composite, for the period 2002-07-04 to 2018-12-13:
wget -q --post-data="sensor=aqua&sdate=2002-07-04&edate=2018-12-13&dtype=L3m&addurl=1&results_as_file=1&search=*L3m_8D_CHL_chlor_a_9km*" -O - https://oceandata.sci.gsfc.nasa.gov/api/file_search |wget -i -