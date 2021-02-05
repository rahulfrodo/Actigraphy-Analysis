# Actigraphy-Analysis
Identification of clinically relevant characteristic patterns of physical activity measured via continuous actigraphy in subjects with and without Mild Alzheimer’s.

Diurnal_Analysis_ref.html is a report containing analysis of continuous actigraphy data in subjects with and without Mild Alzheimer’s. Download the .html to view it in a browser.

Diurnal_Analysis_ref.Rmd contains the all the codes used in the analysis of original data and is used to produce the Diurnal_Analysis_ref.html file.

###################################################
toy_accl.Rdata is a simulated toy accelometry data\
data structure: List, each element is data for one person\
format: Each element of the list is a dataframe with the following columns\
 $ Vector.Magnitude: (num)  Vector magnitude of activity\
 $ Date            : (chr)  e.g "12/19/2012" \
 $ Time            : (chr)  e.g "14:00:00" \
 $ ID              : (int)  ID of the subject\
 $ Age             : (int)  age of the participant\
 $ Sex             : (chr)  gender\
 $ YearsOfEducation: (int)  number of years in education\
 $ V02maxkg        : (num)  cardiorespiratory capacity\
 $ BMI             : (num)  BMI\
 $ ADStatus        : (chr)  "Yes" or "No"

toy_cognitive.Rdata is a simulated toy cognitive measures data\
data structure: data frame, each row is one person, with the following columns\
 $ id          : (int)  ID of the subject\
 $ ATTN        : (num)  Attention score\
 $ VM          : (num)  Verbal memory score\
 $ ExecFunction: (num)  Executive function score

Diurnal_Analysis_toy_ref.html contains code and analysis using the simulated toy datasets toy_accl.Rdata and toy_cognitive.Rdata.
Diurnal_Analysis_toy_ref.Rmd contains the code for generating the .html file using the toy datasets. 
For running Diurnal_Analysis_toy_ref.Rmd the path within the code has to be set to path of the two datafiles toy_accl.Rdata and toy_cognitive.Rdata.
