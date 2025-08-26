# tribal-team-3
taking raw high frequency data → cleaning it using QCtools → write to database WQTS (cleaned hugh frequency dataset)

## main folders

### data_for_proj3
As stated in the name, this where we stored all the data files we have actively used in our code. This includes the raw times data shared by the Jamestown Sklallam Tribe as well as the lookup table with deployment/retrieval dates, included in the folder labeled jst_data_ldr_times [ADD IN WHAT LOOKUP TABLES ARE FOR]. It also includes reference WQTS data from the Hoh tribe.

### cleaning
Example code for how to clean the data using dataQCtools such as cropping the data and creating the qc plots. It also contains the modified function for cropping raw data.

### statistics
Simple code for calculating 7DADM (seven day moving average) as well as the code used to check what caused differences between the statistics that have been calculated

### plots
It has all the qc plots in there

### dataQCtools
A copy of Jesse's repo under the same name. 
