# Care homes

This repository shows the data analysis for dealing with data referred to care homes in UK. It contains various types of folders that include _Data_, _Doc_, _Output_ and _R_

### Data

This folder contains the raw data. It is obtained from the Care Quality Commission Strategy & Intelligence Directorate upon request. This dataset is allocated in:

  - `/raw` and should be considered as read only.
  - `/processed` contains transformed data that has been manipulated. 
  

### Doc

This folder contains the written research.


### Output 

Results of the analysis in terms of datasets, figures and/or tables. 

### R 

Contains scripts with different types of analysis.

   - `funciones`: List of functions used for the analysis. 
   - `cqc_data`: Cleans the data of the cqc dataset. It makes names of variables easier to manipulate.
   - `entries_exits_cqc`: It calculates the entries and exits associated with the CQC. It differentiates between _de novo_ entries and spurious entries. 
   - `l&b_registry`: Cleans data from Laing and Buisson dataset, recodes and selects important variables. 
   - `link_cqc_lb`: Analysis for matching data from CQC and L&B datasets.
   - `link_geographical_cqc`: Link information from the CQC and the ONS geographical directory. 
   



