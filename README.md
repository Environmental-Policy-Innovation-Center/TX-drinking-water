![](epic-logo-transparent.png)

# Texas Drinking Water Analysis

This repository contains code for downloading, organizing, and analyzing data for the Texas Drinking Water project. Data organized from this repository feed into the [Texas Community Water System Prioritization Tool](https://tx-app.policyinnovation.info/), which is a screening tool that can assist in prioritizing advocacy and technical assistance for community water systems in Texas. 

For important context and descriptions of data used within this repository, please review our [data dictionary](https://docs.google.com/spreadsheets/d/1bzNPxhL-l6DeGElhG1c70Of8DGAQasMDUuX3rPHVe2A/edit#gid=0) and [methodology](https://docs.google.com/document/d/1va2Iq2oJxnqiwgNHD4bWpXKxdWbq-TYoYkosj1oz_JU/edit). The application repository is available [here](https://github.com/Environmental-Policy-Innovation-Center/tx-dw-tool). 

## The pipeline for this project (organized in the code folder by number) includes: 

-   Downloading, tidying, and organizing datasets. Data that require extensive tidying/reorganizing are captured in `1_TX_DW_downloading.Rmd`. These data are then read into `2_TX_DW_collating.Rmd`, where all data for the project are collected and organized into data lists that are organized by topic. These data lists are used in the application. 
	
-   Merging datasets. Captured in `3_TX_DW_merging.Rmd`, this file pulls in the data lists and preforms additional merging. Most of the merging in this step aims to connect environmental data with utility information.  
	
-   Analyzing and answering research questions. Analysis files start with 4_TX_DW_ and are split up by topics: hydrology, socioeconomic, and governance. 

Since this project utilizes EPIC's private amazon web service s3 buckets throughout the data pipeline, it is recommended that you redirect links in the code to your own private buckets. However, slight modification of this code (i.e., downloading the data to your local machine instead of uploading/downloading it from s3 buckets) will allow you to run this collection of files without amazon web services. 

Got feedback on our application? Take our [survey](https://forms.gle/Xjbeur68qukaRmFo7) and visit our [public log](https://docs.google.com/document/d/1MvfLFHDhTKoyLuk-cEPwFj8LPZTtdzPLBrkbhbuU38Y/edit)!

Developed in partnership with [Cynthia & George Mitchell](https://cgmf.org/p/home.html) and [T.L.L Temple](https://tlltemple.foundation/) foundations by [Environmental Policy Innovation Center (EPIC)](https://www.policyinnovation.org/). EPIC makes no assurances to the accuracy of the data. All underlying code, methods, and data are available under a Creative Commons License.
