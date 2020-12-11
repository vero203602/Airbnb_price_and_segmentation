# Airbnb_price_and_segmentation

The repository includes the complete code - trials and final - for my Final project.

### Title: Sharing your house, at what  cost?
#### Subtitle: Analysis of a pricing model and customer segmentation for Airbnb

##### Research question:  
##### Provide a good base-price for listing in Airbnb and observe the most significant attributes. Compare the results with findings from previous studies. 
##### Check habits and tendencies of Italian tourists. Firstly, clustering is performed according to purchase history (RFM, k-means). Eventually, socio-demographic attributes are also explored, to understand if these variables are significant. 

***

Data from Airbnb is directly dowloaded from the website and can be used under a Creative Commons CC0 1.0 Universal (CC0 1.0) “Public Domain Dedication” license. In particular, the **calendar** data sets and the **listing** ones (2019) are all downloaded from:
<http://insideairbnb.com/get-the-data.html>


Data for segmentation is retrieved ISTAT, the Italian, National Institute of Statistics, and can be used under a licence Creative Commons – At- tribution – 3.0 version. The link for the data follows: 
<https://www.istat.it/it/archivio/178695>

***

The repository includes different parts.  
Airbnb data cleaning and price prediction:  
* EDA and DataViz.ipynb
* cleaning_listing.R
* predictionfinale.R  

The first file provides a general overview on the data, with plot and maps (*EDA*). Then, the *cleaning_listing* works both on calendar and listing files, in order to create the final set of information. The main problem addressed here are data cleaning (especially imputation of NAs and identification of outliers) and feature engineering. *predictionfinale* contains the final analysis: price prediction and variable importance detection.


The second part includes a unique script for customer segmentation:  
* segmentation.R  

This .R script includes both EDA and data cleaning and analysis.
***
