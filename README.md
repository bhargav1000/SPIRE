# SPIRE (Simplified Package Info Risk Engine)
SPIRE is a risk identifier which looks at the installed apps on a customer's smartphone and identifies how risky the customer is, this helps determine the loan amount that can be given to a customer. The apps are collected through an app or a third party software. All the apps are collected ethically and after customer consent. Care has been taken to ensure that no extraneous information is collected. The apps are used both individually and as flags, which are basically whether any of the apps in a particular condition are present or not and as counts, these are the number of apps present in a particular category. I cannot under current circumstances reveal these apps and conditions as I am still subject to conditions under a Non-Disclosure Agreement which I have signed. 

An xgboost model was developed based on the apps, flags and counts to identify the risk value based on the delinquency value which just states whether the customer is risky or not. The risk quartiles are decided on the probabililty of delinquency of the customer. I have used build data which covers 3 months of data and oot (out of time) data or out of box data to evaluate model performance. The resulting model had an AUC of 0.6 (I cannot reveal accuracy due to NDA reasons), which is pretty good considering that the data is unpredictable and has a lot of variance with each month having a cohort of customers with different behaviours. This model is able to accurately detect the risk level on a customer without compromising other aspects of the customer, rather SPIRE provides a whole new dimension to assess a customer's delinquency potential. This does not mean that a customer must be avoided, it means that a customer with a lower risk level must be selected for a higher loan amount and a highly risky customer (a customer who has too many gambling apps and such) must be given a lower loan amount.

# Requirements
The following R libraries are required:
- plyr
- dplyr
- xgboost
- pROC
- stringr
- DiagrammeR
- beepr (to keep track of the stages of execution of the program, this is an extra library and does not affect the program directly)
- readxl (for reading excel files, this is an optional library if you intend to use only CSV files)

# Getting Started
SPIRE can be run directly in R studio and the resulting visualizations can be used to evaluate the model.

 