---
title: "CTD_project"
author: "Robert Schlegel"
date: "14 June 2018"
output: html_document
---

# CTD project

The purpose of this as yet unnamed project is to create a shiny app that will allow users direct access to 4D CTD data products and other similar things. This repo also houses the pipeline that creates these products and makes them available for the shiny app to then bring them online to the users.

## The data

Currently the data in this project are limited to what I was able to scavenge for an MSc project in 2013. Unfortunately this means that the time series are not terribly long, stretching from roughly 1985 to 2005. There are DAFF small pelagic (November only) data as well as a hodgepodge collection of DEA cruise data. Note that throughout the text and code in this project I refer to all of the data as CTD data. This is not technically correct as there are data present in these products from many different instruments. The type of instrument used to collect the data is shown in the `type` column and the user may choose how they would like to filter the instruments accordingly.

## The pipeline

It is through the data pipeline in this repo that these disparate data sources are taken up into the central project structure, cleaned, and the final product is created. This pipeline is written in such a way that it should be self perpetuating. Meaning that it will stand as is and when new data are available they must just be placed in the correct folders and the pipeline re-run in order to produce the products as desired.

## The shiny app

The basic fnctionality that this app serves to address is that not only can scientists simply access these governmental data (which is their right to do), it also facilitates the process of visualising, exctracting, and saving these products as they require for their own projects.

## The metadata

The metadata, in as much is available, for all of the data products may be found here.

## Updates

* June 14th, 2018
    * Repository created

* June 17th, 2018
    * Updated the app to show different colours as well as the land mask
    
* July 6th, 2018
    * Major updates to pipeline, products, and app

* August 6th, 2018
    * Added more DAFF and DEA cruise meta-data