# DeXTER (DeepTextMiner): A deep learning, critical workflow to contextually enrich digital collections and visualise them
DeXTER (DeepTextMiner) is a deep learning, critical workflow to contextually enrich digital collections and interactively visualise them. It is task-oriented (as opposed to result-oriented) and it is is designed to be generalisable and interoperable (i.e., it is data-set independent). To implement its interoperability, we used language-agnostic algorithms and we are encouraging scholars to replicate the methodology with their own data-sets. Currently, DeXTER is based on [*ChroniclItaly 3.0*](https://public.yoda.uu.nl/i-lab/UU01/T4YMOW.html), an open access digital heritage collection of Italian immigrant newspapers published in the United States from 1898 to 1936. Methodologically, 1) we experimented with different state-of-the-art NLP techniques (e.g., deep learning Named Entity Recognition, deep learning sentiment analysis, network analysis) to enrich the collection with further layers of information (e.g., referential, geographical, emotional, relational); and 2) we developed a [Shiny app](https://shiny.rstudio.com/) to visualise the enriched material and facilitate analysis in an intuitive, interactive, and reproduceable way. This documentation is the step-by-step description of the project. You can find the code and the tutorial in the code folder of this repository. Part of this documentation is taken from [Viola and Fiscarelli](http://ceur-ws.org/Vol-2810/paper5.pdf) (2021). 

Click [here](https://c2dh.shinyapps.io/dexter/) to use the DeXTER App.

## Table of contents

1. [Deep Learning and the Humanities](#1-deep-learning-and-the-humanities)
2. [Data-set](#2-data-set)
3. [Pre-processing](#3-pre-processing)
4. [Enrichment - NER](#4-enrichment-ner)

   4.1 [Critical intervention](#41-critical-intervention)
   
5. [Geocoding with Google API](#5-geocoding-with-google-api)
6. [Entity Sentiment Analysis](#6-entity-sentiment-analysis)

   6.1 [Critical intervention](#61-critical-intervention)

7. [Network analysis](#7-network-analysis)
8. [Installation](#8-installation)
9. [Shiny app](#9-shiny-app)
10. [Remarks](#10-remarks)
11. [License](#license)
12. [Links](#links)
13. [References and further readings](#references-and-further-readings)
14. [The team](#the-team)
12. [How to cite DeXTER](#how-to-cite-dexter)

## 1. Deep Learning and the Humanities
Digitally available repositories are becoming larger and larger and for the humanities scholar, finding the meaningful elements that are hidden within such an unprecedented mass of digital data is increasingly challenging. At the same time, libraries are also confronted with the challenge to maximise the potential of their collections, to improve the user experience, and to increase retrievability. One way to respond to such challenges is to use deep learning to enrich digital material with contextual information. This process, however promising and continually advancing, still requires technical expertise but more importantly, full critical engagement. While there are many tutorials available, what appears to be urgently needed is a description of a generalizable, critical workflow specifically designed for a non-expert audience that could assist less technical scholars and digital heritage operators with the anything but trivial task of enriching digital sources. DexTER aims to offer just that: a step-by-step, non-technical guide for a versatile method that could be applied transversely across different datasets and could explain the gains and losses of each step and intervention. Specifically, it provides a way to enrich distant reading techniques with the qualitative information necessary for contextualising the results and opening up avenues for interpretation. 
 
