# DeXTER (DeepTextMiner): A deep learning, critical workflow to contextually enrich digital heritage collections and visualise them
DEXTER (DeepTextMiner) is a deep learning, critical workflow to contextually enrich digital heritage collections and interactively visualise them. As it is task-oriented rather than result-oriented, DEXTER is designed to be generalisable and interoperable (i.e., it is dataset-independent). To demonstrate its interoperability, we used a variety of datasets in a range of languages (i.e., [*ChroniclItaly*](https://public.yoda.uu.nl/i-lab/UU01/T4YMOW.html) and subsets of the [*Impresso Project*](https://impresso-project.ch/app/newspapers/) collections). These are open access resources. Methodologically, 1) we experimented with different state-of-the-art NLP techniques (e.g., deep learning Named Entity Recognition, deep learning sentiment analysis, network analysis) to enrich digital collections with further layers of information (e.g., referential, geographical, emotional, relational); and 2) we developed a [Shiny app](https://shiny.rstudio.com/) to present and analyse the results in an intuitive, interactive, and reproduceable way. This documentation is the step-by-step description of the project.

DeXTER is also an open access app. Click [here](https://utrecht-university.shinyapps.io/GeoNewsMiner/) to use the DeXTER App.

## Table of contents

1. [Deep Learning and the Humanities](#1-deep-learning-and-the-humanities)
2. [Data-set](#2-data-set)
3. [Pre-processing](#3-pre-processing)
4. [Enrichment - NER](#4-enrichment-ner)

   4.1[Critical intervention](#41-critical-intervention)
   
5. [Geocoding with Google API](#5-geocoding-with-google-api)
6. [Entity Sentiment Analysis](#6-entity-sentiment-analysis)

   6.1[Critical intervention](#61-critical-intervention)

7. [Network analysis](#7-network-analysis)
8. [Installation](#8-installation)
9. [Shiny app](#9-shiny-app)
10. [Remarks](#10-remarks)
11. [License](#license)
12. [Links](#links)
13. [References](#references)
14. [The team](#the-team)
12. [How to cite DeXTER](#how-to-cite-dexter)

## Deep Learning and the Humanities
As digitally available textual repositories are becoming larger and larger, the relevance of distant reading for the humanities has grown exponentially. As a direct consequence, humanities scholars are more and more confronted with the challenge of having to apply quantitative approaches in their research, for traditional close reading methods are no longer suitable for the analysis of such unprecedented mass of digital data. At the same time, libraries are resorting more and more to machine learning (ML) methods to maximise the potential of their collections, to improve the user experience, and to discover the technical requirements necessary to facilitate the exploration and use of digital collections. One effective application of ML is enriching digital data with further layer of information (e.g., geographical, historical, topical, sentiment). While there are many TM programs and tutorials available, what appears to be still missing is a description of a generalizable workflow for the humanities. This repository offers a step-by-step guidance for a versatile method that could be applied transversely across different datasets. Specifically, it provides a way to enrich distant reading techniques with the qualitative information necessary for contextualising the results and opening up avenues for interpretation. 
 
