# DeepTextMiner (DEXTER): A deep learning workflow to contextually enrich digital heritage collections
DEXTER (DeepTextMiner) is a deep learning workflow to contextually enrich digital heritage collections and interactively visualise them. As it is task-oriented rather than result-oriented, DEXTER is generalisable and interoperable (i.e., it is dataset-independent). To demonstrate its interoperability, we used a variety of datasets in a range of languages (i.e., [*ChroniclItaly*](https://public.yoda.uu.nl/i-lab/UU01/T4YMOW.html) and subsets of the [*Impresso Project*](https://impresso-project.ch/app/newspapers/) collections). These are open access resources. Methodologically, 1) we experimented with different state-of-the-art NLP techniques (e.g., deep contextualized word-embeddings, deep learning sentiment analysis, network analysis, topic modelling) to enrich digital collections with further layers of information (e.g., geographical, historical, topical, sentiment); 2) we critically assessed these techniques' models/accuracy, and 3) we developed a [Shiny app](https://shiny.rstudio.com/) to present and analyse the results in an intuitive, interactive, and reproduceable way. This documentation is the step-by-step description of the project.

DEXTER is also an open access app. Click [here](https://utrecht-university.shinyapps.io/GeoNewsMiner/) to use the GNM App.

## Table of contents

1. [Deep Learning and the Humanities](#deep-learning-and-the-humanities)
2. [*ChroniclItaly*](#chroniclitaly)
3. [Sequence Tagging](#sequence-tagging)
4. [Geocoding with Google API](#geocoding-with-google-api)
5. [Installation](#installation)
6. [Shiny app](#shiny-app)
7. [Remarks](#remarks)
8. [License](#license)
9. [Links](#links)
10. [References](#references)
11. [The team](#the-team)
12. [How to cite GNM](#how-to-cite-gnm)

## Deep Learning and the Humanities
As digitally available textual repositories are becoming larger and larger, the relevance of distant reading for the humanities has grown exponentially. As a direct consequence, humanities scholars are more and more confronted with the challenge of having to apply quantitative approaches in their research, for traditional close reading methods are no longer suitable for the analysis of such unprecedented mass of digital data. At the same time, libraries are resorting more and more to machine learning (ML) methods to maximise the potential of their collections, to improve the user experience, and to discover the technical requirements necessary to facilitate the exploration and use of digital collections. One effective application of ML is enriching digital data with further layer of information (e.g., geographical, historical, topical, sentiment). While there are many TM programs and tutorials available, what appears to be still missing is a description of a generalizable workflow for the humanities. This repository offers a step-by-step guidance for a versatile method that could be applied transversely across different datasets. Specifically, it provides a way to enrich distant reading techniques with the qualitative information necessary for contextualising the results and opening up avenues for interpretation. 
 
