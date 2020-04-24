# DeepTextMiner (DTM): A deep learning workflow to contextually enrich digital heritage collections
The DeepTextMiner (DTM) is a deep learning workflow to contextually enrich digital heritage collections and interactively visualise them. The overarching goal of this project is to devise a generalisable, interoperable workflow that could assist researchers in both these tasks. As a use case, we used Italian immigrant newspapers published in the United States from 1898 to 1920, as collected in the corpus [*ChroniclItaly*](https://public.yoda.uu.nl/i-lab/UU01/T4YMOW.html) (Viola 2018). The corpus was previously tagged for entities using a sequence tagging tool ([Riedl and Padó 2018](https://www.aclweb.org/anthology/P18-2020.pdf)). This tagged version of *ChroniclItaly* is [*ChroniclItaly 2.0*](https://public.yoda.uu.nl/i-lab/UU01/4MECRO.html) and it is available as an open access resource. Methodologically, we experimented with different state-of-the-art NLP techniques (e.g., deep contextualized word-embeddings, deep learning sentiment analysis, network analysis, topic modelling) to enrich digital collections with further layers of information (e.g., geographical, historical, topical, sentiment) and critically assessed their models/accuracy. As a visualization tool, we developed a [Shiny app](https://shiny.rstudio.com/) to present and analyse the results in an intuitive, interactive, and reproduceable way. This documentation is the step-by-step description of the project.

DTM is available as an open access app. Click [here](https://utrecht-university.shinyapps.io/GeoNewsMiner/) to use the GNM App.


As digitally available textual repositories are becoming larger and larger, the relevance of distant reading for the humanities has grown exponentially. As a direct consequence, humanities scholars are more and more confronted with the challenge of having to apply quantitative approaches in their research, for traditional close reading methods are no longer suitable for the analysis of such unprecedented mass of digital data. One such quantitative approach is Topic Modelling (TM), a computational, statistical method to discover patterns and topics in large collections of unstructured text. While there are many TM programs and tutorials available, what appears to be still missing is a description of a generalizable TM workflow for the humanities. With this repository, the DHARPA Project aims to offer a step-by-step guidance for a versatile method that could be applied transversely across different datasets. Specifically, it provides a way to enrich the distant reading technique of TM with the qualitative information necessary for contextualising the TM results and opening up avenues for interpretation. This workflow is partially based on Viola and Verheul (2019). Advances in machine learning (ML) are allowing researchers
both in computer science and the humanities 
to develop new tools and methods for exploring
digital collections. At the same time, libraries are resorting
more and more to ML methods to maximise
the potential of their collections, to improve the user
experience, and to discover the technical requirements
necessary to facilitate the discovery and use of
digital collections. One effective application of ML is
enriching digital data with geographical information.
 
