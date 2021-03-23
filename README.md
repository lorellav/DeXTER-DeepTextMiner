# DeXTER (DeepTextMiner): A deep learning, critical workflow to contextually enrich digital collections and visualise them
DeXTER (DeepTextMiner) is a deep learning, critical workflow to contextually enrich digital collections and interactively visualise them. It is task-oriented (as opposed to result-oriented) and it is is designed to be generalisable and interoperable (i.e., it is data-set independent). To implement its interoperability, we used language-agnostic algorithms and we are encouraging scholars to replicate the methodology with their own data-sets. Currently, DeXTER is based on [*ChroniclItaly 3.0*](http://doi.org/10.5281/zenodo.4596345) (Viola and Fiscarelli 2021b), an open access digital heritage collection of Italian immigrant newspapers published in the United States from 1898 to 1936. Methodologically, 1) we experimented with different state-of-the-art NLP techniques (e.g., deep learning Named Entity Recognition, deep learning sentiment analysis, network analysis) to enrich the collection with further layers of information (e.g., referential, geographical, emotional, relational); and 2) we developed a [Shiny app](https://shiny.rstudio.com/) to visualise the enriched material and facilitate analysis in an intuitive, interactive, and reproduceable way. This documentation is the step-by-step description of the project. You can find the code and the tutorial in the code folder of this repository. Part of this documentation is taken from [Viola and Fiscarelli](http://ceur-ws.org/Vol-2810/paper5.pdf) (2021a). 

![Network](https://github.com/lorellav/DeXTER-DeepTextMiner/blob/master/Network_3.png) Click [here](https://c2dh.shinyapps.io/dexter/) to use the DeXTER App.

## Table of contents

1. [Deep Learning and the Humanities](#1-deep-learning-and-the-humanities)
2. [Data-set](#2-data-set)
3. [A critical approach to preparing the data](#3-a-critical-approach-to-preparing-the-data)
4. [Enrichment - NER](#4-enrichment-ner)

   4.1 [Critical intervention](#41-critical-intervention)
   
5. [Geo-coding](#5-geo-coding)

   5.1 [Critical intervention](#51-critical-intervention)
 
6. [Sentiment Analysis](#6-sentiment-analysis)

   6.1 [Critical considerations](#61-critical-considerations)

7. [Network analysis](#7-network-analysis)
8. [Installation](#8-installation)
9. [Shiny app](#9-shiny-app)
10. [Remarks](#10-remarks)
11. [License](#license)
12. [Links](#links)
13. [References](#references)
14. [Funding and the team](#funding-and-the-team)
15. [How to cite DeXTER](#how-to-cite-dexter)

## 1. Deep Learning and the Humanities
Digitally available repositories are becoming larger and larger and for the humanities scholar, finding the meaningful elements that are hidden within such an unprecedented mass of digital data is increasingly challenging. At the same time, libraries are also confronted with the challenge to maximise the potential of their collections, to improve the user experience, and to increase retrievability. One way to respond to such challenges is to use deep learning to enrich digital material with contextual information. This process, however promising and continually advancing, still requires technical expertise but more importantly, full critical engagement. While there are many tutorials available, what appears to be urgently needed is a description of a generalizable, critical workflow specifically designed for a non-expert audience that could assist less technical scholars and digital heritage operators with the anything but trivial task of enriching digital sources. DexTER aims to offer just that: a step-by-step, non-technical guide for a versatile method that could be applied transversely across different datasets and could explain the gains and losses of each step and intervention. Specifically, it provides a way to enrich distant reading techniques with the qualitative information necessary for contextualising the results and opening up avenues for interpretation. 

## 2. Data-set
DeXTER currently uses [*ChroniclItaly 3.0*](http://doi.org/10.5281/zenodo.4596345) (Viola & Fiscarelli 2021b), a corpus of Italian language newspapers published in the United States between 1898 and 1936. The corpus includes the digitized front pages of ten Italian language newspapers published in California, Connecticut, Pennsylvania, Vermont, and West Virginia. The collection includes the following titles: *L’Italia*, *Cronaca sovversiva*, *La libera parola*, *The patriot*, *La ragione*, *La rassegna*, *La sentinella del West Virginia*, *L’Indipendente*, *La Sentinella*, and *and La Tribuna del Connecticut*. The collection, which totals 8,653 issues and contains 21,454,455 words, was collected from [*Chronicling America*](https://chroniclingamerica.loc.gov/newspapers/), an Internet-based, searchable database of U.S. newspapers published in the United States from 1789 to 1963 made available by the Library of Congress. *ChroniclItaly 3.0* is available as an open access resource. As its previous versions [*ChroniclItaly*](https://public.yoda.uu.nl/i-lab/UU01/T4YMOW.html) and [*ChroniclItaly 2.0*](https://public.yoda.uu.nl/i-lab/UU01/4MECRO.html), *ChroniclItaly 3.0* features prominenti (mainstream), sovversivi (anarchic), and independent newspapers thus providing a very nuanced picture of the Italian immigrant community in the United States at the turn of the twentieth century.

## 3. A critical approach to preparing the data
Deciding which of the pre-processing operations should be performed depends on many factors such as the language of the data-set, the type of sources, the individual research question(s), the type of enrichment intervention. It is therefore paramount that this step is tackled **critically** by the researcher as each one of their interventions will have consequences on how each algorithm will process the data and therefore on the results. Typical operations that are considered part of this step are tokenization, lowercasing, stemming, lemmatization, removing stopwords, removing noise (e.g., numbers, punctuation marks, special characters). In principle, all these interventions are optional as the algorithms will process whichever version of the data-set is used. In reality, however, pre-processing the digital material is key to the subsequent operations for several reasons. First and foremost, pre-processing the data will remove most OCR mistakes which are always present in digital textual collections to various
degrees. This is especially true for corpora such as historical collections - like in the case of *ChroniclItaly 3.0* -, repositories of under-documented languages, or digitised archives from handwritten texts. Second, it will reduce the size of the collection thus decreasing the required processing power and time. Third, it is de facto a data exploration step which allows the digital heritage scholar to look more closely at the material. It is important to remember that each one of these steps is an additional layer of manipulation and has direct, heavy consequences on the material and therefore on the following operations. It is critical that digital scholars assess carefully to what degree they want to intervene on the material and how. For this reason, this part of the process of contextual enrichment should not be considered as
separate from the enrichment itself, on the contrary, it is an integral part of the entire process.
Here's a short explanation of each operation:
- **Tokenization**: Split the text into sentences and/or the sentences into words. In other words, this operation establishes the word boundaries (i.e., tokens) a very helpful way of finding patterns. It is also the typical step prior to stemming and lemmatization; 
- **Lowercasing**: Lowercase the words. This operation is a double-edged sword. It can be effective at yielding potentially better results in the case of relatively small datasets or datatsets with a high percentage of OCR mistakes. For instance, if lowercasing is not performed, the algorithm will treat *USA*, *Usa*, *usa*, *UsA*, *uSA*, etc. as distinct tokens, even though they may all refer to the same entity. On the other hand, if the dataset does not contain such OCR mistakes, then it may become difficult to distinguish between homonyms and make interpreting the topics much harder;
- **Stemming/Lemmatization**: Reduce inflection in words (e.g. states --> state). Although similar, stemming should not be confused with lemmatization. While the latter reduces the inflected word to the actual root form (e.g., better --> good), the former outputs a canonical form of the original word (e.g., past and future tense --> present tense), and not the grammatical root. Performing or not either of these operations is very much dependant on the dataset's language as in terms of TM, they may not affect the output *per se*;
- **Removing stopwords and words with less than three characters**: Remove low information words. These are typically words such as articles, pronouns, prepositions, conjunctions, etc. which are not semantically salient. There are numerous stopword lists available for many, though not all, languages which can be easily adapted to the individual researcher's needs. Removing words with less than three characters may additionally remove many OCR mistakes. Both these operations have the dual advantage of yielding more reliable results while reducing the size of the dataset, thus in turn reducing the required processing power. This step can therefore hardly be considered optional in TM;
- **Noise removal**: Remove elements such as punctuation marks, special characters, numbers, html formatting, etc. This operation is again concerned with removing elements that may not be relevant to the text analysis and in fact interfere with it. Depending on the dataset and research question, this operation can become essential.

The specific pre-processing actions taken towards enriching *ChroniclItaly 3.0* were: tokenization, removing numbers, dates, removing words with less than two characters and special characters, merging words wrongfully separated by a newline, a white space or punctuation. For more information on the specific pre-processing steps taken on *ChroniclItaly 3.0* please refer to [Viola and Fiscarelli](http://ceur-ws.org/Vol-2810/paper5.pdf) (2021). The code and the scripts used to perform this step can be found in the dedicated folder of this repository.

## 4. Enrichment - NER
One effective application of AI is the possibility to enrich the digital material with data that could allow for in-depth cultural analyses. One example of such text enrichment is deep learning Named Entity Recognition (NER), that is using contextual information to identify referential entities such as names of persons, locations and organisations. *ChroniclItaly 3.0* was tagged for entities using a deep learning [sequence tagging tool](https://github.com/riedlma/sequence_tagging#download-models-and-embeddings) (Riedl and Padó 2018) that implements [Tensorflow](https://www.tensorflow.org/). The Italian language model of the sequence tagging tool was trained on [I-CAB](http://ontotext.fbk.eu/icab.html) (Italian Content Annotation Bank), an open access corpus annotated for entities (i.e. persons, organizations, locations, and geopolitical entities), temporal expressions, and relations between entities. I-CAB contains 525 news articles taken from the newspaper *L'Adige* and totals up around 180,000 words.
Once the training was complete, the output had the following format:

```sh
il      il      KNOWN   O       O
principio      principio      KNOWN   O       O
delle   delle   KNOWN   O       O
ostilità       ostilità       KNOWN   O       O
fra     fra     KNOWN   O       O
la      la      KNOWN   O       O
Spagna  spagna  KNOWN   O       B-GPE
e       e       KNOWN   O       O
gli     gli     KNOWN   O       O
Stati   stati   KNOWN   O       B-GPE
Uniti   uniti   KNOWN   O       I-GPE
.       .       KNOWN   O       O
```

The first column is the input word, the second column specifies the pre-processed, lowercased word, the third column contains a flag, that is whether the word has been known during training (KNOWN) or not (UNKNOWN). If labels are assigned to the input file, these will appear in the third column. The last column contains the predicted tags. The no-entity tag is O. Because some entities (e.g., Stati Uniti "United States") have multiple words, the tagging scheme distinguishes between the beginning (tag B-...) or the inside of an entity (tag I-...). The tags are:

```sh
LOC -> Location
GPE -> Geo-political entity
PER -> Person
ORG -> Organization
```

The NER algorithm retrieved 547,667 entities, which occurred 1,296,318 times across the ten titles. Users are free to choose which deep learning algorithm to use to perform NER. The code and the scripts used to perform this step can be found in the dedicated folder of this repository.

## 4.1 Critical intervention
A close analysis of the entities revealed a number of issues which required a manipulation of the output. These issues included: entities that had been assigned the wrong tag (e.g., New York - PER), multiple entities referring to the same entity (e.g., Woodraw Wilson, President Woodraw Wilson), elements wrongfully tagged as entities (e.g., venerdi ‘Friday’ - ORG). Therefore, a list of these exceptions was compiled and the results adjusted accordingly. Once the data were modified, the data-set counted 521,954 unique entities which occurred 1,205,880 times. This action required a combination of expert knowledge and technical ability as the entities had to be carefully analysed and historically triangulated in order to make informed decisions on how to intervene on the output without introducing errors. Although time-consuming and in principle optional, this **critical** evaluation intervention nevertheless significantly improved the accuracy of the tags thus overall increasing the quality of the NER output in preparation for the following stages of the enrichment, thus adding more value to the user’s experience for access and reuse. It is therefore a highly recommended operation. The code and the scripts used to perform this step can be found in the dedicated folder of this repository.

## 5. Geo-coding
The relevance of geo-coding for digital heritage collections lies in what has been referred to as Spatial turn, the study of space and place as distinct entities in
that it sees place as created through social experiences and can therefore be both real and imagined whereas space is essentially geographic. Based on Geographic Information Systems (GIS), locations in data-sets are geo-coded and displayed on a map. Especially in the case of large collections with hundreds of thousands of geographical entities, the visualisation is believed to help scholars access the different layers of information that may be behind geo-references. The theoretical relevance of using NER to enrich digital heritage collections lies precisely in its great potential for discovering the cultural significance underneath referential units and how that may have changed over time. A challenge posed to digital heritage scholars is that of the language of the collection. In the case of *ChroniclItaly 3.0*, for example, almost all choices
made by the researcher towards enriching were conditioned by the fact that the language of the collection is not English. The relative lack of appropriate computational resources available for languages other than English often dictates which tools and platforms can be used for specific tasks. For this reason, the geographical entities in *ChroniclItaly 3.0* have been geocoded using the Google Cloud Natural Language API within the Google Cloud Platform Console [Google Cloud Platform Console](https://cloud.google.com/console/google/maps-apis/overview) which provides a range of NLP technologies in a wide range of languages, including Italian. Users are free to choose the geo-coding tool most suitable to them. For more details on how to perform geo-coding with Google API, please visit [Google Geocoding API](https://developers.google.com/maps/documentation/geocoding/start). A description of the challenges encountered in this process can be found [here](https://github.com/lorellav/GeoNewsMiner#geocoding-with-google-api).

In this workflow, we decided to geo-code only GPE entities. Though not optimal, the decision was made also considering that GPE entities are generally more informative as they would typically refer to countries and cities (though it was found to retrieve also counties and States) while LOC entities are typically rivers, lakes, and geographical areas (e.g., the Pacific Ocean). Depending on individual researcher's needs, users are free to geo-code LOC-entities as well. The code and the scripts used to perform this step can be found in the dedicated folder of this repository.

## 5.1 Critical intervention
In addition to the geo-coordinates, Google also provides additional details, such as the tag *type[]* which indicates why Google has provided certain geo-coordinates to a location. Some of these geo-coordinates may have been "misinterpreted", therefore understanding the *type[]* of a location is important. "Misinterpretations" may happen especially when working with historical data. Because the Google Places database stores places based on a contemporary world map, the locations in a historical dataset may have changed name or may no longer exist. Moreover, categories such as country, city, region, municipality, etc. which Google uses to determine the location *type[]* are highly dependent on the location itself and consequently, certain categories may not apply or they may change from place to place.

For example, within the level *country*, Google distinguishes between five sub-levels: `administrative_area_level_1`, `administrative_area_level_2`, `administrative_area_level_3`, and so on. Generally speaking, the levels proceed from bigger (level 1) to  smaller (level 5). The type `locality` corresponds to a city or town. In the United States, for instance, level_1 would correspond to a State, level_2 to a county and so on. However, not every country would have States, counties, etc. In geocoding the places in *ChroniclItaly 2.0*, we encountered cases in which the level given by Google required a manual edit. Specifically, three main types of manual edits needed to be performed:
#### - Changing the level to a higher level:
This was the case of cities like Naples which Google tagged as `administrative_area_level_2` (municipality) instead of `locality`;
#### - Aggregating locations according to a higher level:
This was the case of places like *S. Pietro* -the square in Vatican City -or *New York Harbor*. In these cases, we manually changed the type (`establishment`) to match that of the corresponding country or city (`locality`);
#### - Changing a wrong *type[]*:
This was the case of places that no longer exist or that have changed name. An example of this kind is the city of *Zara* in Croatia which today is called Zadar and which was a province of the Kingdom of Italy from 1918 to 1947. However, Google tagged Zara as `establishment` and gave it the geo-coordinates of a Zara department store in the United States.

All the locations that have been manually edited are marked in red in the file `data/output_20191622_041444_geocoding_edit.xlsx`. This file can be compared with the file `output/output_20191622_041444_geocoding.xlsx` to see the original output. For an overview of all the possible *types[]* tags, please refer to the the [Google Maps Platform documentation](https://developers.google.com/maps/documentation/geocoding/intro).

## 6. Sentiment analysis
Another enriching technique used to add value to digital heritage collections is Sentiment Analysis (SA), a way to identify the prevailing emotional attitude of the writer towards for example referential entities. This allows users to identify the layers of meaning humans attached historically to people, organisations, and geographical spaces. Understanding the meaning humans invested in such entities and, in the case of historical collections such as *ChroniclItaly 3.0*, how that meaning may have changed over time, provides digital heritage scholars with powerful means to access part of collective narratives of fear, pride, longing, loss. 
The process of performing SA on the collection required several steps:

– Identify the sentence delimiters (i.e., full stop, semicolon, colon, exclamation mark, question mark) and divide the textual material accordingly. At the
end of this step, 677,030 sentences were obtained;

– Select the most frequent entities for each category and in each title. As each title differs in size, we used a logarithmic function to obtain a more
representative number of entities per title (2\*log2 function used). At the end of this step, 228 entities were obtained;

– Select only the sentences that contained the entities identified in the previous step. This step was done to limit the number of API requests and reduce processing time and costs. The selection returned 133,281 sentences;

– Perform SA on the sentences so selected. When the study was carried out, no suitable SA models trained on Italian were found, therefore this step was performed once again using the Google Cloud Platform Console. 

The code and the scripts used to perform this step can be found in the dedicated folder of this repository.

## 6.1 Critical considerations

## 7. Network analysis
Network analysis allows us to observe the patterns of connection between entities in a corpus, i.e., to analyse how entities interact with each other, rather than focussing on the entities in isolation. In DeXTER, entities are built in a co-occurrence network, where the nodes represent the entities cited in the corpus as connected if appearing in the same sentence. Therefore, the edges represent the sentences in which the entities co-occur. Several attributes are assigned to each edge/sentence, such as the issue containing said sentences, the timestamp, the co-occurrence frequency, the sentiment score and magnitude. Conversely, each node/entity has several attributes, that are computed by aggregating all edges connected to it. For example, the occurrence frequency of an entity in the corpus is given by the sum of the co-occurrence frequencies of all edges connected to it, while the sentiment score and magnitude are given by the mean value.
These interactions are not static because the network is built in a way such that when changing the time filter, the obtained network represents the interaction between entities in the selected time frame.
All the visualizations in the [DeXTER Shiny app](https://c2dh.shinyapps.io/dexter/) are based on this co-occurrence network and can be explored in two different ways.
The first one, called ego-network, focusses on entities. The ego network of an entity consists of the neighbouring nodes of the entity and all edges connecting them. This is particurarly useful when the user is interested in a specific entity and in how it interacts with other entities. The second one, called issue-network, focusses on issues. In this case, the network consists of all nodes/entities that appear in one or more issues, chosen by the user, and all edges connecting them. This is useful when the user is interested in exploring how events were discussed on a particular day/issue.

## 8. Installation
Please install R 3.6.3 (2020-02-29) or higher to run the tutorial. The tutorial installs all the necessary libraries which can be found at the beginning of the file dexter.R.

## 9. Shiny app

![Network](https://github.com/lorellav/DeXTER-DeepTextMiner/blob/master/Network_3.png)

To explore the data, a Shiny app was developed. A [live version](https://c2dh.shinyapps.io/dexter/) of the DeXTER app is available open access. The DeXTER app is an interactive tool which allows users to explore references to place names (e.g., city, region, and/or country), their distribution and relations (i.e., co-occurrence, sentiment) across titles, issues, over time by selecting a wide range of filters, as follows:

-	Time: from 1898 to 1936;
-	Newspaper’s title;
-	Type of frequency visualization: absolute, percentage;
-	Geographical information: include/exclude references to cities/regions;
-	Percentile: select the least/most mentioned entity;
-	Entity information: include/exclude specific entities;
-	Sentiment information: select polarity, sentence co-occurrence frequency, top entities, over time trend;
-	Network information: ego-network and issue network;
-	Choose the map: 1880, 1914, 1920, 1994.

Users can also share their results, download the raw data which may reflect their filters’ selection, and download the visualisations as .png files.

The Shiny app requires R. Please install the dependencies found in [dexter_shiny_app/global.R](https://git.science.uu.nl/research-it-support/viola_geocoding_history/blob/master/gnm_shiny_app/global.R). The source code of the app can be found in the [/dexter_shiny_app](/gnm_shiny_app) folder. 

## 10. Remarks
Please see [Viola and Fiscarelli](http://ceur-ws.org/Vol-2810/paper5.pdf) (2021a) for further technical remarks regarding issues such as OCR, NER F1 score, collection compostion, etc. 

## 11. License
The software is licensed under GPL-3. 

## 12. Links 
- The DeXTER app: https://c2dh.shinyapps.io/dexter/
- Historical maps: http://web.archive.org/web/20080328104539/http:/library.thinkquest.org:80/C006628/download.html (No longer available in the orginal location. Copy available on webarchive)
- *ChroniclItaly*: https://doi.org/10.24416/UU01-T4YMOW
- *ChroniclItaly 2.0*: https://doi.org/10.24416/UU01-4MECRO
- *ChroniclItaly 3.0*: http://doi.org/10.5281/zenodo.4596345.
- Interactive applications with Shiny: https://shiny.rstudio.com/
- Sequence tagging with Tensorflow: https://github.com/riedlma/sequence_tagging#download-models-and-embeddings
- Italian Content Annotation Bank (I-CAB): http://ontotext.fbk.eu/icab.html
- Google Cloud Platform Console: https://console.cloud.google.com/?pli=1

## 13. References

Riedl, M. and Padó, S. 2018. "A Named Entity Recognition Shootout for German". *Proceedings of the 56th Annual Meeting of the Association for Computational Linguistics (Short Papers)*, pages 120–125. Melbourne, Australia, July 15 - 20, 2018.

Viola, L. 2021. “*ChroniclItaly* and *ChroniclItaly 2.0*: Digital Heritage to access narratives of migration”. *IJHAC - Journal of Digital Humanities*.

Viola, L. and Fiscarelli, A. 2021a. "From digitised sources to digital data: Behind the scenes of (critically) enriching a digital heritage collection". In *Proceedings of the International Conference Collect and Connect: Archives and Collections in a Digital Age*, edited by Weber, A. Heerlien, M., Miracle, E. G. and Wolstencroft, K. CEUR – Workshops Proceedings.

Viola, L. and Fiscarelli, A. 2021b. *ChroniclItaly 3.0. A deep-learning, contextually enriched digital heritage collection of Italian immigrant newspapers published in the USA, 1898-1936*. (Version v3.0.0) [Data set]. Zenodo. http://doi.org/10.5281/zenodo.4596345.

## 14. Funding and the team

DeXTER is a project by [Lorella Viola](https://www.lorellaviola.me.uk). This project was funded by the Luxembourg Centre for Contemporary and Digital History (C<sup>2</sup>DH) [*Thinkering Grant*](https://www.c2dh.uni.lu/thinkering). The technical implementation including the development of the app was provided by [Antonio Maria Fiscarelli](https://github.com/AntonioFiscarelli).

## 15. How to cite DeXTER

To cite the DeXTER repository, please use the following format according to the APA style guide:

Viola, Lorella and Fiscarelli, Antonio Maria. 2021. *DeXTER (DeepTextMiner): A deep learning, critical workflow to contextually enrich digital collections and visualise them* (v1.0.0). Luxembourg: University of Luxembourg. Available at https://github.com/lorellav/DeXTER-DeepTextMiner. DOI: 



