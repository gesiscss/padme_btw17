# PADME: Online reproducibility using PADME and GESIS Notebooks

## Demonstrator code for the Paper
PADME-SoSci: A Platform for Analytics and Distributed Machine Learning for the Social Sciences, by Zeyd Boukhers, Arnim Bleier, Yeliz Ucer Yediel, Mio Hienstorfer-Heitmann, Mehrshad Jaberansary, Adamantios Koumpis, and Oya Beyan.
https://arxiv.org/abs/2303.18200

This repository provides a schema analysis of tweets by and to German political accounts during the last hours of the German federal election 2017. Its purpose is to demonstrate how an online reproducibility service such as [GESIS Notebooks](https://notebooks.gesis.org/) provides a schematic analysis that researchers can submit to the analytics and machine learning platform [PADME](https://padme-analytics.de/ŧ) to perform the same analysis with private data.


## Included Materials
1. Twitter Sentiment Analysis ([notebook](twitter_analysis.ipynb))
2. Sample sentiment data per hour ([CSV file](data/sentiment_data_hour.csv))
3. Sample sentiment data per minute ([CSV file](data/sentiment_data_minute.csv))
4. Further dictionary data in "data/SentiWS*"


## Online Analysis Environment
[![GESIS Notebooks](https://notebooks.gesis.org/static/images/logo/logo_text.png)](https://notebooks.gesis.org)

You can execute the project online at [GESIS Notebooks](https://notebooks.gesis.org/)

+ To launch the analysis for one-time usage click  
[![Binder](https://notebooks.gesis.org/binder/badge.svg)](https://notebooks.gesis.org/binder/v2/gh/gesiscss/padme_btw17/HEAD?labpath=twitter_analysis.ipynb)



## About

Materials are licensed under [CC BY-SA 4.0](http://creativecommons.org/licenses/by-sa/4.0/).

[![Creative Commons Lizenzvertrag](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)](http://creativecommons.org/licenses/by-sa/4.0/)


This work extends the contents and data used in www.github.com/gesiscss/btw17_sample_scripts

## References

*R. Remus, U. Quasthoff & G. Heyer*. **SentiWS - a Publicly Available German-language Resource for Sentiment Analysis.** In: *Proceedings of the 7th International Language Ressources and Evaluation (LREC'10)*, 2010

*Sebastian Stier, Arnim Bleier, Malte Bonart, Fabian Mörsheim, Mahdi Bohlouli, Margarita Nizhegorodov, Lisa Posch, Jürgen Maier, Tobias Rothmund, Steffen Staab*. **Systematically monitoring social media: The case of the German federal election 2017** ([pdf](https://arxiv.org/pdf/1804.02888.pdf), [ZA6926](https://search.gesis.org/research_data/ZA6926))
