import pandas as pd
import numpy as np
import spacy
from gensim import corpora
from gensim.models import LdaModel
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize

# Set seed for replicability
np.random.seed(42)

# This code generates LDA allocations

tasks  =pd.read_excel("task_taxonomy/training_data/Task Statements.xlsx")["Task"]
stop_words = set(stopwords.words('english'))
texts = [[word for word in word_tokenize(document.lower()) if word.isalpha() and word not in stop_words] for document in tasks]

# Create dictionary
dictionary = corpora.Dictionary(texts)
corpus = [dictionary.doc2bow(text) for text in texts]

num_topics = 100
lda_model = LdaModel(corpus, num_topics=num_topics, id2word=dictionary, passes=300)

all_ = []
for assigns in lda_model.get_document_topics(corpus):
    default =list(np.repeat(0,100))
    for assign in assigns:
        default[int(assign[0])] = assign[1]

    all_.append(default)

pd.DataFrame(all_).to_csv("task_taxonomy/output/task_lda.csv")