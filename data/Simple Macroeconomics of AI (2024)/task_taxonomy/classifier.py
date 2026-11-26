import pandas as pd
import numpy as np
import spacy

from gensim import corpora
from gensim.models import LdaModel
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
import nltk

from sklearn.ensemble import HistGradientBoostingClassifier 

# Set seed for replicability
np.random.seed(42)


# This function extracts the primary verbs of tasks
def v_extract(sentence): 
    words =sentence.replace("'","").replace(".","").split(" ")
    v_s = []
    if "," in words[0]:
        v_s.append(words[0].replace(",",""))
        if "," in words[1]:
            v_s.append(words[1].replace(",",""))
            if "," in words[2]:
                v_s.append(words[1].replace(",",""))
            else:
                if words[2] == "or":
                    v_s.append(words[3])   
    else:
        v_s.append(words[0])
        try:
            if words[1] == "or":
                v_s.append(words[2])   
        except:
            j = 0
    
    
    
    return v_s

# Importing key data
nlp = spacy.load("en_core_web_sm")
tasks  =pd.read_excel("task_taxonomy/training_data/Task Statements.xlsx")["Task"]

v_list = []
# Process the text to extract verbs
for task in tasks :
    doc = nlp(task)
    # Extract verbs
    verbs_ = v_extract(task)
    v_list.append(verbs_)

tasks_verb =pd.DataFrame(tasks)
tasks_verb["verbs"] = v_list
lens = []
# Document how many key verbs there are in the task
for v in v_list:
    lens.append(len(v))
tasks_verb["num"] = lens

# Creata dataset merging information about verbs
j=0
ids = []
vvs = []
for verbs in tasks_verb["verbs"]:
    for verb in verbs:
        ids.append(j)
        vvs.append(verb)

    j=j+1

verbs_main =pd.DataFrame(ids)
verbs_main["verbs"] = vvs
l_v = []
# This standardizes verbs so that capitalization doesn't matter.
for v in vvs:
    l_v.append(v.lower())
verbs_main["clean"] = l_v

# Loading the training data for the algorithm
tasks_aut = pd.read_csv("task_taxonomy/training_data/eloundou_task.csv")
trained =pd.read_excel("task_taxonomy/training_data/train_set_all.xlsx")

# Focus on tasks that are exposed
tasks_aut["automation"] =  (tasks_aut["gpt4_automation"]>.5)
all_to_classify =tasks_aut[tasks_aut["automation"]>0].reset_index(drop=True)[["Task ID"]]

# Merging verb data
task_index =pd.read_excel("task_taxonomy/training_data/Task Statements.xlsx")[["Task ID"]].reset_index(drop=False)
verbs_main["index"] = verbs_main[0]
tv_all =task_index.merge(verbs_main, on = "index")[["Task ID","clean"]]
tv= all_to_classify.merge(tv_all, on = "Task ID")
# Manual classifications of verbs
man_class =pd.read_excel("task_taxonomy/training_data/verb_label.xlsx")
man_class["Unnamed: 2"] = man_class["Unnamed: 2"].fillna(" ")
man_class["man_verb_hard"]= (man_class["Unnamed: 2"] == "H")*1
man_class["man_verb_easy"]= (man_class["Unnamed: 2"] == "E")*1
man_class = man_class[["clean","man_verb_hard","man_verb_easy"]]
# Observed classifications of verbs 
aut_class =tv.merge(trained[["Task ID","Easy = 1"]], on = "Task ID").groupby("clean")["Easy = 1"].mean().reset_index(drop=False)
aut_class =tv_all.merge(aut_class, on = "clean", how = "left").groupby("Task ID")["Easy = 1"].mean().reset_index(drop=False).fillna(0)
verb_class=tv_all.merge(man_class, on = "clean", how = "left").groupby("Task ID")[["man_verb_hard","man_verb_easy"]].mean().reset_index(drop=False).fillna(0)
verb_class["verb_aut_class"] = aut_class["Easy = 1"]
# Merging IWA data with tasks
iwa_dwa =pd.read_excel("task_taxonomy/training_data/DWA Reference.xlsx")[["IWA ID","DWA ID"]]
dwa_task =pd.read_excel("task_taxonomy/training_data/Tasks to DWAs.xlsx")[["Task ID","DWA ID"]]
dwa_task = task_index.merge(dwa_task, on ="Task ID", how = "left")
dwa_classify =dwa_task.merge(iwa_dwa, on = "DWA ID", how = "left")
# Merging manual labelling
iwa_label =pd.read_excel("task_taxonomy/training_data/IWA_label.xlsx")
iwa_label["IWA Hard"] = (iwa_label["H/E"] == "H")*1
iwa_label["IWA Easy"] = (iwa_label["H/E"] == "E")*1
# Merging observed (empirical) classifications
iwa_merged =dwa_classify.merge(iwa_label[["IWA ID","IWA Hard","IWA Easy"]].fillna(0), on = "IWA ID", how = "left")[["Task ID","IWA Hard","IWA Easy"]].groupby("Task ID").mean()
iwa_merged["sum"] =iwa_merged["IWA Hard"] + iwa_merged["IWA Easy"]
iwa_merged["sum"] =iwa_merged["sum"]+ 1*(iwa_merged["sum"] == 0)
iwa_merged["IWA Hard"] = iwa_merged["IWA Hard"].fillna(0) /iwa_merged["sum"]
iwa_merged["IWA Easy"] = iwa_merged["IWA Easy"].fillna(0)  /iwa_merged["sum"]
iwa_merged = iwa_merged.reset_index(drop=False)[["Task ID","IWA Hard","IWA Easy"]]
iwa_aut =dwa_classify[["Task ID","IWA ID"]].merge(trained[["Task ID","Easy = 1"]], on = "Task ID", how = "left").groupby("IWA ID")["Easy = 1"].mean().reset_index(drop=False)
iwa_aut =dwa_classify[["Task ID","IWA ID"]].merge(iwa_aut, on = "IWA ID", how = "left").groupby("Task ID")["Easy = 1"].mean().reset_index(drop=False).fillna(0)
iwa_merged["IWA Empirical"] = iwa_aut["Easy = 1"]
# Merging LDA
lda = pd.read_csv("task_taxonomy/output/task_lda.csv")
lda["Task ID"] = task_index["Task ID"]
master_0 =iwa_merged.merge(lda.drop("Unnamed: 0",axis=1),on="Task ID", how ="left")
master_0 =master_0.merge(verb_class, on = "Task ID", how = "left")

# Training Model
X_train = pd.DataFrame(trained["Task ID"]).merge(master_0, on = "Task ID", how = "left").groupby("Task ID").mean().reset_index(drop=True)
y_train = trained["Easy = 1"]
model =HistGradientBoostingClassifier(random_state=42)
model.fit(X_train, y_train)

# Outputting scores
scores = model.predict_proba(master_0.groupby("Task ID").mean()).T[1]
master_0["Scores"] = scores
all_to_classify.merge(master_0[["Task ID","Scores"]], on = "Task ID").to_csv("task_taxonomy/output/easy_v_hard.csv")


