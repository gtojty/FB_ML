import pandas as pd
import csv
import textmining
from nltk.tokenize import word_tokenize
from nltk.tokenize import RegexpTokenizer
from StringIO import StringIO
import sys
import re
from sklearn.feature_extraction.text import CountVectorizer
import textmining
from gensim import corpora, models
import gensim
from stop_words import get_stop_words
from nltk.stem.porter import PorterStemmer
from gensim import corpora, models

remove_words=['what','who','is','a','at','is','he','a','about','after','again','against','ago','ahead','all','almost','along',
'already','also','although','backward','backwards','be','because','before','behind','below','beneath','beside','between',
'both','but','by','had','hadnt','has','hasnt','have','havent','he','her','here','hers','herself','him','himself','his','how',
'however','I','can','cannot','cant','cause','cos','could','couldnt','despite','did','didnt','do','does','doesnt','dont',
'down','during','each','either','even','ever','every','except','for','forward','from','if','in','inside','inspite','instead',
'into','is','isnt','it','its','itself','just','least','less','like','many','may','me', 'might','mine','more','most','much',
'must','musnt','my','myself','near','need','neednt','needs','neither','never','no','none','nor','not','now','of','off','often',
'on','once','only','onto','or','ought','oughtnt','our','ours','ourselves','out','outside','over', 
'past','perhaps','quite','rather','seldom','several','shall','she','should', 'shouldnt','since','so','some','sometimes','soon',
'than','that','the','their','theirs','them','themselves','then','there','therefore','these','they','this','those','though', 
'through','thus','till','to','together','too','towards','under','unless','until','up','upon','us','used','usually','very',
'was','wasnt','we','well','were','werent','what','when','where','whether','which','while','who','whom',
'whose','why','will','with','without','wont','would','wouldnt','bobsnewline','i','http','u'] 

def main(args): 
	tdm = textmining.TermDocumentMatrix()
	df = pd.DataFrame(pd.read_csv(args[1]))
	#	grouped = pd.DataFrame()
	grouped=df.groupby('user_id')
	f1 = open(args[2],'wb')
	writer = csv.writer(f1)
	en_stop = get_stop_words('en')
	p_stemmer = PorterStemmer()
	user_docs = pd.DataFrame(index=range(len(grouped)),columns = ('user_id','doc'))
	dictionary_words = []
	count = 0
	for ix, grouped_df in grouped:
		status_grouped= grouped_df['status']
		grouped_cleaned_words=[]
		a = status_grouped.str.split('/t')
		for stat in a:
			status_cleaned_words = []
			stat_words = str(stat).split(" ") 
			for wor in stat_words:
				aa = re.sub(r'\W+', ' ', wor)
				status_cleaned_words.append(aa.lower())
			stopped_tokens = [i for i in status_cleaned_words if not i in en_stop]
#			stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]
			removed_tokens = [word for word in stopped_tokens if not word in remove_words]
			removed_numbers= [re.sub('[0-9]+','',word) for word in removed_tokens]
			grouped_cleaned_words = grouped_cleaned_words+ removed_numbers
		user_docs.loc[count] = [ix,' '.join(grouped_cleaned_words)]
		dictionary_words.append(grouped_cleaned_words)
		count +=1
	dictionary = corpora.Dictionary(dictionary_words)
	corpus = [dictionary.doc2bow(t) for t in dictionary_words]
	numTopics= 20
	ldamodel = gensim.models.ldamodel.LdaModel(corpus, id2word = dictionary, num_topics=numTopics,  passes=20)
	#ldamodel = gensim.models.ldamodel.LdaModel(corpus, id2word = dictionary, num_topics=2000,  passes=20)
	data= ldamodel.show_topics(num_topics=numTopics, num_words=20, formatted=False)
	#print ldamodel.show_topics(num_topics=2, num_words=3)
	#writer.writerow("[")		
	for row in data:
		row = row[1]
		tmprow = []
		#tmprow_prob = []
		for word in row:
			tmprow.append(word[0])
			#tmprow_prob.append(word[1])
		writer.writerow(tmprow)
		#print(tmprow_prob)
	f1.close()
	#writer.writerow("]")
	with open(args[3],'wb') as csvfile:
		spamwriter = csv.writer(csvfile,delimiter=',',quotechar ='"',quoting = csv.QUOTE_MINIMAL)
		for i in user_docs.index:
			words = user_docs.loc[i]['doc'].split()
			bow = dictionary.doc2bow(words)
			topics = ldamodel.get_document_topics(bow,0)
			topics_probs = []
			for j in topics:
				topics_probs.append(j[1])
			spamwriter.writerow([user_docs.loc[i]['user_id']]+topics_probs)

if __name__ == "__main__":
    main(sys.argv)



