
# coding: utf-8

# In[47]:

from alignment.sequence import Sequence
from alignment.vocabulary import Vocabulary
from alignment.sequencealigner import SimpleScoring, GlobalSequenceAligner

def recommendation(name, movies):
    """Find the top ten neartest match in a list of movie names
    
    Args:
        name: a string of key words seperated by white space
        dic: a list of movie names to choose from
    
    Returns:
        A list of movie names
    """
    # Create sequences to be aligned.
    key = Sequence(name.split())
    dic = [Sequence(movie.split()) for movie in movies]
    # Create a vocabulary and encode the sequences.
    v = Vocabulary()
    keyEncoded = v.encodeSequence(key)
    dicEncoded = [v.encodeSequence(movie) for movie in dic]
    # Create a scoring and align the sequences using global aligner.
    scoring = SimpleScoring(1, 0)
    aligner = GlobalSequenceAligner(scoring, -2)
    score = [aligner.align(keyEncoded, dEncoded, backtrace=False) for dEncoded in dicEncoded]
    # Get the top five score in all movies 
    topFive = sorted(range(len(score)), key=lambda i:score[i])
    return [ movies[i] for i in topFive ]


# In[48]:

# recommendation("help me out",["help","help out","help me out","help him out"])

