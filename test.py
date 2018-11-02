import pymongo
import pprint
import html2text

from graphviz import Digraph

client = pymongo.MongoClient()
database = client.goo
collection = database.sentences
words_collection = database.words

count = 1

for sentence in collection.find():
    dot = Digraph()

    contents = sentence["sentence-contents"]
    test_sentence = sentence

    dot.node("sentence", '{}...'.format(test_sentence["sentence-contents"][:10]))

    words = set()

    def make_edges(parent, child):

        dot.edge(parent, child)

        if child in words:
            return

        dot.node(child)
        words.add(child)

        print("Looking for word {}".format(child))
        word = words_collection.find_one({"_id": child})

        if not word:
            print("Couldn't find it!")
            return

        child_words = word["child-words"]

        print(child_words)

        if child_words is None:
            return

        for child_word in child_words:
            make_edges(child, child_word)

    if test_sentence["child-words"] is None:
        continue

    for child_word in test_sentence["child-words"]:
        make_edges("sentence", child_word)

    print(dot.source)
    dot.render('/tmp/test-output-{}.gv'.format(count))
    count += 1
