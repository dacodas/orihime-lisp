from anki import Collection
import json, os

import anki_config as configuration

col = Collection(configuration.collection_file)

model = col.models.byName('orihime')
col.models.setCurrent(model)

for card_name in os.listdir(configuration.cards_directory): 

    card_file = os.path.join(configuration.cards_directory, card_name)
    
    note = col.newNote()

    with open(card_file) as f:
        note.fields = json.loads(f.read())['fields']

    col.addNote(note)

col.close()
