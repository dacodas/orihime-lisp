import os
import copy

import anki_config as configuration

field_template = {
    "font": "Arial",
    "media": [],
    "rtl": False,
    "size": 20,
    "sticky": False
    # "name": "Text",
    # "ord": 0,
}

card_template = {
    "qfmt": "",
    "afmt": "",
    "bafmt": "",
    "bqfmt": "",
    "did": None,
    # "name": "Card 1",
    # "ord": 0
} 

from anki import Collection

import os

col = Collection(configuration.collection_file)

text_field = copy.deepcopy(field_template)
text_field['name'] = "Text"
text_field['ord'] = 0

fields = [text_field]
cards = []

for card_number in range(configuration.number_of_anki_fields):

    current_field = copy.deepcopy(field_template)
    current_field['name'] = str(card_number)
    current_field['ord'] = card_number + 1;

    fields.append(current_field)

    current_card = copy.deepcopy(card_template)
    current_card['name'] = "Child word {}".format(card_number)
    current_card['ord'] = card_number

    front_file = os.path.join(configuration.model_directory, str(card_number), "front.html")
    with open(front_file) as f:
        front_contents = f.read()

    back_file = os.path.join(configuration.model_directory, str(card_number), "back.html")
    with open(back_file) as f:
        back_contents = f.read()

    current_card['qfmt'] = front_contents
    current_card['afmt'] = back_contents

    cards.append(current_card)

model = col.models.new('orihime')
model['tmpls'] = cards
model['flds'] = fields
model['css'] = ""
col.models.add(model)

col.close()

