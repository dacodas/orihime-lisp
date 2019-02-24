import os

root_folder = "/var/lib/anki"

anki_folder = os.path.join(root_folder, "data")
lisp_folder = os.path.join(root_folder, "lisp-output")

user_name = "dacoda"

collection_file = os.path.join(anki_folder, user_name, "collection.anki2")

number_of_anki_fields = 10

cards_directory = os.path.join(lisp_folder, "cards")
model_directory = os.path.join(lisp_folder, "model")
