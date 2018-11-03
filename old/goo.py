import sys
sys.path.append("/usr/share/anki")

import anki.notes as notes
from anki.storage import Collection

# Lisp sends old_md5sum, new_md5sum

collection = Collection("/home/dacoda/.local/share/Anki2/User 1/collection.anki2")
# id = grab_note_id(old_md5sum)
# note = Note(collection.col, id=id)

# # It looks like fields[0] is a checksum of all the fields
# old_fields = note.fields 
# note.fields[1:] = []

# words = get_sentence_words_from_lisp(new_md5sum)
# note.fields.append(words)
# note.flush()
