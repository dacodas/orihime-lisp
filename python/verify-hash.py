import bcrypt
import sys

(user_id, password,
 salt, purported_hash) = [arg.encode('utf8') for arg in sys.argv[1:]]

hash = bcrypt.hashpw(user_id, salt)

# print(hash)
# print(purported_hash)
# print(hash == purported_hash)

sys.exit(0 if hash == purported_hash else 1)

