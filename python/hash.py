import bcrypt
import sys

user_id = sys.argv[1]
password = sys.argv[2]
salt = bcrypt.gensalt()
hash = bcrypt.hashpw(user_id.encode('utf8'), salt)

print(salt.decode('ascii'))
print(hash.decode('ascii'), end="")
