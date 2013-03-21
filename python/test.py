__author__ = 'Keyston'

import rethinkdb as r

def print_table():
    print r.table("foo")

if __name__ == "__main__":
    print_table()