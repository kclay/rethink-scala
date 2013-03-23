__author__ = 'Keyston'

import rethinkdb as r

def print_table():
    ast =r.table("tbl",True).insert([
        dict(id=0),
        dict(id=1)
    ])
    print ast

if __name__ == "__main__":
    print_table()