__author__ = 'Keyston'

import rethinkdb as r
from rethinkdb import ql2_pb2 as p

def print_table():
    ast = r.expr("hello")
    ast+=" world"
   # c = r.connect(host="172.16.2.45",db="foo")
    #ast.run(c)
    term = p.Term()
    ast.build(term)
    print term

if __name__ == "__main__":
    print_table()