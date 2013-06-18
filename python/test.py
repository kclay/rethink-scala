from rethinkdb.ast import DB

__author__ = 'Keyston'

import rethinkdb as r
from rethinkdb import ql2_pb2 as p, expr


def print_table():
    ast = r.do(r.table('marvel').get('IronMan'),'2',
               lambda ironman: ironman['name'])

    v = None
    #ast=r.table("marvel").map(lambda hero: hero['combatPower'] + hero['compassionPower'] * 2)
    term = p.Term()
    ast.build(term)
    print term
   # c = r.connect(host="172.16.2.45",db="foo")
    #ast.run(c)
    #print to_query(ast,db="test")

def to_query(term, **global_opt_args):
    token = 1


    # Construct query
    query = p.Query()
    query.type = p.Query.START
    query.token = token

    # Set global opt args

    # The 'db' option will default to this connection's default
    # if not otherwise specified.
    if 'db' in global_opt_args:
        global_opt_args['db'] = DB(global_opt_args['db'])
    #else:
     #   if db:
     #       global_opt_args['db'] = self.db

    for k,v in global_opt_args.iteritems():
        pair = query.global_optargs.add()
        pair.key = k
        expr(v).build(pair.val)

    # Compile query to protobuf
    term.build(query.query)
    return query
if __name__ == "__main__":
    print_table()