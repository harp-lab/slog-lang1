'''
Command Specification

Yihao Sun
'''

HELP = '''
    Command:
    NOTE: `(...)` means optionanl argument, `/` means alternative argument, `<...>` is meta name

    help                                Print help

    showdb                              Show all committed databases

    compile "<file>"                    Load and compile a slog source file.

    run "<file>" (<db>) (<core>)        Load a slog source file into background, will create a database
                                        with file name, and then compile and run it, if db is not provide
                                        will run with current db, core is how many core mpirun use.

    dump <hash>/"<tag>" ("<file>")      Dump all data in a relation into stdout, if optional file argument
                                        is provided, result will be printed to file 

    connect "<server>"                  Connect to a slog server

    load "<csv_file/folder>"            Upload a csv file/folder into input database, file must ends with
                                        `.fact`, name of target relation will be same as file name

    tag <hash> "<tag>"                  Give a database hash a taged name

    switch <db>                         Switch to a given DB
'''

CMD = ['help', 'run', 'connect', 'dump', 'showdb',
       'load', 'commit', 'compile', 'tag', 'switch']


def exec_command(repl, raw_input: str):
    """
    A naive valiadator and processer for command
    """
    raw_input = raw_input.strip()
    if raw_input == '':
        # bypass empty command
        return
    # check if it a query
    if raw_input.startswith('?(') and raw_input.endswith(')'):
        return
    # normal command
    cmd = raw_input.split(' ')[0].strip()
    args = [r.strip() for r in raw_input.split(' ')[1:] if r.strip() != '']
    if cmd == 'help':
        print(HELP)
    elif cmd == 'showdb':
        repl.showdbs()
    elif cmd == 'connect':
        if len(args) == 1:
            repl.connect(args[0])
        else:
            repl.invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'dump':
        if len(args) == 1:
            repl.dump_relations(args[0])
        elif len(args) == 2:
            if args[1].startswith('"') and args[1].endswith('"'):
                repl.dump_relations(args[0], args[1][1:-1])
            else:
                repl.invalid_alert(f'{cmd} expect a string at postion 2 as arg')
        else:
            repl.invalid_alert(f'{cmd} expect 1/2 arg, but get {len(args)}')
    elif cmd == 'load':
        if len(args) == 1:
            if args[0].startswith('"') and args[0].endswith('"'):
                repl.upload_csv(args[0][1:-1])
            else:
                repl.invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            repl.invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'compile':
        if len(args) == 1:
            if args[0].startswith('"') and args[0].endswith('"'):
                repl.load_slog_file(args[0][1:-1])
            else:
                repl.invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            repl.invalid_alert(f'{cmd} expect 1 arg, but get {len(args)}')
    elif cmd == 'run':
        if len(args) > 1 and (not args[0].startswith('"') or not args[0].endswith('"')):
            repl.invalid_alert(f'{cmd} expect a string at postion 1 as arg')
            return
        if len(args) == 2 and len(args[1]) < 5 and args[1].isnumeric():
            repl.run_with_db(args[0][1:-1], cores=int(args[1]))
        elif len(args) == 2:
            repl.run_with_db(args[0][1:-1], args[1])
        elif len(args) == 3 and args[2].isnumeric():
            repl.run_with_db(args[0][1:-1], args[1], int(args[2]))
        else:
            repl.invalid_alert(f'{cmd} has wrong args, please see help')
    elif cmd == 'tag':
        if len(args) == 2:
            if args[1].startswith('"') and args[1].endswith('"'):
                repl.tag_db(args[0], args[1][1:-1])
            else:
                repl.invalid_alert(f'{cmd} expect a string at postion 1 as arg')
        else:
            repl.invalid_alert(f'{cmd} expect 2 arg, but get {len(args)}')  
    elif cmd == 'switch':
        if len(args) == 1:
            repl.switch_db(args[0])
        else:
            repl.invalid_alert(f'{cmd} expected 1 argument, got {len(args)}.')
    else:
        repl.invalid_alert(f'{cmd} is not a valid command, type `help to see help`')
