'''
Data format convert untility function

Yihao Sun
'''

import multiprocessing as mp
import os
from functools import partial

TAG_MASK =      0xFFFFC00000000000
BUCKET_MASK =   0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
U32_MASK =      0x00000000FFFFFFFF
SIGN_FILP_CONST = 0x0000200000000000
SIGNED_NUM_MASK = 0xFFFFE00000000000
VAL_MASK = ~ TAG_MASK
INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3
randid = 1

class SlogStr:
    def __init__(self, sid) -> None:
        self.sid = sid

class SlogTuple:
    """
    a python data class to represent a slog tuple
    a nest relation coloumn will be `(NESTED ...)`
    """
    def __init__(self, rel_name, col):
        self.rel_name = rel_name
        self.print_name = None
        self.col = col
        self.tuple_id = col[0]
        self.data_col = col[1:]
        self.tag = col[0][0]
        self.arity = len(col) - 1
        

    def __eq__(self, o: object) -> bool:
        return self.tuple_id == o.tuple_id

    def __str__(self) -> str:
        return str(self.col)

class SlogTupleParser:
    """
    A parser will parse a slog query result u64 tuple set into well-formated string.
    """

    def __init__(self, query_res, cardinality, max_depth, tag_map, intern_string_dict, rel_name, rel_tag, limit=100, local_db_path=""):
        self.rel_name = rel_name
        self.rel_tag = rel_tag
        self.printed_id_map = {}
        self.reversed_id_map = {}
        self.count_map = {}
        self.query_res = query_res
        # print(f"\n\nQuery Res: {query_res}\n\n")
        self.cardinality = cardinality
        self.max_depth = max_depth
        self.tag_map = tag_map
        self.intern_string_dict = intern_string_dict
        self.limit = limit
        self.local_db_path = local_db_path
        # self.randid = 1 #adding this to fix the key error issue

    def parse_tuple_row(self, u64_list, rel_name) -> SlogTuple:
        """ parse a row of u64 tuple into a python object """
        parsed_row = []
        # index col
        rel_tag = u64_list[0] >> 46
        bucket_id = (u64_list[0] & BUCKET_MASK) >> 28
        tuple_id = u64_list[0] & (~TUPLE_ID_MASK)
        t_id = (rel_tag, bucket_id, tuple_id)
        parsed_row.append(t_id)
        require_rels = []
        if t_id in self.count_map:
            self.count_map[t_id] += 1
        else:
            self.count_map[t_id] = 0
        for u64 in u64_list[1:]:
            val_tag = u64 >> 46
            if val_tag == INT_TAG:
                attr_val = (u64 & VAL_MASK)
                if attr_val >= SIGN_FILP_CONST:
                    attr_val = SIGN_FILP_CONST - attr_val
            elif val_tag == STRING_TAG:
                # attr_val = intern_string_dict[u64 & U32_MASK]
                attr_val = SlogStr(u64 & VAL_MASK)
            else:
                # relation
                bucket_id = (u64 & BUCKET_MASK) >> 28
                tuple_id = u64 & (~TUPLE_ID_MASK)
                attr_val = ('NESTED', val_tag, bucket_id, tuple_id)
                if val_tag not in require_rels:
                    require_rels.append(val_tag)
            parsed_row.append(attr_val)
        return SlogTuple(rel_name, parsed_row), require_rels


    def tuple_to_str(self, slog_tuple: SlogTuple, cur_max_depth):
        """ turn a python tuple  into sexpr string """
        # print(rel)
        if self.count_map[slog_tuple.tuple_id] >= self.cardinality:
            printed_id = None
            for p_id, _t in self.printed_id_map.items():
                if _t == slog_tuple:
                    printed_id = p_id
            return f"#{printed_id}"
        res = []
        rel_name = slog_tuple.rel_name
        for col in slog_tuple.data_col:
            if isinstance(col, tuple):
                if col[0] == 'NESTED':
                    nested_tag = col[1]
                    val = None
                    try:
                        printed_id, val = self.reversed_id_map[col[1:]] # This ERRORS out
                    except:
                        print(f"\n\n\n query res: {self.query_res} \ncardinality {self.cardinality}, \nmax_depth {self.max_depth},\nintern_string_dict: {self.intern_string_dict},\nrel_name: {self.rel_name}, \nrel_tag: {self.rel_tag} \nnested_tag {nested_tag} \ncol: {slog_tuple.data_col}")
                    if val is None:
                        nested_id = col[3]
                        res.append(f'"{self.tag_map[nested_tag]} has no fact with id {nested_id} !"')
                    elif cur_max_depth == 0:
                        res.append(f"({self.tag_map[nested_tag][0]} #{val.rel_name}_{val.tuple_id[2]})")
                    else:
                        res.append(self.tuple_to_str(val, cur_max_depth-1))
            elif isinstance(col, SlogStr):
                res.append(self.intern_string_dict[col.sid])
                # try:
                #     res.append(self.intern_string_dict[col.sid])
                # except:
                #     res.append(str(col.sid))
            else:
                res.append(str(col))
        return f"({rel_name} {' '.join(res)})"

    # def count_tuples(self, slog_tuple: SlogTuple, cur_max_depth, top_level=True):
    #     """ count the appearance of tuples nested in a tuple """
    #     if slog_tuple.tuple_id in self.count_map:
    #         self.count_map[slog_tuple.tuple_id] += 1
    #     elif top_level:
    #         self.count_map[slog_tuple.tuple_id] = 0
    #     else:
    #         self.count_map[slog_tuple.tuple_id] = 1
    #     for col in slog_tuple.data_col:
    #         if isinstance(col, tuple):
    #             if col[0] == 'NESTED':
    #                 val = None
    #                 for _t in self.printed_id_map.values():
    #                     if _t.tuple_id == col[1:]:
    #                         val = _t
    #                 if val is not None and  cur_max_depth != 0:
    #                     self.count_tuples(val, cur_max_depth-1, False)

    def pretty_str_tuples(self, print_rel_list):
        """ pretty stringfy a list of relation """
        rel_tag_list = [r[2] for r in print_rel_list]
        # for slog_tuple in slog_tuples:
        #     self.count_tuples(slog_tuple, self.max_depth, self.printed_id_map)
        unfolded_ids = []
        for t_id, count in self.count_map.items():
            if count == 0 or count >= self.cardinality:
                unfolded_ids.append(t_id)
        
        # to write the facts in facts.txt in sorted order
        sorted_items = sorted(self.printed_id_map.items(), key=lambda item: int(item[1].tuple_id[2]))
            
        # for p_id, val in self.printed_id_map.items():
        for p_id, val in sorted_items:
            # global randid
            # randid += 1
            if val.tuple_id[0] in rel_tag_list and val.tuple_id in unfolded_ids:
                pp_str = self.tuple_to_str(val, self.max_depth)
                # res.append(f"#{p_id}\t{pp_str}")
                # yield f"#{self.reversed_id_map[val.data_col[0][1:][0]]}\t{pp_str}"
                yield f"#{val.rel_name}_{val.tuple_id[2]}\t{pp_str}" # WORKING LINE
                # yield f"#{p_id}\t{pp_str}"
                # yield f"#{randid}_{p_id}\t{pp_str}"
                # yield f"#{val.rel_name}\t{pp_str}"
        # return res

    def pretty_str_printed_id(self, printed_id):
        """ pretty print an slog tuple with given printed_id """
        pp_str = self.tuple_to_str(self.printed_id_map[printed_id], self.max_depth)
        return f"#{printed_id}\t{pp_str}"

    def parse_query_result(self, limit=100):
        """
        parse a query result (u64) into python slog tuples object
        query_res is a mapping of relation tag to list of u64 tuples.
        also a tag to (name,arity) mapping is required.
        a intern string mapping is required to render string.
        if nested relation reach max_nested_depth, it will printed as
        `({rel name} #{row count})`. If a fact appears more than
        `cardinality` times, it won't be nested

        return a list of sexpr facts (if some other relation nested,
        they will also been there) and the updated print id map

        NOTE:
        in printed map only index > 0 index will be printed
        This function will update printed_id_map
        """
        self.limit = limit
        parsed_tuples = []
        rel_to_parse = [self.rel_tag]
        rel_parsed = []
        # parse all u64 tuples to python list first
        # for rel_tag, tuples in self.query_res.items():
        #     if tuples == []:
        #         continue
        #     rel_name = self.tag_map[rel_tag][0]
        #     rel_arity = self.tag_map[rel_tag][1]
        #     for i in range(0, len(tuples), rel_arity+1):
        #         buf = tuples[i:i+rel_arity+1]
        #         if len(buf) == rel_arity + 1:
        #             slog_tuple = self.parse_tuple_row(buf, rel_name)
        #             parsed_tuples.append(slog_tuple)
        while len(rel_to_parse) != 0:
            # if rel_to_parse == self.rel_tag:
            next_rel_tag = rel_to_parse[0]
            tuples = []
            if next_rel_tag not in self.query_res:
                # local mode try to fectch from disk
                self.query_res[next_rel_tag] = []
                target_file = None
                for table_file in os.listdir(self.local_db_path):
                    if not (table_file.endswith('.table') or table_file.endswith('.table_full')):
                        continue 
                    # 467.appk.4.table
                    # if table_file == "467.appk.4.table":
                        
                    if int(table_file.split('.')[0]) == next_rel_tag:
                        target_file = table_file # 467.appk.4.table
                        break
                arity = int(target_file.split('.')[-2])
                # 4
                with open(os.path.join(self.local_db_path, target_file), 'rb') as bin_file:
                    bin_bytes = bin_file.read()
                    u64_buf = []
                    for i in range(0, len(bin_bytes), 8):
                        raw_u64 = int.from_bytes(bin_bytes[i:i+8], 'little', signed=False)
                        u64_buf.append(raw_u64)
                        if len(u64_buf) == arity + 1:
                            self.query_res[next_rel_tag].append(u64_buf[-1])
                            for u64 in u64_buf[:-1]:
                                self.query_res[next_rel_tag].append(u64)
                            u64_buf = []
            
            tuples = self.query_res[next_rel_tag]
            rel_name = self.tag_map[next_rel_tag][0]
            rel_arity = self.tag_map[next_rel_tag][1]
            for i in range(0, len(tuples), rel_arity+1):
                if (next_rel_tag == self.rel_tag) and (i > self.limit):
                    break
                buf = tuples[i:i+rel_arity+1]
                if len(buf) == rel_arity + 1:
                    slog_tuple, required_rels = self.parse_tuple_row(buf, rel_name)
                    rel_to_parse += [r for r in required_rels
                                     if (r not in rel_to_parse) and (r not in rel_parsed)]
                    parsed_tuples.append(slog_tuple)
            # if rel_to_parse != []:
            #     print(f"{next_rel_tag} >>>>>>>> {rel_to_parse}")
            rel_parsed.append(next_rel_tag)
            rel_to_parse = rel_to_parse[1:]

        if self.printed_id_map == {}:
            cur_printed_id = 0
        else:
            cur_printed_id = max(self.printed_id_map.keys())
        #  update tuple tag map
        # generate a reversed map
        # reversed_id_map = dict([reversed(i.tuple_id) for i in self.printed_id_map.items()])
        for new_tuple in parsed_tuples:
            # p_id = find_printed_id_by_val(printed_id_map, new_tuple)
            if new_tuple.tuple_id not in self.reversed_id_map:
                cur_printed_id += 1
                self.printed_id_map[cur_printed_id] = new_tuple
                self.reversed_id_map[new_tuple.tuple_id] = (cur_printed_id, new_tuple)
        # print(self.printed_id_map)
        return parsed_tuples
