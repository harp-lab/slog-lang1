'''
Data format convert untility function

Yihao Sun
'''


TAG_MASK = 0xFFFFC00000000000
BUCKET_MASK = 0x00003FFFF0000000
TUPLE_ID_MASK = 0xFFFFFFFFF0000000
VAL_MASK = ~ TAG_MASK
INT_TAG = 0
STRING_TAG = 2
SYMBOL_TAG = 3


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


def parse_tuple_row(u64_list, rel_name, intern_string_dict) -> SlogTuple:
    """ parse a row of u64 tuple into a python object """
    parsed_row = []
    # index col
    rel_tag = u64_list[0] >> 46
    bucket_id = (u64_list[0] & BUCKET_MASK) >> 28
    tuple_id = u64_list[0] & (~TUPLE_ID_MASK)
    parsed_row.append((rel_tag, bucket_id, tuple_id))
    for u64 in u64_list[1:]:
        val_tag = u64 >> 46
        if val_tag == INT_TAG:
            attr_val = u64 & VAL_MASK
        elif val_tag == STRING_TAG:
            attr_val = intern_string_dict[u64 & VAL_MASK]
        else:
            # relation
            bucket_id = (u64 & BUCKET_MASK) >> 28
            tuple_id = u64 & (~TUPLE_ID_MASK)
            attr_val = ('NESTED', val_tag, bucket_id, tuple_id)
        parsed_row.append(attr_val)
    return SlogTuple(rel_name, parsed_row)

# def parse_all_tuple()

def tuple_to_str(slog_tuple: SlogTuple, max_depth, cardinality, tag_map, printed_id_map,
                 count_map):
    """ turn a python tuple  into sexpr string """
    # print(rel)
    if count_map[slog_tuple.tuple_id] >= cardinality:
        printed_id = None
        for p_id, _t in printed_id_map.items():
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
                printed_id = None
                for p_id, _t in printed_id_map.items():
                    if _t.tuple_id == col[1:]:
                        printed_id, val = p_id, _t
                if val is None:
                    nested_id = col[3]
                    res.append(f'"{tag_map[nested_tag]} has no fact with id {nested_id} !"')
                elif max_depth == 0:
                    res.append(f"({tag_map[nested_tag][0]} #{printed_id})")
                else:
                    res.append(tuple_to_str(val, max_depth-1, cardinality, tag_map,
                                            printed_id_map, count_map))
        else:
            res.append(str(col))
    return f"({rel_name} {' '.join(res)})"

def count_tuples(slog_tuple: SlogTuple, max_depth, tag_map, printed_id_map, count_map, top_level=True):
    """ count the appearance of tuples nested in a tuple """
    if slog_tuple.tuple_id in count_map:
        count_map[slog_tuple.tuple_id] += 1
    elif top_level:
        count_map[slog_tuple.tuple_id] = 0
    else:
        count_map[slog_tuple.tuple_id] = 1
    for col in slog_tuple.data_col:
        if isinstance(col, tuple):
            if col[0] == 'NESTED':
                val = None
                for _t in printed_id_map.values():
                    if _t.tuple_id == col[1:]:
                        val = _t
                if val is not None and  max_depth != 0:
                    count_tuples(val, max_depth-1, tag_map,printed_id_map, count_map, False)

def pretty_str_tuples(slog_tuples, max_depth, cardinality, tag_map, printed_id_map):
    """ pretty stringfy a list of tuples """
    count_map = {}
    res = []
    for slog_tuple in slog_tuples:
        count_tuples(slog_tuple,max_depth, tag_map, printed_id_map, count_map)
    unfolded_ids = []
    for t_id, count in count_map.items():
        if count == 0 or count >= cardinality:
            unfolded_ids.append(t_id)
    for p_id, val in printed_id_map.items():
        if val.tuple_id in unfolded_ids:
            pp_str = tuple_to_str(val, max_depth, cardinality, tag_map,
                                  printed_id_map, count_map)
            res.append(f"#{p_id}\t{pp_str}")
    return res

def find_printed_id_by_val(printed_map, val):
    for p_id, p_val in printed_map.items():
        if p_val == val:
            return p_id

def parse_query_result(query_res, tag_map, intern_string_dict,
                       printed_id_map: dict=None):
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
    parsed_tuples = []

    # parse all u64 tuples to python list first
    for rel_tag, tuples in query_res.items():
        if tuples == []:
            continue
        rel_name = tag_map[rel_tag][0]
        rel_arity = tag_map[rel_tag][1]
        buf = []
        for u64 in tuples:
            buf.append(u64)
            if len(buf) == rel_arity + 1:
                slog_tuple = parse_tuple_row(buf, rel_name, intern_string_dict)
                parsed_tuples.append(slog_tuple)
                buf = []
    if printed_id_map == {}:
        cur_printed_id = 0
    else:
        cur_printed_id = max(printed_id_map.keys())
    #  update tuple tag map
    for new_tuple in parsed_tuples:
        p_id = find_printed_id_by_val(printed_id_map, new_tuple)
        if not p_id:
            cur_printed_id += 1
            printed_id_map[cur_printed_id] = new_tuple
    return parsed_tuples
