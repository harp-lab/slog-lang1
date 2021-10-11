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

def parse_tuple_row(u64_list, intern_string_dict):
    """ parse a row of u64 tuple into a python list """
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
            attr_val = ['NESTED', val_tag, bucket_id, tuple_id]
        parsed_row.append(attr_val)
    return parsed_row


def binary_to_sexpr(bdatabase: dict[int, list[int]], tag_map: dict[int, tuple[str, int]],
                    intern_string_dict, max_nested_depth=10):
    """
    convert a bianry database  into a list of sexpression string
    bdatabase is a mapping of relation tag to list of u64 tuples.
    also a tag to (name,arity) mapping is required.
    a intern string mapping is required to render string.
    if nested relation reach max_nested_depth, it will printed as
    `({rel name} #{row count})`

    return similar one but instead of byte, rows are in sexpr format
    """
    parsed_tuples = {}
    def find_val_by_id(tag, bucket, t_id):
        for cnt, row in enumerate(parsed_tuples[tag]):
            if row[0] == (tag, bucket, t_id):
                return row, cnt
        return None, None
    def rel_to_str(rel, max_depth):
        """ turn a python list style row into sexpr string """
        # print(rel)
        res = []
        rel_index = rel[0]
        rel_name = tag_map[rel_index[0]][0]
        for col in rel[1:]:
            if isinstance(col, list):
                if col[0] == 'NESTED':
                    nested_tag = col[1]
                    nested_bucket = col[2]
                    nested_id = col[3]
                    val, row_cnt = find_val_by_id(nested_tag, nested_bucket, nested_id)
                    if val is None:
                        res.append(f'"{tag_map[nested_tag]} has no fact with id {nested_id} !"')
                    elif max_depth == 0:
                        nested_name = tag_map[nested_tag][0]
                        res.append(f"({nested_name} #{row_cnt})")
                    else:
                        res.append(rel_to_str(val, max_depth-1))
            else:
                res.append(str(col))
        return f"({rel_name} {' '.join(res)})"
    sexpr_database = {k: [] for k in bdatabase.keys()}
    # parse all u64 tuples to python list first
    for rel_tag, tuples in bdatabase.items():
        if tuples == []:
            continue
        # rel_name = tag_map[rel_tag][0]
        rel_arity = tag_map[rel_tag][1]
        buf = []
        rows = []
        for u64 in tuples:
            buf.append(u64)
            if len(buf) == rel_arity + 1:
                rows.append(parse_tuple_row(buf, intern_string_dict))
                buf = []
        parsed_tuples[rel_tag] = rows
    # turn all parsed database into sexpr style database
    for rel_tag, facts in parsed_tuples.items():
        for r in facts:
            sexpr_database[rel_tag].append(rel_to_str(r, max_nested_depth))
    return sexpr_database
