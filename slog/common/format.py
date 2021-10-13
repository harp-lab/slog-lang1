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
            attr_val = ('NESTED', val_tag, bucket_id, tuple_id)
        parsed_row.append(attr_val)
    return parsed_row


def binary_table_to_sexpr(name, bdatabase, tag_map, intern_string_dict,
                          max_nested_depth=10, cardinality=5):
    """
    convert a bianry database  into a list of sexpression string
    name is relation table name
    bdatabase is a mapping of relation tag to list of u64 tuples.
    also a tag to (name,arity) mapping is required.
    a intern string mapping is required to render string.
    if nested relation reach max_nested_depth, it will printed as
    `({rel name} #{row count})`. If a fact appears more than
    `cardinality` times, it won't be nested

    return a list of sexpr facts (if some other relation nested,
    they will also been there)
    """
    parsed_tuples = {}
    def rel_to_str(rel, max_depth, printed_id_map):
        """ turn a python list style row into sexpr string """
        # print(rel)
        res = []
        rel_index = rel[0]
        rel_name = tag_map[rel_index[0]][0]
        for col in rel[1:]:
            if isinstance(col, tuple):
                if col[0] == 'NESTED':
                    nested_tag = col[1]
                    val = parsed_tuples[col[1:]]
                    if val is None:
                        nested_id = col[3]
                        res.append(f'"{tag_map[nested_tag]} has no fact with id {nested_id} !"')
                    elif max_depth == 0:
                        nested_name = tag_map[nested_tag][0]
                        printed_id = printed_id_map[col[1:]]
                        res.append(f"({nested_name} #{printed_id})")
                    else:
                        res.append(rel_to_str(val, max_depth-1, printed_id_map))
            else:
                res.append(str(col))
        return f"({rel_name} {' '.join(res)})"
    nested_count_map = {}    # a map of nested fact to how many time it appears
    unfolded_tuples = []
    # parse all u64 tuples to python list first
    for rel_tag, tuples in bdatabase.items():
        if tuples == []:
            continue
        rel_name = tag_map[rel_tag][0]
        rel_arity = tag_map[rel_tag][1]
        buf = []
        for u64 in tuples:
            buf.append(u64)
            if len(buf) == rel_arity + 1:
                parsed_row = parse_tuple_row(buf, intern_string_dict)
                row_id = parsed_row[0]
                parsed_tuples[row_id] = parsed_row
                if rel_name == name:
                    if row_id not in nested_count_map:
                        unfolded_tuples.append(row_id)
                    elif nested_count_map[row_id] > cardinality:
                        unfolded_tuples.append(row_id)
                for col in parsed_row[1:]:
                    if isinstance(col, tuple) and col[0] == 'NESTED':
                        nested_t_id = col[1:]
                        if nested_t_id not in nested_count_map:
                            nested_count_map[nested_t_id] = 1
                            if nested_t_id in unfolded_tuples:
                                unfolded_tuples.remove(nested_t_id)
                        elif nested_count_map[nested_t_id] <= cardinality:
                            nested_count_map[nested_t_id] += 1
                            if nested_t_id in unfolded_tuples:
                                unfolded_tuples.remove(nested_t_id)
                        else:
                            unfolded_tuples.append(nested_t_id)
                buf = []
    printed_id_map = {x: idx for x, idx in enumerate(unfolded_tuples)}
    res = []
    for f_id in unfolded_tuples:
        res.append(rel_to_str(parsed_tuples[f_id], max_nested_depth, printed_id_map))
    return res
