// Originally by Sidharth Kumar
// Subsequently by Kris Micinski
// Convert Souffle CSV (tab-separated value) files to Slog input tuple files
// compile with >= c++14
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <algorithm>
#include <cstdio>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <filesystem>

#include "../src/compat.h"

#define MIN_REL_TAG 256
#define MAX_REL_TAG 0x3FFFF

#define INTEGER_TAG 0
#define FLOAT_TAG 1
#define STRING_TAG 2
#define BOOL_TAG 4

#define TUPLE_MASK_LENGTH 28
#define TUPLE_MASK 0x000000000FFFFFFF
#define BUCKET_MASK 0x00003FFFF0000000
#define BUCKET_MASK_LENGTH 18
#define TAG_MASK 0xFFFFC00000000000

using namespace std;

// globals
unordered_map<string, long> strings_map;
long max_string_id = 0;
long current_tuple_id = 0;
unsigned arity;
unsigned index_length = 0;
unordered_set<u64> tuple_ids;
unsigned buckets;
string string_intern_file_path;
string mode = "slog";

// hash a tuple n values long using our hashing algorithm
u64 hash_tuple(u64 *fact, unsigned num)
{
	u64 prime = 1099511628211ull;
	u64 hash = 14695981039346656037ull;
	u64 chunk, h0;
	for (unsigned i = 0; i < num; i++)
	{
		chunk = fact[i];
		h0 = hash ^ (chunk & 255);
		hash = h0 * prime;
		for (unsigned j = 0; j < 7; j++)
		{
			chunk = chunk >> 8;
			h0 = hash ^ (chunk & 255);
			hash = h0 * prime;
		}
	}
	return hash;
}

// parse a column ordering (C string) into a vector
void parse_column_order(char *index, vector<unsigned> &result)
{
	stringstream s_stream;
	s_stream << index;
	unsigned cur_col = 1;
	unsigned tuple_cols = arity + 1;
	while (s_stream.good())
	{
		string substr;
		int curnum;
		getline(s_stream, substr, ',');
		try
		{
			curnum = stoi(substr);
		}
		catch (...)
		{
			cerr << "Did not understand how to parse index (couldn't read " << substr << ")\n";
			exit(1);
		}
		if (curnum <= 0)
		{
			cerr << "All column numbers must be > 0 "
				 << "(" << curnum << " is not allowed)\n";
			cerr << "Note that canonical indices may not include 0.\n";
			exit(1);
		}
		if (curnum > arity + 1)
		{
			cerr << "Index contains column number " << curnum
				 << " which is larger than arity " << arity << endl;
			exit(1);
		}
		if (!(find(result.begin(), result.end(), curnum) != result.end()))
		{
			result.push_back((unsigned)curnum);
		}
		else
		{
			cerr << "Invalid repeated use of column " << curnum << " in index.\n";
			exit(1);
		}
		cur_col++;
	}
	// index coloumn (0) is the last one
	result.push_back(0);

	if (cur_col > tuple_cols)
	{
		cerr << "Found " << cur_col << "columns in the index " << index
			 << " but arity is only " << arity << endl;
		exit(1);
	}
}

void read_strings(string filename)
{
	ifstream fp_in(filename, ios_base::in);
	string string_id;
	string string_value;
	unsigned lineno = 1;
	if (fp_in.fail())
	{
		cout << "warning: could not open $strings.csv file. Will create it.\n";
	}
	while (fp_in.good())
	{
		string row;
		getline(fp_in, row);
		if (row == "")
			continue;
		istringstream row_stream(row);
		try
		{
			getline(row_stream, string_id, '\t');
			getline(row_stream, string_value, '\t');
			long id = stoi(string_id);
			if (id < 0)
			{
				cerr << "error: ID < 0 is not allowed" << endl;
				exit(1);
			}
			strings_map[string_value] = id;
			max_string_id = max(max_string_id, id);
		}
		catch (const exception &exc)
		{
			cerr << "error reading strings (line " << lineno << ")\n";
			cerr << exc.what() << "  " << string_id << " " << string_value << endl;
			exit(1);
		}
		lineno++;
	}
}

// fill in $strings.csv, ...
void write_interned_pools()
{
	// const string strings_file = "./$strings.csv";
	int strings = open(string_intern_file_path.c_str(), O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	for (const auto &[key, value] : strings_map)
	{
		string s = to_string(value) + "\t" + key + "\n";
		write(strings, s.c_str(), s.length());
	}
}

#define BUF_SIZE 40960

void file_to_slog(char *input_file, char *output_file,
				  vector<unsigned> &column_order, unsigned rel_tag)
{
	u64 tuple_buffer[column_order.size()];

	ifstream fp_in(input_file);
	int fp_out = open(output_file, O_CREAT | O_RDWR | O_APPEND, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	size_t lineno = 0;
	// string prev_left;
	stringstream buf_stream;
	buf_stream << fp_in.rdbuf();
	while (buf_stream)
	{
		string row;
		getline(buf_stream, row, '\n');
		if (row == "")
		{
			// empty line at the end of file
			continue;
		}
		// if (prev_left != "")
		// {
		// 	row = prev_left + row;
		// 	prev_left = "";
		// }
		istringstream row_stream(row);
		string col;
		int col_count = 0;
		while (row_stream)
		{
			getline(row_stream, col, '\t');
			if (col_count >= column_order.size() - 1)
			{
				break;
			}
			try
			{
				// TODO: support float later
				// FIXME: detect empty space here!
				u64 u64_v = stoi(col);
				tuple_buffer[col_count] = TUPLE_MASK & u64_v;
				// cout << "number at " << col_count << " : " << u64_v << endl;
			}
			catch (...)
			{
				// if not number all goes to string
				auto itr = strings_map.find(col);
				u64 u64_v = STRING_TAG;
				u64_v <<= TUPLE_MASK_LENGTH + BUCKET_MASK_LENGTH;
				if (itr == strings_map.end())
				{
					long new_id = max_string_id + 1;
					strings_map[col] = new_id;
					max_string_id = new_id;
					u64_v |= new_id;
				}
				else
				{
					u64_v |= strings_map[col];
				}
				// cout << "string at " << col_count << " : " << col << endl;
				tuple_buffer[col_count] = u64_v;
			}
			col_count++;
		}
		// if (col_count < column_order.size())
		// {
		// 	// incomplete line
		// 	prev_left = row;
		// 	col_count = 0;
		// 	continue;
		// }

		u64 tid = rel_tag;
		tid <<= (TUPLE_MASK_LENGTH + BUCKET_MASK_LENGTH);
		tid |= ((hash_tuple(tuple_buffer, arity) % buckets)) << TUPLE_MASK_LENGTH;
		tid |= current_tuple_id++;
		// cout << "tuple in bucket " << ((hash_tuple(tuple_buffer, arity) % buckets)) << endl;
		// cout << "id at " << col_count << " : " << tid << endl;
		tuple_buffer[col_count] = tid;
		// write data
		write(fp_out, tuple_buffer, 8 * (arity + 1));
		lineno++;
	}
	write_interned_pools();
	close(fp_out);
}

// void parsing_type

int main(int argc, char **argv)
{
	char *input_file;
	char *output_file;
	vector<unsigned> column_ordering;
	unsigned rel_tag = MIN_REL_TAG;
	string interned_prims_dir("");

	if (argc < 6 || argc > 7)
	{
		cerr << "usage: tsv_bin <input>.(c|t)sv <arity> <output> <buckets> <tag>\n";
		cerr << "usage: tsv_bin <input>.(c|t)sv <arity> <output> <buckets> <tag> <interned-prims-dir>\n";
		cerr << "This utility converts tab-separated value files (either .csv\n";
		cerr << "or .tsv, assuming tab separators regardless) into Slog\n";
		cerr << "u64-encoded input tuple files. The tuple file <output> will be\n";
		cerr << "written using the comma-separated ordering provided (e.g.,\n";
		cerr << "`1,2,0`). <buckets> is the number of buckets. If <tag>\n";
		cerr << "is provided, <tag> will be used as the relation tag.\n";
		cerr << "mode is either souffle or slog, in souffle mode, first \n";
		cerr << "line of fact will be type of column \n\n";
		cerr << "Primitive values are assumed to exist in $(strings|...).csv\n";
		cerr << "of the current directory or <interned-prims-dir> if it is provided.\n";
		exit(1);
	}

	if (argc > 6)
	{
		interned_prims_dir = argv[6];
	}

	input_file = argv[1];
	try
	{
		arity = stoi(argv[2]);
	}
	catch (...)
	{
		cerr << "Could not read arity " << argv[3] << endl;
		exit(1);
	}

	output_file = argv[3];

	// Fail if column order is broken
	// parse_column_order(argv[4], column_ordering);
	for (unsigned i = 1; i < arity + 1; i++)
	{
		column_ordering.push_back(i);
	}
	column_ordering.push_back(0);

	try
	{
		buckets = stoi(argv[4]);
		if (buckets < 1)
		{
			cerr << "Bad # buckets " << buckets << endl;
		}
	}
	catch (...)
	{
		cerr << "Could not read arity " << argv[3] << endl;
		exit(1);
	}

	int i = stoi(argv[5]);
	if (i < MIN_REL_TAG || i > MAX_REL_TAG)
	{
		cerr << "invalid tag (must be between "
			 << MIN_REL_TAG << " and " << MAX_REL_TAG << ").\n";
		exit(1);
	}
	rel_tag = (unsigned)i;

	// if output file exists check file size
	if (filesystem::exists(output_file))
	{
		current_tuple_id = filesystem::file_size(output_file) / ((arity + 1) * 8);
	}
	else
	{
		current_tuple_id = 0;
	}

	// read strings
	string_intern_file_path = interned_prims_dir + "/$strings.csv";
	read_strings(string_intern_file_path);

	string output_file_name = output_file;
	// stream this CSV output to slog
	file_to_slog(input_file, output_file, column_ordering, rel_tag);

	return 0;
}
