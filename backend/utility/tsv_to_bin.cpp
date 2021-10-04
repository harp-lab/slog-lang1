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
#define SYMBOL_TAG 3
#define BOOL_TAG 4

#define TUPLE_MASK_LENGTH 28
#define TUPLE_MASK 0x000000000FFFFFFF
#define BUCKET_MASK 0x00003FFFF0000000
#define BUCKET_MASK_LENGTH 18
#define TAG_MASK 0xFFFFC00000000000

using namespace std;

// globals
unordered_map<string, long> symbols_map;
long max_symbol_id;
unordered_map<string, long> strings_map;
long max_string_id = 0;
long current_tuple_id = 0;
unsigned arity;
unsigned index_length = 0;
unordered_set<u64> tuple_ids;
unsigned buckets;
string string_intern_file_path;
string symbol_intern_file_path;
string size_file_path;
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

	// otherwise, add additional columns after index
	// for (unsigned i = 0; i < arity + 1; i++)
	// {
	// 	// if not in result already, add it
	// 	if (!(find(result.begin(), result.end(), i) != result.end()))
	// 	{
	// 		result.push_back(i);
	// 	}
	// }
}

// populate symbols_map
void read_symbols(string filename)
{
	ifstream fp_in(filename, ios_base::in);
	string symbol_id;
	string symbol_value;
	unsigned lineno = 1;
	if (fp_in.fail())
	{
		cout << "warning: could not open $symbols.csv file. Will create it.\n";
	}
	while (fp_in.good())
	{
		try
		{
			getline(fp_in, symbol_id, '\t');
			if (!fp_in.good())
				break;
			getline(fp_in, symbol_value, '\n');
			long id = stoi(symbol_id);
			if (id < 0)
			{
				cerr << "error: ID < 0 is not allowed" << endl;
				exit(1);
			}
			symbols_map[symbol_value] = id;
			max_symbol_id = max(max_symbol_id, id);
			cout << symbol_id << "   " << symbol_value << endl;
		}
		catch (...)
		{
			cerr << "error reading symbols (line " << lineno << ")\n";
			exit(1);
		}
		lineno++;
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
		try
		{
			fp_in >> string_id;
			if (!fp_in.good())
				break;
			fp_in >> quoted(string_value);
			long id = stoi(string_id);
			if (id < 0)
			{
				cerr << "error: ID < 0 is not allowed" << endl;
				exit(1);
			}
			strings_map["\"" + string_value + "\""] = id;
			max_string_id = max(max_string_id, id);
		}
		catch (...)
		{
			cerr << "error reading strings (line " << lineno << ")\n";
			exit(1);
		}
		lineno++;
	}
}

// read size file if already exists
// set current_tuple_id to
void read_size(string filename)
{
	ifstream size_in(filename, ios_base::in);
	int col_size;
	if (size_in.fail())
	{
		cout << "warning: could not open size file" << filename << "file. Will create it.\n";
	}
	size_in >> current_tuple_id >> col_size;
	if (col_size != (arity + 1))
	{
		cerr << "error: arity in size file" << filename << " mismatch with input arity ! expect "
			 << arity + 1 << " in file but get" << col_size << endl;
		size_in.close();
		exit(1);
	}
	size_in.close();
}

// fill in $strings.csv, $symbols.csv, ...
void write_interned_pools()
{
	// const string strings_file = "./$strings.csv";
	// const string symbols_file = "./$symbols.csv";
	int strings = open(string_intern_file_path.c_str(), O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	for (const auto &[key, value] : strings_map)
	{
		string s = to_string(value) + "\t" + key + "\n";
		write(strings, s.c_str(), s.length());
	}
	int symbols = open(symbol_intern_file_path.c_str(), O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	for (const auto &[key, value] : symbols_map)
	{
		string s = to_string(value) + "\t" + key + "\n";
		write(symbols, s.c_str(), s.length());
	}
}

void write_size_file(string filename)
{
	cout << "updating size file ... \n";
	fstream size_file(filename);
	if (filesystem::exists(filename))
	{
		filesystem::resize_file(filename, 0);
		size_file.seekp(0);
	}
	size_file << current_tuple_id << '\n'
			  << arity + 1 << '\n';
	size_file.close();
}

#define BUF_SIZE 4096

// void parsing_type

void stream_file_to_slog(char *input_file, char *output_file,
						 vector<unsigned> &column_order, unsigned rel_tag)
{
	// string size_file = (string)output_file + ".size";
	int fp_in = open(input_file, O_CREAT | O_RDONLY, S_IRUSR | S_IRGRP | S_IROTH);
	int fp_out = open(output_file, O_CREAT | O_RDWR | O_APPEND, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
	// initialize state machine
	char buffer[BUF_SIZE + 1];
	buffer[BUF_SIZE] = 0; // to ensure end of buffer is nul-terminated
	unsigned last_idx;
	enum LexState
	{
		Scanning,
		ReadNumber,
		ReadString,
		ReadSymbol,
		StringEsc,
		StringEnd
	} cur_state = Scanning;
	string string_buffer; // builds up long strings
	int num_cols = -1;
	int cur_col = 1;
	int lineno = 1;
	int tuple_counts = 0;

	// compute the inverse column order to use to shuffle u64s
	vector<unsigned> inverse_column_order(column_order.size());
	for (size_t i = 0; i < column_order.size(); i++)
	{
		size_t j = 0;
		while (column_order[j] != i)
			j++;
		inverse_column_order[i] = j;
		// cout << "inverse_column_order[" << i << "] = " << j << endl;
	}

	u64 tuple_buffer[column_order.size()];

	// continuously scan the input in chunks of 4k
	do
	{
		if (cur_state != Scanning)
		{
			string_buffer += buffer[last_idx];
		}
		size_t num_bytes = read(fp_in, buffer, BUF_SIZE);
		last_idx = 0;
		if (num_bytes == 0)
			break;
		// loop over bytes to lex them
		for (size_t i = 0; i < num_bytes; i++)
		{
			char c = buffer[i];

			// If scanning, look to start lexing
			if (cur_state == Scanning)
			{
				if (c == '-' || (c >= '0' && c <= '9'))
				{
					// Number
					last_idx = i;
					cur_state = ReadNumber;
				}
				else if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_') || (c == '$'))
				{
					// Symbol
					last_idx = i;
					cur_state = ReadSymbol;
				}
				else if (c == '\"')
				{
					// String
					last_idx = i;
					cur_state = ReadString;
				}
				else if (c == '\t')
				{
					// Tab when already scanning, ignore this tab
					last_idx = i;
					cur_state = ReadSymbol;
					i--;
					continue;
				}
				else
				{
					cerr << "unexpected character " << c << " on line " << lineno << endl;
					exit(1);
				}
			}
			else if (cur_state == StringEsc)
			{
				// skip this character
				cur_state = ReadString;
			}
			else if (cur_state == ReadString && c == '\\')
			{
				cur_state = StringEsc;
			}
			else if (cur_state == ReadString && c != '\"')
			{
				cur_state = ReadString;
			}
			else if (cur_state == ReadString && c == '\"')
			{
				cur_state = StringEnd;
			}
			else if (c == '\n' && cur_state == Scanning)
			{
				// skip empty lines
				lineno++;
			}
			else if (c == '\t' || c == '\n')
			{
				u64 v = 0;
				// Done reading int/symbol/string
				char buf[i - last_idx + 1];
				for (size_t j = last_idx, x = 0; j < i; j++, x++)
				{
					buf[x] = buffer[j];
				}
				buf[i - last_idx] = '\0';
				string_buffer += buf;
				// now string_buffer contains the thing we want to write
				if (cur_state == ReadNumber)
				{
					u64 input = stoull(string_buffer);
					if (((TUPLE_MASK | BUCKET_MASK) & input) != input)
					{
						cerr << "Value outside of u48 range.\n";
					}
					v = TUPLE_MASK & input;
				}
				else if (cur_state == StringEnd)
				{
					// encode string, extend strings map if necessary
					string str = string_buffer; //string_buffer.substr(1,string_buffer.length()-2);
					auto itr = strings_map.find(str);
					v = STRING_TAG;
					v <<= TUPLE_MASK_LENGTH + BUCKET_MASK_LENGTH;
					if (itr == strings_map.end())
					{
						long new_id = max_string_id + 1;
						strings_map[str] = new_id;
						max_string_id = new_id;
						v |= new_id;
					}
					else
					{
						v |= strings_map[str];
					}
				}
				else if (cur_state == ReadSymbol)
				{
					// encode symbol, extend symbols map if necessary
					if (string_buffer == "")
					{
						// empty symbol
						string_buffer = "$_EMPTY";
					}
					auto itr = symbols_map.find(string_buffer);
					v = SYMBOL_TAG;
					v <<= TUPLE_MASK_LENGTH + BUCKET_MASK_LENGTH;
					if (itr == symbols_map.end())
					{
						long new_id = max_symbol_id + 1;
						cout << "symbol in buffer is " << string_buffer << endl;
						symbols_map[string_buffer] = new_id;
						max_symbol_id = new_id;
						v |= new_id;
					}
					else
					{
						v |= symbols_map[string_buffer];
					}
				}

				// cout << "original " << cur_col << " inverse " << inverse_column_order[cur_col] << endl;
				tuple_buffer[inverse_column_order[cur_col]] = v;

				if (c == '\t')
				{
					string_buffer = "";
					cur_col++;
					cur_state = Scanning;
				}
				else if (c == '\n')
				{
					string_buffer = "";
					// cur_col++;
					// first row
					if (num_cols == -1)
					{
						num_cols = cur_col;
						if (num_cols != column_order.size() - 1)
						{
							cerr << "should have " << column_order.size() - 1
								 << " columns, but found " << num_cols << " cols on first row.\n";
							cerr << "note: make sure to include the 0 column for new fact IDs.\n";
							exit(1);
						}
					}
					else
					{
						// validate cols == first row cols
						if (num_cols != cur_col)
						{
							cerr << "Invalid column layout (are you using tabs rather than whitespace) on line "
								 << lineno << endl;
							exit(1);
						}
					}

					// generate intern id
					u64 tid = rel_tag;
					tid <<= (TUPLE_MASK_LENGTH + BUCKET_MASK_LENGTH);
					tid |= ((hash_tuple(tuple_buffer, index_length) % buckets)) << TUPLE_MASK_LENGTH;
					tid |= current_tuple_id++;
					tuple_buffer[inverse_column_order[0]] = tid;

					// write data
					write(fp_out, tuple_buffer, 8 * (arity + 1));
					tuple_counts++;
					cur_col = 1;
					lineno++;
					cur_state = Scanning;
				}
			}
		}
	} while (true);
	write_size_file(size_file_path);
	write_interned_pools();
	close(fp_in);
	close(fp_out);
}

int main(int argc, char **argv)
{
	char *input_file;
	char *output_file;
	vector<unsigned> column_ordering;
	unsigned rel_tag = MIN_REL_TAG;
	string interned_prims_dir("");
	// symbols_map[""] = 0;

	if (argc < 5 || argc > 7)
	{
		cout << "usage: tsv_bin <input>.(c|t)sv <arity> <output> <index> <buckets>\n";
		cout << "usage: tsv_bin <input>.(c|t)sv <arity> <output> <index> <buckets> <interned-prims-dir>\n";
		cout << "usage: tsv_bin <input>.(c|t)sv <arity> <output> <index> <buckets> <interned-prims-dir> <mode> \n\n";
		cout << "This utility converts tab-separated value files (either .csv\n";
		cout << "or .tsv, assuming tab separators regardless) into Slog\n";
		cout << "u64-encoded input tuple files. The tuple file <output> will be\n";
		cout << "written using the comma-separated ordering provided (e.g.,\n";
		cout << "`1,2,0`). <buckets> is the number of buckets. If <tag>\n";
		cout << "is provided, <tag> will be used as the relation tag.\n";
		cout << "mode is either souffle or slog, in souffle mode, first \n";
		cout << "line of fact will be type of column \n\n";
		cout << "Primitive values are assumed to exist in $(symbols|strings|...).csv\n";
		cout << "of the current directory or <interned-prims-dir> if it is provided.\n";
		exit(1);
	}

	if (argc > 6)
	{
		interned_prims_dir = argv[6];
	}

	if (argc > 7)
	{
		mode = argv[7];
	}

	if (argc > 8)
	{
		int i = stoi(argv[8]);
		if (i < MIN_REL_TAG || i > MAX_REL_TAG)
		{
			cerr << "invalid tag (must be between "
				 << MIN_REL_TAG << " and " << MAX_REL_TAG << ").\n";
			exit(1);
		}
		rel_tag = (unsigned)i;
	}

	input_file = argv[1];
	try
	{
		arity = stoi(argv[2]);
	}
	catch (...)
	{
		cout << "Could not read arity " << argv[3] << endl;
		exit(1);
	}

	output_file = argv[3];

	// Fail if column order is broken
	parse_column_order(argv[4], column_ordering);

	try
	{
		buckets = stoi(argv[5]);
		if (buckets < 1)
		{
			cerr << "Bad # buckets " << buckets << endl;
		}
	}
	catch (...)
	{
		cout << "Could not read arity " << argv[3] << endl;
		exit(1);
	}

	// read symbols and strings
	string_intern_file_path = interned_prims_dir + "/$strings.csv";
	symbol_intern_file_path = interned_prims_dir + "/$symbols.csv";
	read_symbols(symbol_intern_file_path);
	read_strings(string_intern_file_path);

	// read size
	string output_file_name = output_file;
	size_file_path = output_file_name + ".size";
	read_size(size_file_path);
	// stream this CSV output to slog
	stream_file_to_slog(input_file, output_file, column_ordering, rel_tag);

	return 0;
}
