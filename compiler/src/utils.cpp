#include <string>
#include <iostream>
#include <filesystem>
#include <regex>
#include <unordered_map>
#include <tuple>

namespace fs = std::filesystem;
using namespace std;

struct RelArity {
  string rel;
  int arity;

  bool operator==(const RelArity& otherPos) const {
    if (this->rel == otherPos.rel && this->arity == otherPos.arity) return true;
    else return false;
  }

  struct HashFunction {
    size_t operator()(const RelArity& pos) const {
      return std::hash<string>()(pos.rel) ^ (std::hash<int>()(pos.arity) << 1);
    }
  };
};

vector<string> split(const char *phrase, string delimiter){
    vector<string> list;
    string s = string(phrase);
    size_t pos = 0;
    string token;
    while ((pos = s.find(delimiter)) != string::npos) {
        token = s.substr(0, pos);
        list.push_back(token);
        s.erase(0, pos + delimiter.length());
    }
    list.push_back(s);
    return list;
}

string* rel_arity_tags_cached_dir = nullptr;
std::unordered_map<RelArity, int, RelArity::HashFunction>* rel_arity_tags_map = nullptr;

/// gets the tag for the given rel-arity in the given database dir.
/// If no such rel-arity exists, returns -1
int get_tag_for_rel_arity(const string &rel_name, int arity, string& dir) {

    if( rel_arity_tags_cached_dir == nullptr || dir != *rel_arity_tags_cached_dir) {
        cout << "working ...\n";
        auto map = new std::unordered_map<RelArity, int, RelArity::HashFunction>();

        for (const auto & entry : filesystem::directory_iterator(dir)){
            auto file_name = entry.path().filename().generic_string();

            // format: {tag}.{rel_name}.{arity}.table
            auto parts = split(file_name.c_str(), ".");
            if (parts.size() == 4 && parts[3] == "table") {
                (*map)[RelArity{parts[1], (int) std::stoi(parts[2])}] = std::stoi(parts[0]);
            }
        }
        rel_arity_tags_map = map;
        rel_arity_tags_cached_dir = new string(dir);
    }

    if (rel_arity_tags_map->count({rel_name, arity}) > 0) {
        return rel_arity_tags_map->at({rel_name, arity});
    } else {
        return -1;
    }
}

int main() {

    string dir = "../local";
    cout << "tag for foo 3: " << get_tag_for_rel_arity(string("foo"), 3, dir) << "\n";
    cout << "tag for foo 6: " << get_tag_for_rel_arity(string("foo"), 6, dir) << "\n";
    cout << "tag for foo 60: " << get_tag_for_rel_arity(string("foo"), 60, dir) << "\n";

    cout << "tag for bar 4: " << get_tag_for_rel_arity(string("bar"), 4, dir) << "\n";
    cout << "tag for bar 5: " << get_tag_for_rel_arity(string("bar"), 5, dir) << "\n";
    cout << "tag for bar 65: " << get_tag_for_rel_arity(string("bar"), 65, dir) << "\n";

    return 0;
}