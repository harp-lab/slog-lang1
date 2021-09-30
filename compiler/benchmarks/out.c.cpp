
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
struct t_btree_4__0_1_2_3__1__15 {
using t_tuple = Tuple<RamDomain, 4>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1,2,3>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[4];
std::copy(ramDomain, ramDomain + 4, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3) {
RamDomain data[4] = {a0,a1,a2,a3};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_4__0_1_2_3__1__15& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[1] = MIN_RAM_DOMAIN;
high[1] = MAX_RAM_DOMAIN;
low[2] = MIN_RAM_DOMAIN;
high[2] = MAX_RAM_DOMAIN;
low[3] = MIN_RAM_DOMAIN;
high[3] = MAX_RAM_DOMAIN;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
}
range<t_ind_0::iterator> equalRange_15(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_15(const t_tuple& t) const {
context h;
return equalRange_15(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 4 direct b-tree index [0,1,2,3]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_4__0_1_2_3__15 {
using t_tuple = Tuple<RamDomain, 4>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1,2,3>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[4];
std::copy(ramDomain, ramDomain + 4, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3) {
RamDomain data[4] = {a0,a1,a2,a3};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_4__0_1_2_3__15& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_15(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_15(const t_tuple& t) const {
context h;
return equalRange_15(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 4 direct b-tree index [0,1,2,3]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_1__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_1__0__1& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 1 direct b-tree index [0]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_2__0_1__3 {
using t_tuple = Tuple<RamDomain, 2>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_2__0_1__3& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t) const {
context h;
return equalRange_3(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023 {
using t_tuple = Tuple<RamDomain, 10>;
Table<t_tuple> dataTable;
Lock insert_lock;
using t_ind_0 = btree_multiset<const t_tuple*, index_utils::deref_compare<typename index_utils::comparator<1,2,3,4,6,7,8,9>>>;
t_ind_0 ind_0;
using t_ind_1 = btree_set<const t_tuple*, index_utils::deref_compare<typename index_utils::comparator<0,1,2,3,4,5,6,7,8,9>>>;
t_ind_1 ind_1;
using iterator_0 = IterDerefWrapper<typename t_ind_0::iterator>;
using iterator_1 = IterDerefWrapper<typename t_ind_1::iterator>;
using iterator = iterator_1;
struct context {
t_ind_0::operation_hints hints_0;
t_ind_1::operation_hints hints_1;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
const t_tuple* masterCopy = nullptr;
{
auto lease = insert_lock.acquire();
if (contains(t, h)) return false;
masterCopy = &dataTable.insert(t);
ind_1.insert(masterCopy, h.hints_1);
}
ind_0.insert(masterCopy, h.hints_0);
return true;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[10];
std::copy(ramDomain, ramDomain + 10, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3,RamDomain a4,RamDomain a5,RamDomain a6,RamDomain a7,RamDomain a8,RamDomain a9) {
RamDomain data[10] = {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
bool contains(const t_tuple& t, context& h) const {
return ind_1.contains(&t, h.hints_1);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_1.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_1.find(&t, h.hints_1);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_1.begin(),ind_1.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_1.begin(),ind_1.end());
}
range<iterator_1> equalRange_1(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[1] = MIN_RAM_DOMAIN;
high[1] = MAX_RAM_DOMAIN;
low[2] = MIN_RAM_DOMAIN;
high[2] = MAX_RAM_DOMAIN;
low[3] = MIN_RAM_DOMAIN;
high[3] = MAX_RAM_DOMAIN;
low[4] = MIN_RAM_DOMAIN;
high[4] = MAX_RAM_DOMAIN;
low[5] = MIN_RAM_DOMAIN;
high[5] = MAX_RAM_DOMAIN;
low[6] = MIN_RAM_DOMAIN;
high[6] = MAX_RAM_DOMAIN;
low[7] = MIN_RAM_DOMAIN;
high[7] = MAX_RAM_DOMAIN;
low[8] = MIN_RAM_DOMAIN;
high[8] = MAX_RAM_DOMAIN;
low[9] = MIN_RAM_DOMAIN;
high[9] = MAX_RAM_DOMAIN;
return range<iterator_1>(ind_1.lower_bound(&low, h.hints_1), ind_1.upper_bound(&high, h.hints_1));
}
range<iterator_1> equalRange_1(const t_tuple& t) const {
context h; return equalRange_1(t, h);
}
range<iterator_0> equalRange_30(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[0] = MIN_RAM_DOMAIN;
high[0] = MAX_RAM_DOMAIN;
low[5] = MIN_RAM_DOMAIN;
high[5] = MAX_RAM_DOMAIN;
low[6] = MIN_RAM_DOMAIN;
high[6] = MAX_RAM_DOMAIN;
low[7] = MIN_RAM_DOMAIN;
high[7] = MAX_RAM_DOMAIN;
low[8] = MIN_RAM_DOMAIN;
high[8] = MAX_RAM_DOMAIN;
low[9] = MIN_RAM_DOMAIN;
high[9] = MAX_RAM_DOMAIN;
return range<iterator_0>(ind_0.lower_bound(&low, h.hints_0), ind_0.upper_bound(&high, h.hints_0));
}
range<iterator_0> equalRange_30(const t_tuple& t) const {
context h; return equalRange_30(t, h);
}
range<iterator_0> equalRange_990(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[0] = MIN_RAM_DOMAIN;
high[0] = MAX_RAM_DOMAIN;
low[5] = MIN_RAM_DOMAIN;
high[5] = MAX_RAM_DOMAIN;
return range<iterator_0>(ind_0.lower_bound(&low, h.hints_0), ind_0.upper_bound(&high, h.hints_0));
}
range<iterator_0> equalRange_990(const t_tuple& t) const {
context h; return equalRange_990(t, h);
}
range<iterator_1> equalRange_1023(const t_tuple& t, context& h) const {
auto pos = find(t, h);
auto fin = end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<iterator_1> equalRange_1023(const t_tuple& t) const {
context h; return equalRange_1023(t, h);
}
bool empty() const {
return ind_1.empty();
}
std::vector<range<iterator>> partition() const {
std::vector<range<iterator>> res;
for (const auto& cur : ind_1.getChunks(400)) {
    res.push_back(make_range(derefIter(cur.begin()), derefIter(cur.end())));
}
return res;
}
void purge() {
ind_0.clear();
ind_1.clear();
dataTable.clear();
}
iterator begin() const {
return ind_1.begin();
}
iterator end() const {
return ind_1.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 10 indirect b-tree index [1,2,3,4,6,7,8,9]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
const auto& stats_1 = ind_1.getHintStatistics();
o << prefix << "arity 10 indirect b-tree index [0,1,2,3,4,5,6,7,8,9]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_1.inserts.getHits() << "/" << stats_1.inserts.getMisses() << "/" << stats_1.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_1.contains.getHits() << "/" << stats_1.contains.getMisses() << "/" << stats_1.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_1.lower_bound.getHits() << "/" << stats_1.lower_bound.getMisses() << "/" << stats_1.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_1.upper_bound.getHits() << "/" << stats_1.upper_bound.getMisses() << "/" << stats_1.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_5__0_1_2_3_4__31 {
using t_tuple = Tuple<RamDomain, 5>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1,2,3,4>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[5];
std::copy(ramDomain, ramDomain + 5, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3,RamDomain a4) {
RamDomain data[5] = {a0,a1,a2,a3,a4};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_5__0_1_2_3_4__31& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_31(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_31(const t_tuple& t) const {
context h;
return equalRange_31(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 5 direct b-tree index [0,1,2,3,4]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023 {
using t_tuple = Tuple<RamDomain, 10>;
Table<t_tuple> dataTable;
Lock insert_lock;
using t_ind_0 = btree_set<const t_tuple*, index_utils::deref_compare<typename index_utils::comparator<0,5,6,7,8,9,1,2,3,4>>>;
t_ind_0 ind_0;
using iterator_0 = IterDerefWrapper<typename t_ind_0::iterator>;
using iterator = iterator_0;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
const t_tuple* masterCopy = nullptr;
{
auto lease = insert_lock.acquire();
if (contains(t, h)) return false;
masterCopy = &dataTable.insert(t);
ind_0.insert(masterCopy, h.hints_0);
}
return true;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[10];
std::copy(ramDomain, ramDomain + 10, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3,RamDomain a4,RamDomain a5,RamDomain a6,RamDomain a7,RamDomain a8,RamDomain a9) {
RamDomain data[10] = {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(&t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(&t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator_0> equalRange_993(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[1] = MIN_RAM_DOMAIN;
high[1] = MAX_RAM_DOMAIN;
low[2] = MIN_RAM_DOMAIN;
high[2] = MAX_RAM_DOMAIN;
low[3] = MIN_RAM_DOMAIN;
high[3] = MAX_RAM_DOMAIN;
low[4] = MIN_RAM_DOMAIN;
high[4] = MAX_RAM_DOMAIN;
return range<iterator_0>(ind_0.lower_bound(&low, h.hints_0), ind_0.upper_bound(&high, h.hints_0));
}
range<iterator_0> equalRange_993(const t_tuple& t) const {
context h; return equalRange_993(t, h);
}
range<iterator_0> equalRange_1023(const t_tuple& t, context& h) const {
auto pos = find(t, h);
auto fin = end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<iterator_0> equalRange_1023(const t_tuple& t) const {
context h; return equalRange_1023(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
std::vector<range<iterator>> res;
for (const auto& cur : ind_0.getChunks(400)) {
    res.push_back(make_range(derefIter(cur.begin()), derefIter(cur.end())));
}
return res;
}
void purge() {
ind_0.clear();
dataTable.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 10 indirect b-tree index [0,5,6,7,8,9,1,2,3,4]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767 {
using t_tuple = Tuple<RamDomain, 15>;
Table<t_tuple> dataTable;
Lock insert_lock;
using t_ind_0 = btree_set<const t_tuple*, index_utils::deref_compare<typename index_utils::comparator<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14>>>;
t_ind_0 ind_0;
using iterator_0 = IterDerefWrapper<typename t_ind_0::iterator>;
using iterator = iterator_0;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
const t_tuple* masterCopy = nullptr;
{
auto lease = insert_lock.acquire();
if (contains(t, h)) return false;
masterCopy = &dataTable.insert(t);
ind_0.insert(masterCopy, h.hints_0);
}
return true;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[15];
std::copy(ramDomain, ramDomain + 15, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2,RamDomain a3,RamDomain a4,RamDomain a5,RamDomain a6,RamDomain a7,RamDomain a8,RamDomain a9,RamDomain a10,RamDomain a11,RamDomain a12,RamDomain a13,RamDomain a14) {
RamDomain data[15] = {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(&t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(&t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator_0> equalRange_32767(const t_tuple& t, context& h) const {
auto pos = find(t, h);
auto fin = end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<iterator_0> equalRange_32767(const t_tuple& t) const {
context h; return equalRange_32767(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
std::vector<range<iterator>> res;
for (const auto& cur : ind_0.getChunks(400)) {
    res.push_back(make_range(derefIter(cur.begin()), derefIter(cur.end())));
}
return res;
}
void purge() {
ind_0.clear();
dataTable.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 15 indirect b-tree index [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};

class Sf_out : public SouffleProgram {
private:
static inline bool regex_wrapper(const std::string& pattern, const std::string& text) {
   bool result = false; 
   try { result = std::regex_match(text, std::regex(pattern)); } catch(...) { 
     std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\"" << text << "\").\n";
}
   return result;
}
private:
static inline std::string substr_wrapper(const std::string& str, size_t idx, size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
private:
static inline RamDomain wrapper_tonumber(const std::string& str) {
   RamDomain result=0; 
   try { result = stord(str); } catch(...) { 
     std::cerr << "error: wrong string provided by to_number(\"";
     std::cerr << str << "\") functor.\n";
     raise(SIGFPE);
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable
;// -- Table: Lam
std::unique_ptr<t_btree_4__0_1_2_3__1__15> rel_1_Lam = std::make_unique<t_btree_4__0_1_2_3__1__15>();
souffle::RelationWrapper<0,t_btree_4__0_1_2_3__1__15,Tuple<RamDomain,4>,4> wrapper_rel_1_Lam;
// -- Table: App
std::unique_ptr<t_btree_4__0_1_2_3__15> rel_2_App = std::make_unique<t_btree_4__0_1_2_3__15>();
souffle::RelationWrapper<1,t_btree_4__0_1_2_3__15,Tuple<RamDomain,4>,4> wrapper_rel_2_App;
// -- Table: Prog
std::unique_ptr<t_btree_1__0__1> rel_3_Prog = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<2,t_btree_1__0__1,Tuple<RamDomain,1>,1> wrapper_rel_3_Prog;
// -- Table: Var
std::unique_ptr<t_btree_2__0_1__3> rel_4_Var = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<3,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_4_Var;
// -- Table: Time
std::unique_ptr<t_btree_4__0_1_2_3__15> rel_5_Time = std::make_unique<t_btree_4__0_1_2_3__15>();
souffle::RelationWrapper<4,t_btree_4__0_1_2_3__15,Tuple<RamDomain,4>,4> wrapper_rel_5_Time;
// -- Table: @delta_Time
std::unique_ptr<t_btree_4__0_1_2_3__15> rel_6_delta_Time = std::make_unique<t_btree_4__0_1_2_3__15>();
// -- Table: @new_Time
std::unique_ptr<t_btree_4__0_1_2_3__15> rel_7_new_Time = std::make_unique<t_btree_4__0_1_2_3__15>();
// -- Table: AEval
std::unique_ptr<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023> rel_8_AEval = std::make_unique<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023>();
souffle::RelationWrapper<5,t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023,Tuple<RamDomain,10>,10> wrapper_rel_8_AEval;
// -- Table: @delta_AEval
std::unique_ptr<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023> rel_9_delta_AEval = std::make_unique<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023>();
// -- Table: @new_AEval
std::unique_ptr<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023> rel_10_new_AEval = std::make_unique<t_btree_10__1_2_3_4_6_7_8_9__0_1_2_3_4_5_6_7_8_9__1__30__990__1023>();
// -- Table: ReachesCfg
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_11_ReachesCfg = std::make_unique<t_btree_5__0_1_2_3_4__31>();
souffle::RelationWrapper<6,t_btree_5__0_1_2_3_4__31,Tuple<RamDomain,5>,5> wrapper_rel_11_ReachesCfg;
// -- Table: @delta_ReachesCfg
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_12_delta_ReachesCfg = std::make_unique<t_btree_5__0_1_2_3_4__31>();
// -- Table: @new_ReachesCfg
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_13_new_ReachesCfg = std::make_unique<t_btree_5__0_1_2_3_4__31>();
// -- Table: Store
std::unique_ptr<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023> rel_14_Store = std::make_unique<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023>();
souffle::RelationWrapper<7,t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023,Tuple<RamDomain,10>,10> wrapper_rel_14_Store;
// -- Table: @delta_Store
std::unique_ptr<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023> rel_15_delta_Store = std::make_unique<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023>();
// -- Table: @new_Store
std::unique_ptr<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023> rel_16_new_Store = std::make_unique<t_btree_10__0_5_6_7_8_9_1_2_3_4__993__1023>();
// -- Table: Step
std::unique_ptr<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767> rel_17_Step = std::make_unique<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767>();
souffle::RelationWrapper<8,t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767,Tuple<RamDomain,15>,15> wrapper_rel_17_Step;
// -- Table: @delta_Step
std::unique_ptr<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767> rel_18_delta_Step = std::make_unique<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767>();
// -- Table: @new_Step
std::unique_ptr<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767> rel_19_new_Step = std::make_unique<t_btree_15__0_1_2_3_4_5_6_7_8_9_10_11_12_13_14__32767>();
// -- Table: ReachesClo
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_20_ReachesClo = std::make_unique<t_btree_5__0_1_2_3_4__31>();
souffle::RelationWrapper<9,t_btree_5__0_1_2_3_4__31,Tuple<RamDomain,5>,5> wrapper_rel_20_ReachesClo;
// -- Table: @delta_ReachesClo
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_21_delta_ReachesClo = std::make_unique<t_btree_5__0_1_2_3_4__31>();
// -- Table: @new_ReachesClo
std::unique_ptr<t_btree_5__0_1_2_3_4__31> rel_22_new_ReachesClo = std::make_unique<t_btree_5__0_1_2_3_4__31>();
public:
Sf_out() : 
wrapper_rel_1_Lam(*rel_1_Lam,symTable,"Lam",std::array<const char *,4>{{"i:number","i:number","i:number","i:number"}},std::array<const char *,4>{{"id","x","y","idbody"}}),

wrapper_rel_2_App(*rel_2_App,symTable,"App",std::array<const char *,4>{{"i:number","i:number","i:number","i:number"}},std::array<const char *,4>{{"id","f","a0","a1"}}),

wrapper_rel_3_Prog(*rel_3_Prog,symTable,"Prog",std::array<const char *,1>{{"i:number"}},std::array<const char *,1>{{"x"}}),

wrapper_rel_4_Var(*rel_4_Var,symTable,"Var",std::array<const char *,2>{{"i:number","i:number"}},std::array<const char *,2>{{"id","var"}}),

wrapper_rel_5_Time(*rel_5_Time,symTable,"Time",std::array<const char *,4>{{"i:number","i:number","i:number","i:number"}},std::array<const char *,4>{{"dt0","dt1","dt2","dt3"}}),

wrapper_rel_8_AEval(*rel_8_AEval,symTable,"AEval",std::array<const char *,10>{{"i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number"}},std::array<const char *,10>{{"id","dt0","dt1","dt2","dt3","id1","dot0","dot1","dot2","dot3"}}),

wrapper_rel_11_ReachesCfg(*rel_11_ReachesCfg,symTable,"ReachesCfg",std::array<const char *,5>{{"i:number","i:number","i:number","i:number","i:number"}},std::array<const char *,5>{{"id","dt0","dt1","dt2","dt3"}}),

wrapper_rel_14_Store(*rel_14_Store,symTable,"Store",std::array<const char *,10>{{"i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number"}},std::array<const char *,10>{{"var","dt0","dt1","dt2","dt3","stored_lam","dot0","dot1","dot2","dot3"}}),

wrapper_rel_17_Step(*rel_17_Step,symTable,"Step",std::array<const char *,15>{{"i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number","i:number"}},std::array<const char *,15>{{"from","dt0","dt1","dt2","dt3","toid","dto_t0","dto_t1","dto_t2","dto_t3","clo_lam","dclo_t0","dclo_t1","dclo_t2","dclo_t3"}}),

wrapper_rel_20_ReachesClo(*rel_20_ReachesClo,symTable,"ReachesClo",std::array<const char *,5>{{"i:number","i:number","i:number","i:number","i:number"}},std::array<const char *,5>{{"id","dt0","dt1","dt2","dt3"}}){
addRelation("Lam",&wrapper_rel_1_Lam,false,false);
addRelation("App",&wrapper_rel_2_App,false,false);
addRelation("Prog",&wrapper_rel_3_Prog,false,false);
addRelation("Var",&wrapper_rel_4_Var,false,false);
addRelation("Time",&wrapper_rel_5_Time,false,false);
addRelation("AEval",&wrapper_rel_8_AEval,false,true);
addRelation("ReachesCfg",&wrapper_rel_11_ReachesCfg,false,true);
addRelation("Store",&wrapper_rel_14_Store,false,true);
addRelation("Step",&wrapper_rel_17_Step,false,true);
addRelation("ReachesClo",&wrapper_rel_20_ReachesClo,false,true);
}
~Sf_out() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

#if defined(__EMBEDDED_SOUFFLE__) && defined(_OPENMP)
omp_set_num_threads(8);
#endif

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Lam(12,0,1,13).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [73:1-73:19])_");
rel_1_Lam->insert(RamDomain(12),RamDomain(0),RamDomain(1),RamDomain(13));
SignalHandler::instance()->setMsg(R"_(Lam(15,0,1,16).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [74:1-74:19])_");
rel_1_Lam->insert(RamDomain(15),RamDomain(0),RamDomain(1),RamDomain(16));
SignalHandler::instance()->setMsg(R"_(Lam(2,0,1,3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [75:1-75:17])_");
rel_1_Lam->insert(RamDomain(2),RamDomain(0),RamDomain(1),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(Lam(7,0,1,8).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [76:1-76:17])_");
rel_1_Lam->insert(RamDomain(7),RamDomain(0),RamDomain(1),RamDomain(8));
SignalHandler::instance()->setMsg(R"_(Lam(18,0,1,19).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [77:1-77:19])_");
rel_1_Lam->insert(RamDomain(18),RamDomain(0),RamDomain(1),RamDomain(19));
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
SignalHandler::instance()->setMsg(R"_(App(16,17,18,23).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [78:1-78:21])_");
rel_2_App->insert(RamDomain(16),RamDomain(17),RamDomain(18),RamDomain(23));
SignalHandler::instance()->setMsg(R"_(App(13,14,15,24).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [79:1-79:21])_");
rel_2_App->insert(RamDomain(13),RamDomain(14),RamDomain(15),RamDomain(24));
SignalHandler::instance()->setMsg(R"_(App(8,9,10,11).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [80:1-80:19])_");
rel_2_App->insert(RamDomain(8),RamDomain(9),RamDomain(10),RamDomain(11));
SignalHandler::instance()->setMsg(R"_(App(19,20,21,22).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [81:1-81:21])_");
rel_2_App->insert(RamDomain(19),RamDomain(20),RamDomain(21),RamDomain(22));
SignalHandler::instance()->setMsg(R"_(App(3,4,5,6).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [82:1-82:17])_");
rel_2_App->insert(RamDomain(3),RamDomain(4),RamDomain(5),RamDomain(6));
SignalHandler::instance()->setMsg(R"_(App(1,2,7,12).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [83:1-83:18])_");
rel_2_App->insert(RamDomain(1),RamDomain(2),RamDomain(7),RamDomain(12));
}();
/* END STRATUM 1 */
/* BEGIN STRATUM 2 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Prog(1).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [71:1-71:9])_");
rel_3_Prog->insert(RamDomain(1));
}();
/* END STRATUM 2 */
/* BEGIN STRATUM 3 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Var(24,1).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [72:1-72:12])_");
rel_4_Var->insert(RamDomain(24),RamDomain(1));
}();
/* END STRATUM 3 */
/* BEGIN STRATUM 4 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Time(x,x,x,x) :- 
   Prog(x).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [19:1-19:49])_");
if(!(rel_3_Prog->empty())) {
{
auto part = rel_3_Prog->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_3_Prog_op_ctxt,rel_3_Prog->createContext());
CREATE_OP_CONTEXT(rel_5_Time_op_ctxt,rel_5_Time->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
Tuple<RamDomain,4> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0])}});
rel_5_Time->insert(tuple,READ_OP_CONTEXT(rel_5_Time_op_ctxt));
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
rel_6_delta_Time->insertAll(*rel_5_Time);
rel_9_delta_AEval->insertAll(*rel_8_AEval);
SignalHandler::instance()->setMsg(R"_(ReachesCfg(x,x,x,x,x) :- 
   Prog(x).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [19:1-19:49])_");
if(!(rel_3_Prog->empty())) {
{
auto part = rel_3_Prog->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_3_Prog_op_ctxt,rel_3_Prog->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[0])}});
rel_11_ReachesCfg->insert(tuple,READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt));
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
rel_12_delta_ReachesCfg->insertAll(*rel_11_ReachesCfg);
rel_15_delta_Store->insertAll(*rel_14_Store);
rel_18_delta_Step->insertAll(*rel_17_Step);
rel_21_delta_ReachesClo->insertAll(*rel_20_ReachesClo);
iter = 0;
for(;;) {
SECTIONS_START;
SECTION_START;
SignalHandler::instance()->setMsg(R"_(Time(id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_7_new_Time_op_ctxt,rel_7_new_Time->createContext());
CREATE_OP_CONTEXT(rel_5_Time_op_ctxt,rel_5_Time->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && !(rel_5_Time->contains(Tuple<RamDomain,4>({{env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_5_Time_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,4> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_7_new_Time->insert(tuple,READ_OP_CONTEXT(rel_7_new_Time_op_ctxt));
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Time(id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_7_new_Time_op_ctxt,rel_7_new_Time->createContext());
CREATE_OP_CONTEXT(rel_5_Time_op_ctxt,rel_5_Time->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_5_Time->contains(Tuple<RamDomain,4>({{env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_5_Time_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,4> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_7_new_Time->insert(tuple,READ_OP_CONTEXT(rel_7_new_Time_op_ctxt));
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Time(id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_7_new_Time_op_ctxt,rel_7_new_Time->createContext());
CREATE_OP_CONTEXT(rel_5_Time_op_ctxt,rel_5_Time->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_5_Time->contains(Tuple<RamDomain,4>({{env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_5_Time_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,4> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_7_new_Time->insert(tuple,READ_OP_CONTEXT(rel_7_new_Time_op_ctxt));
break;
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Time(id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_7_new_Time_op_ctxt,rel_7_new_Time->createContext());
CREATE_OP_CONTEXT(rel_5_Time_op_ctxt,rel_5_Time->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_5_Time->contains(Tuple<RamDomain,4>({{env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_5_Time_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !rel_9_delta_AEval->equalRange_990(Tuple<RamDomain,10>({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)).empty()) {
Tuple<RamDomain,4> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_7_new_Time->insert(tuple,READ_OP_CONTEXT(rel_7_new_Time_op_ctxt));
break;
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(AEval(id,t0,t1,t2,t3,id,t0,t1,t2,t3) :- 
   Time(t0,t1,t2,t3),
   Lam(id,_,_,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [37:1-37:74])_");
if(!(rel_1_Lam->empty()) && !(rel_6_delta_Time->empty())) {
{
auto part = rel_6_delta_Time->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_10_new_AEval_op_ctxt,rel_10_new_AEval->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_6_delta_Time_op_ctxt,rel_6_delta_Time->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_1_Lam) {
if( !(rel_8_AEval->contains(Tuple<RamDomain,10>({{env1[0],env0[0],env0[1],env0[2],env0[3],env1[0],env0[0],env0[1],env0[2],env0[3]}}),READ_OP_CONTEXT(rel_8_AEval_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env1[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env0[2]),static_cast<RamDomain>(env0[3]),static_cast<RamDomain>(env1[0]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env0[2]),static_cast<RamDomain>(env0[3])}});
rel_10_new_AEval->insert(tuple,READ_OP_CONTEXT(rel_10_new_AEval_op_ctxt));
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(AEval(ref_id,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   Var(ref_id,x),
   ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Store(x,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [40:1-42:60])_");
if(!(rel_14_Store->empty()) && !(rel_21_delta_ReachesClo->empty()) && !(rel_4_Var->empty())) {
{
auto part = rel_4_Var->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_4_Var_op_ctxt,rel_4_Var->createContext());
CREATE_OP_CONTEXT(rel_10_new_AEval_op_ctxt,rel_10_new_AEval->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_15_delta_Store_op_ctxt,rel_15_delta_Store->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_21_delta_ReachesClo_op_ctxt,rel_21_delta_ReachesClo->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_21_delta_ReachesClo) {
const Tuple<RamDomain,10> key({{env0[1],0,0,0,0,env1[0],env1[1],env1[2],env1[3],env1[4]}});
auto range = rel_14_Store->equalRange_993(key,READ_OP_CONTEXT(rel_14_Store_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_8_AEval->contains(Tuple<RamDomain,10>({{env0[0],env2[1],env2[2],env2[3],env2[4],env1[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_8_AEval_op_ctxt))) && !(rel_15_delta_Store->contains(Tuple<RamDomain,10>({{env0[1],env2[1],env2[2],env2[3],env2[4],env1[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_15_delta_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env2[1]),static_cast<RamDomain>(env2[2]),static_cast<RamDomain>(env2[3]),static_cast<RamDomain>(env2[4]),static_cast<RamDomain>(env1[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4])}});
rel_10_new_AEval->insert(tuple,READ_OP_CONTEXT(rel_10_new_AEval_op_ctxt));
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(AEval(ref_id,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   Var(ref_id,x),
   ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Store(x,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [40:1-42:60])_");
if(!(rel_15_delta_Store->empty()) && !(rel_20_ReachesClo->empty()) && !(rel_4_Var->empty())) {
{
auto part = rel_4_Var->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_4_Var_op_ctxt,rel_4_Var->createContext());
CREATE_OP_CONTEXT(rel_10_new_AEval_op_ctxt,rel_10_new_AEval->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_20_ReachesClo_op_ctxt,rel_20_ReachesClo->createContext());
CREATE_OP_CONTEXT(rel_15_delta_Store_op_ctxt,rel_15_delta_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_20_ReachesClo) {
const Tuple<RamDomain,10> key({{env0[1],0,0,0,0,env1[0],env1[1],env1[2],env1[3],env1[4]}});
auto range = rel_15_delta_Store->equalRange_993(key,READ_OP_CONTEXT(rel_15_delta_Store_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_8_AEval->contains(Tuple<RamDomain,10>({{env0[0],env2[1],env2[2],env2[3],env2[4],env1[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_8_AEval_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env2[1]),static_cast<RamDomain>(env2[2]),static_cast<RamDomain>(env2[3]),static_cast<RamDomain>(env2[4]),static_cast<RamDomain>(env1[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4])}});
rel_10_new_AEval->insert(tuple,READ_OP_CONTEXT(rel_10_new_AEval_op_ctxt));
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(AEval(id_ref,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   Step(_,_,_,_,_,_,t0,t1,t2,t3,tclo_lam,_,_,_,_),
   Lam(tclo_lam,p0,p1,_),
   Var(id_ref,x),
   AEval(id_ref,_,_,_,_,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   p0 != x,
   p1 != x.
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [45:1-51:11])_");
if(!(rel_8_AEval->empty()) && !(rel_4_Var->empty()) && !(rel_1_Lam->empty()) && !(rel_18_delta_Step->empty())) {
{
auto part = rel_18_delta_Step->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_4_Var_op_ctxt,rel_4_Var->createContext());
CREATE_OP_CONTEXT(rel_10_new_AEval_op_ctxt,rel_10_new_AEval->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_18_delta_Step_op_ctxt,rel_18_delta_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,4> key({{env0[10],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env1 : range) {
for(const auto& env2 : *rel_4_Var) {
if( ((env1[2]) != (env2[1])) && ((env1[1]) != (env2[1]))) {
const Tuple<RamDomain,10> key({{env2[0],0,0,0,0,0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_1(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_8_AEval->contains(Tuple<RamDomain,10>({{env2[0],env0[6],env0[7],env0[8],env0[9],env3[5],env3[6],env3[7],env3[8],env3[9]}}),READ_OP_CONTEXT(rel_8_AEval_op_ctxt))) && !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env3[1],env3[2],env3[3],env3[4],env3[5],env3[6],env3[7],env3[8],env3[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env2[0]),static_cast<RamDomain>(env0[6]),static_cast<RamDomain>(env0[7]),static_cast<RamDomain>(env0[8]),static_cast<RamDomain>(env0[9]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env3[6]),static_cast<RamDomain>(env3[7]),static_cast<RamDomain>(env3[8]),static_cast<RamDomain>(env3[9])}});
rel_10_new_AEval->insert(tuple,READ_OP_CONTEXT(rel_10_new_AEval_op_ctxt));
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(AEval(id_ref,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   Step(_,_,_,_,_,_,t0,t1,t2,t3,tclo_lam,_,_,_,_),
   Lam(tclo_lam,p0,p1,_),
   Var(id_ref,x),
   AEval(id_ref,_,_,_,_,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   p0 != x,
   p1 != x.
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [45:1-51:11])_");
if(!(rel_9_delta_AEval->empty()) && !(rel_4_Var->empty()) && !(rel_1_Lam->empty()) && !(rel_17_Step->empty())) {
{
auto part = rel_17_Step->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_4_Var_op_ctxt,rel_4_Var->createContext());
CREATE_OP_CONTEXT(rel_10_new_AEval_op_ctxt,rel_10_new_AEval->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_17_Step_op_ctxt,rel_17_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
const Tuple<RamDomain,4> key({{env0[10],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env1 : range) {
for(const auto& env2 : *rel_4_Var) {
if( ((env1[2]) != (env2[1])) && ((env1[1]) != (env2[1]))) {
const Tuple<RamDomain,10> key({{env2[0],0,0,0,0,0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_1(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_8_AEval->contains(Tuple<RamDomain,10>({{env2[0],env0[6],env0[7],env0[8],env0[9],env3[5],env3[6],env3[7],env3[8],env3[9]}}),READ_OP_CONTEXT(rel_8_AEval_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env2[0]),static_cast<RamDomain>(env0[6]),static_cast<RamDomain>(env0[7]),static_cast<RamDomain>(env0[8]),static_cast<RamDomain>(env0[9]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env3[6]),static_cast<RamDomain>(env3[7]),static_cast<RamDomain>(env3[8]),static_cast<RamDomain>(env3[9])}});
rel_10_new_AEval->insert(tuple,READ_OP_CONTEXT(rel_10_new_AEval_op_ctxt));
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(ReachesCfg(id_body,id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt,rel_13_new_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env4[3],env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_13_new_ReachesCfg->insert(tuple,READ_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesCfg(id_body,id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt,rel_13_new_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env4[3],env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_13_new_ReachesCfg->insert(tuple,READ_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesCfg(id_body,id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt,rel_13_new_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env4[3],env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_13_new_ReachesCfg->insert(tuple,READ_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt));
}
}
break;
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesCfg(id_body,id,t0,t1,t2) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt,rel_13_new_ReachesCfg->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !rel_9_delta_AEval->equalRange_990(Tuple<RamDomain,10>({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)).empty()) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env3[3],env0[0],env1[1],env1[2],env1[3]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env3[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3])}});
rel_13_new_ReachesCfg->insert(tuple,READ_OP_CONTEXT(rel_13_new_ReachesCfg_op_ctxt));
}
}
break;
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(Store(x,id,t0,t1,t2,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,x,_,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[1],env0[0],env1[1],env1[2],env1[3],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[1]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env2[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(x,id,t0,t1,t2,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,x,_,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[1],env0[0],env1[1],env1[2],env1[3],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[1]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env2[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(x,id,t0,t1,t2,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,x,_,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[1],env0[0],env1[1],env1[2],env1[3],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[1]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env2[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
break;
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(x,id,t0,t1,t2,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,a0_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,x,_,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !rel_9_delta_AEval->equalRange_990(Tuple<RamDomain,10>({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)).empty()) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env3[1],env0[0],env1[1],env1[2],env1[3],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env3[1]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env2[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(y,id,t0,t1,t2,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,y,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[2],env0[0],env1[1],env1[2],env1[3],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[2]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(y,id,t0,t1,t2,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,y,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[2],env0[0],env1[1],env1[2],env1[3],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[2]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(y,id,t0,t1,t2,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,y,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[2],env0[0],env1[1],env1[2],env1[3],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[2]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Store(y,id,t0,t1,t2,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,_,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,a1_lam,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,y,_).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_14_Store_op_ctxt,rel_14_Store->createContext());
CREATE_OP_CONTEXT(rel_16_new_Store_op_ctxt,rel_16_new_Store->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_9_delta_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env3 : range) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_14_Store->contains(Tuple<RamDomain,10>({{env4[2],env0[0],env1[1],env1[2],env1[3],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_14_Store_op_ctxt)))) {
Tuple<RamDomain,10> tuple({{static_cast<RamDomain>(env4[2]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env3[5]),static_cast<RamDomain>(env2[6]),static_cast<RamDomain>(env2[7]),static_cast<RamDomain>(env2[8]),static_cast<RamDomain>(env2[9])}});
rel_16_new_Store->insert(tuple,READ_OP_CONTEXT(rel_16_new_Store_op_ctxt));
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(Step(id,t0,t1,t2,t3,id_body,id,t0,t1,t2,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_19_new_Step_op_ctxt,rel_19_new_Step->createContext());
CREATE_OP_CONTEXT(rel_17_Step_op_ctxt,rel_17_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_17_Step->contains(Tuple<RamDomain,15>({{env0[0],env1[1],env1[2],env1[3],env1[4],env4[3],env0[0],env1[1],env1[2],env1[3],env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_17_Step_op_ctxt)))) {
Tuple<RamDomain,15> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4]),static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_19_new_Step->insert(tuple,READ_OP_CONTEXT(rel_19_new_Step_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Step(id,t0,t1,t2,t3,id_body,id,t0,t1,t2,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_19_new_Step_op_ctxt,rel_19_new_Step->createContext());
CREATE_OP_CONTEXT(rel_17_Step_op_ctxt,rel_17_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_17_Step->contains(Tuple<RamDomain,15>({{env0[0],env1[1],env1[2],env1[3],env1[4],env4[3],env0[0],env1[1],env1[2],env1[3],env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_17_Step_op_ctxt)))) {
Tuple<RamDomain,15> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4]),static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_19_new_Step->insert(tuple,READ_OP_CONTEXT(rel_19_new_Step_op_ctxt));
}
}
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Step(id,t0,t1,t2,t3,id_body,id,t0,t1,t2,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_19_new_Step_op_ctxt,rel_19_new_Step->createContext());
CREATE_OP_CONTEXT(rel_17_Step_op_ctxt,rel_17_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env4 : range) {
if( !(rel_17_Step->contains(Tuple<RamDomain,15>({{env0[0],env1[1],env1[2],env1[3],env1[4],env4[3],env0[0],env1[1],env1[2],env1[3],env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_17_Step_op_ctxt)))) {
Tuple<RamDomain,15> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4]),static_cast<RamDomain>(env4[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_19_new_Step->insert(tuple,READ_OP_CONTEXT(rel_19_new_Step_op_ctxt));
}
}
break;
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(Step(id,t0,t1,t2,t3,id_body,id,t0,t1,t2,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   Lam(clo_lam,_,_,id_body).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_19_new_Step_op_ctxt,rel_19_new_Step->createContext());
CREATE_OP_CONTEXT(rel_17_Step_op_ctxt,rel_17_Step->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !rel_9_delta_AEval->equalRange_990(Tuple<RamDomain,10>({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)).empty()) {
const Tuple<RamDomain,4> key({{env1[5],0,0,0}});
auto range = rel_1_Lam->equalRange_1(key,READ_OP_CONTEXT(rel_1_Lam_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_17_Step->contains(Tuple<RamDomain,15>({{env0[0],env1[1],env1[2],env1[3],env1[4],env3[3],env0[0],env1[1],env1[2],env1[3],env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_17_Step_op_ctxt)))) {
Tuple<RamDomain,15> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[4]),static_cast<RamDomain>(env3[3]),static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env1[2]),static_cast<RamDomain>(env1[3]),static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_19_new_Step->insert(tuple,READ_OP_CONTEXT(rel_19_new_Step_op_ctxt));
}
}
break;
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTION_START;
SignalHandler::instance()->setMsg(R"_(ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_20_ReachesClo_op_ctxt,rel_20_ReachesClo->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt,rel_22_new_ReachesClo->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_9_delta_AEval) {
if( !(rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) && !(rel_20_ReachesClo->contains(Tuple<RamDomain,5>({{env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_20_ReachesClo_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_22_new_ReachesClo->insert(tuple,READ_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt));
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_12_delta_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_20_ReachesClo_op_ctxt,rel_20_ReachesClo->createContext());
CREATE_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt,rel_12_delta_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt,rel_22_new_ReachesClo->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_20_ReachesClo->contains(Tuple<RamDomain,5>({{env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_20_ReachesClo_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_12_delta_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_12_delta_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env2[0],env1[1],env1[2],env1[3],env1[4],env2[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_22_new_ReachesClo->insert(tuple,READ_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt));
break;
}
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_8_AEval->empty()) && !(rel_9_delta_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_20_ReachesClo_op_ctxt,rel_20_ReachesClo->createContext());
CREATE_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt,rel_22_new_ReachesClo->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_20_ReachesClo->contains(Tuple<RamDomain,5>({{env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_20_ReachesClo_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_9_delta_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt));
for(const auto& env2 : range) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}});
auto range = rel_8_AEval->equalRange_990(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env3 : range) {
if( !(rel_9_delta_AEval->contains(Tuple<RamDomain,10>({{env3[0],env1[1],env1[2],env1[3],env1[4],env3[5],env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)))) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_22_new_ReachesClo->insert(tuple,READ_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt));
break;
}
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SignalHandler::instance()->setMsg(R"_(ReachesClo(clo_lam,clo_t0,clo_t1,clo_t2,clo_t3) :- 
   App(id,_,_,_),
   AEval(_,t0,t1,t2,t3,clo_lam,clo_t0,clo_t1,clo_t2,clo_t3),
   Lam(clo_lam,_,_,_),
   ReachesCfg(id,t0,t1,t2,t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3),
   AEval(_,t0,t1,t2,t3,_,a0_t0,a0_t1,a0_t2,a0_t3).
in file /Users/krismicinski/projects/slog-lang/benchmarks/worstcase-2-terms-4-m.dl [22:1-34:30])_");
if(!(rel_9_delta_AEval->empty()) && !(rel_8_AEval->empty()) && !(rel_11_ReachesCfg->empty()) && !(rel_1_Lam->empty()) && !(rel_8_AEval->empty()) && !(rel_2_App->empty())) {
{
auto part = rel_2_App->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_Lam_op_ctxt,rel_1_Lam->createContext());
CREATE_OP_CONTEXT(rel_2_App_op_ctxt,rel_2_App->createContext());
CREATE_OP_CONTEXT(rel_8_AEval_op_ctxt,rel_8_AEval->createContext());
CREATE_OP_CONTEXT(rel_9_delta_AEval_op_ctxt,rel_9_delta_AEval->createContext());
CREATE_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt,rel_11_ReachesCfg->createContext());
CREATE_OP_CONTEXT(rel_20_ReachesClo_op_ctxt,rel_20_ReachesClo->createContext());
CREATE_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt,rel_22_new_ReachesClo->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{
for(const auto& env0 : *it) {
for(const auto& env1 : *rel_8_AEval) {
if( !(rel_20_ReachesClo->contains(Tuple<RamDomain,5>({{env1[5],env1[6],env1[7],env1[8],env1[9]}}),READ_OP_CONTEXT(rel_20_ReachesClo_op_ctxt))) && !rel_1_Lam->equalRange_1(Tuple<RamDomain,4>({{env1[5],0,0,0}}),READ_OP_CONTEXT(rel_1_Lam_op_ctxt)).empty() && rel_11_ReachesCfg->contains(Tuple<RamDomain,5>({{env0[0],env1[1],env1[2],env1[3],env1[4]}}),READ_OP_CONTEXT(rel_11_ReachesCfg_op_ctxt))) {
const Tuple<RamDomain,10> key({{0,env1[1],env1[2],env1[3],env1[4],0,0,0,0,0}});
auto range = rel_8_AEval->equalRange_30(key,READ_OP_CONTEXT(rel_8_AEval_op_ctxt));
for(const auto& env2 : range) {
if( !rel_9_delta_AEval->equalRange_990(Tuple<RamDomain,10>({{0,env1[1],env1[2],env1[3],env1[4],0,env2[6],env2[7],env2[8],env2[9]}}),READ_OP_CONTEXT(rel_9_delta_AEval_op_ctxt)).empty()) {
Tuple<RamDomain,5> tuple({{static_cast<RamDomain>(env1[5]),static_cast<RamDomain>(env1[6]),static_cast<RamDomain>(env1[7]),static_cast<RamDomain>(env1[8]),static_cast<RamDomain>(env1[9])}});
rel_22_new_ReachesClo->insert(tuple,READ_OP_CONTEXT(rel_22_new_ReachesClo_op_ctxt));
break;
}
}
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
}
SECTION_END
SECTIONS_END;
if(rel_7_new_Time->empty() && rel_10_new_AEval->empty() && rel_13_new_ReachesCfg->empty() && rel_16_new_Store->empty() && rel_19_new_Step->empty() && rel_22_new_ReachesClo->empty()) break;
rel_5_Time->insertAll(*rel_7_new_Time);
std::swap(rel_6_delta_Time, rel_7_new_Time);
rel_7_new_Time->purge();
rel_8_AEval->insertAll(*rel_10_new_AEval);
std::swap(rel_9_delta_AEval, rel_10_new_AEval);
rel_10_new_AEval->purge();
rel_11_ReachesCfg->insertAll(*rel_13_new_ReachesCfg);
std::swap(rel_12_delta_ReachesCfg, rel_13_new_ReachesCfg);
rel_13_new_ReachesCfg->purge();
rel_14_Store->insertAll(*rel_16_new_Store);
std::swap(rel_15_delta_Store, rel_16_new_Store);
rel_16_new_Store->purge();
rel_17_Step->insertAll(*rel_19_new_Step);
std::swap(rel_18_delta_Step, rel_19_new_Step);
rel_19_new_Step->purge();
rel_20_ReachesClo->insertAll(*rel_22_new_ReachesClo);
std::swap(rel_21_delta_ReachesClo, rel_22_new_ReachesClo);
rel_22_new_ReachesClo->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled()) rel_6_delta_Time->purge();
if (!isHintsProfilingEnabled()) rel_7_new_Time->purge();
if (!isHintsProfilingEnabled()) rel_9_delta_AEval->purge();
if (!isHintsProfilingEnabled()) rel_10_new_AEval->purge();
if (!isHintsProfilingEnabled()) rel_12_delta_ReachesCfg->purge();
if (!isHintsProfilingEnabled()) rel_13_new_ReachesCfg->purge();
if (!isHintsProfilingEnabled()) rel_15_delta_Store->purge();
if (!isHintsProfilingEnabled()) rel_16_new_Store->purge();
if (!isHintsProfilingEnabled()) rel_18_delta_Step->purge();
if (!isHintsProfilingEnabled()) rel_19_new_Step->purge();
if (!isHintsProfilingEnabled()) rel_21_delta_ReachesClo->purge();
if (!isHintsProfilingEnabled()) rel_22_new_ReachesClo->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3\tid1\tdot0\tdot1\tdot2\tdot3"},{"filename","./AEval.csv"},{"name","AEval"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_8_AEval);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3"},{"filename","./ReachesCfg.csv"},{"name","ReachesCfg"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_11_ReachesCfg);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","var\tdt0\tdt1\tdt2\tdt3\tstored_lam\tdot0\tdot1\tdot2\tdot3"},{"filename","./Store.csv"},{"name","Store"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_14_Store);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tdt0\tdt1\tdt2\tdt3\ttoid\tdto_t0\tdto_t1\tdto_t2\tdto_t3\tclo_lam\tdclo_t0\tdclo_t1\tdclo_t2\tdclo_t3"},{"filename","./Step.csv"},{"name","Step"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_17_Step);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3"},{"filename","./ReachesClo.csv"},{"name","ReachesClo"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_20_ReachesClo);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_3_Prog->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_1_Lam->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_2_App->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_4_Var->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_5_Time->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_8_AEval->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_11_ReachesCfg->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_14_Store->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_17_Step->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_20_ReachesClo->purge();
}();
/* END STRATUM 4 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_Lam:\n";
rel_1_Lam->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_App:\n";
rel_2_App->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_Prog:\n";
rel_3_Prog->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_Var:\n";
rel_4_Var->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_5_Time:\n";
rel_5_Time->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_6_delta_Time:\n";
rel_6_delta_Time->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_7_new_Time:\n";
rel_7_new_Time->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_8_AEval:\n";
rel_8_AEval->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_9_delta_AEval:\n";
rel_9_delta_AEval->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_10_new_AEval:\n";
rel_10_new_AEval->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_11_ReachesCfg:\n";
rel_11_ReachesCfg->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_12_delta_ReachesCfg:\n";
rel_12_delta_ReachesCfg->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_13_new_ReachesCfg:\n";
rel_13_new_ReachesCfg->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_14_Store:\n";
rel_14_Store->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_15_delta_Store:\n";
rel_15_delta_Store->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_16_new_Store:\n";
rel_16_new_Store->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_17_Step:\n";
rel_17_Step->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_18_delta_Step:\n";
rel_18_delta_Step->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_19_new_Step:\n";
rel_19_new_Step->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_20_ReachesClo:\n";
rel_20_ReachesClo->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_21_delta_ReachesClo:\n";
rel_21_delta_ReachesClo->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_22_new_ReachesClo:\n";
rel_22_new_ReachesClo->printHintStatistics(std::cout,"  ");
std::cout << "\n";
}
SignalHandler::instance()->reset();
}
public:
void run(size_t stratumIndex = (size_t) -1) override { runFunction(".", ".", stratumIndex, false); }
public:
void runAll(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1) override { runFunction(inputDirectory, outputDirectory, stratumIndex, true);
}
public:
void printAll(std::string outputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3\tid1\tdot0\tdot1\tdot2\tdot3"},{"filename","./AEval.csv"},{"name","AEval"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_8_AEval);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3"},{"filename","./ReachesCfg.csv"},{"name","ReachesCfg"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_11_ReachesCfg);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","var\tdt0\tdt1\tdt2\tdt3\tstored_lam\tdot0\tdot1\tdot2\tdot3"},{"filename","./Store.csv"},{"name","Store"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_14_Store);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","from\tdt0\tdt1\tdt2\tdt3\ttoid\tdto_t0\tdto_t1\tdto_t2\tdto_t3\tclo_lam\tdclo_t0\tdclo_t1\tdclo_t2\tdclo_t3"},{"filename","./Step.csv"},{"name","Step"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_17_Step);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","id\tdt0\tdt1\tdt2\tdt3"},{"filename","./ReachesClo.csv"},{"name","ReachesClo"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_20_ReachesClo);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = ".") override {
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_8_AEval");
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_8_AEval);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_11_ReachesCfg");
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_11_ReachesCfg);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_14_Store");
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_14_Store);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_17_Step");
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_17_Step);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_20_ReachesClo");
IOSystem::getInstance().getWriter(std::vector<bool>({0,0,0,0,0}), symTable, ioDirectives, false)->writeAll(*rel_20_ReachesClo);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
};
SouffleProgram *newInstance_out(){return new Sf_out;}
SymbolTable *getST_out(SouffleProgram *p){return &reinterpret_cast<Sf_out*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_out: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_out();
};
public:
factory_Sf_out() : ProgramFactory("out"){}
};
static factory_Sf_out __factory_Sf_out_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(worstcase-2-terms-4-m.dl)",
R"(.)",
R"(.)",
false,
R"()",
8,
-1);
if (!opt.parse(argc,argv)) return 1;
#if defined(_OPENMP) 
omp_set_nested(true);

#endif
souffle::Sf_out obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
