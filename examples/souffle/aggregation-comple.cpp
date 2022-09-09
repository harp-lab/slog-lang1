#include "souffle/CompiledSouffle.h"
#include "souffle/SignalHandler.h"
#include "souffle/SouffleInterface.h"
#include "souffle/datastructure/BTree.h"
#include "souffle/io/IOSystem.h"
#include <any>
namespace functors {
extern "C" {}
} // namespace functors
namespace souffle::t_btree_i__0__1 {
struct Type {
  static constexpr Relation::arity_type Arity = 1;
  using t_tuple = Tuple<RamDomain, 1>;
  struct t_comparator_0 {
    int operator()(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1
             : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0]))
                 ? 1
                 : (0);
    }
    bool less(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
    }
    bool equal(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
    }
  };
  using t_ind_0 = btree_set<t_tuple, t_comparator_0>;
  t_ind_0 ind_0;
  using iterator = t_ind_0::iterator;
  struct context {
    t_ind_0::operation_hints hints_0_lower;
    t_ind_0::operation_hints hints_0_upper;
  };
  context createContext() { return context(); }
  bool insert(const t_tuple &t);
  bool insert(const t_tuple &t, context &h);
  bool insert(const RamDomain *ramDomain);
  bool insert(RamDomain a0);
  bool contains(const t_tuple &t, context &h) const;
  bool contains(const t_tuple &t) const;
  std::size_t size() const;
  iterator find(const t_tuple &t, context &h) const;
  iterator find(const t_tuple &t) const;
  range<iterator> lowerUpperRange_0(const t_tuple & /* lower */,
                                    const t_tuple & /* upper */,
                                    context & /* h */) const;
  range<iterator> lowerUpperRange_0(const t_tuple & /* lower */,
                                    const t_tuple & /* upper */) const;
  range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower,
                                             const t_tuple &upper,
                                             context &h) const;
  range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower,
                                             const t_tuple &upper) const;
  bool empty() const;
  std::vector<range<iterator>> partition() const;
  void purge();
  iterator begin() const;
  iterator end() const;
  void printStatistics(std::ostream &o) const;
};
} // namespace souffle::t_btree_i__0__1
namespace souffle::t_btree_i__0__1 {
using t_ind_0 = Type::t_ind_0;
using iterator = Type::iterator;
using context = Type::context;
bool Type::insert(const t_tuple &t) {
  context h;
  return insert(t, h);
}
bool Type::insert(const t_tuple &t, context &h) {
  if (ind_0.insert(t, h.hints_0_lower)) {
    return true;
  } else
    return false;
}
bool Type::insert(const RamDomain *ramDomain) {
  RamDomain data[1];
  std::copy(ramDomain, ramDomain + 1, data);
  const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
  context h;
  return insert(tuple, h);
}
bool Type::insert(RamDomain a0) {
  RamDomain data[1] = {a0};
  return insert(data);
}
bool Type::contains(const t_tuple &t, context &h) const {
  return ind_0.contains(t, h.hints_0_lower);
}
bool Type::contains(const t_tuple &t) const {
  context h;
  return contains(t, h);
}
std::size_t Type::size() const { return ind_0.size(); }
iterator Type::find(const t_tuple &t, context &h) const {
  return ind_0.find(t, h.hints_0_lower);
}
iterator Type::find(const t_tuple &t) const {
  context h;
  return find(t, h);
}
range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */,
                                        const t_tuple & /* upper */,
                                        context & /* h */) const {
  return range<iterator>(ind_0.begin(), ind_0.end());
}
range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */,
                                        const t_tuple & /* upper */) const {
  return range<iterator>(ind_0.begin(), ind_0.end());
}
range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower,
                                                 const t_tuple &upper,
                                                 context &h) const {
  t_comparator_0 comparator;
  int cmp = comparator(lower, upper);
  if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {
      fin = pos;
      ++fin;
    }
    return make_range(pos, fin);
  }
  if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
  }
  return make_range(ind_0.lower_bound(lower, h.hints_0_lower),
                    ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower,
                                                 const t_tuple &upper) const {
  context h;
  return lowerUpperRange_1(lower, upper, h);
}
bool Type::empty() const { return ind_0.empty(); }
std::vector<range<iterator>> Type::partition() const {
  return ind_0.getChunks(400);
}
void Type::purge() { ind_0.clear(); }
iterator Type::begin() const { return ind_0.begin(); }
iterator Type::end() const { return ind_0.end(); }
void Type::printStatistics(std::ostream &o) const {
  o << " arity 1 direct b-tree index 0 lex-order [0]\n";
  ind_0.printStats(o);
}
} // namespace souffle::t_btree_i__0__1
namespace souffle::t_btree_i__0__2__1 {
struct Type {
  static constexpr Relation::arity_type Arity = 1;
  using t_tuple = Tuple<RamDomain, 1>;
  struct t_comparator_0 {
    int operator()(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1
             : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0]))
                 ? 1
                 : (0);
    }
    bool less(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
    }
    bool equal(const t_tuple &a, const t_tuple &b) const {
      return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
    }
  };
  using t_ind_0 = btree_set<t_tuple, t_comparator_0>;
  t_ind_0 ind_0;
  using iterator = t_ind_0::iterator;
  struct context {
    t_ind_0::operation_hints hints_0_lower;
    t_ind_0::operation_hints hints_0_upper;
  };
  context createContext() { return context(); }
  bool insert(const t_tuple &t);
  bool insert(const t_tuple &t, context &h);
  bool insert(const RamDomain *ramDomain);
  bool insert(RamDomain a0);
  bool contains(const t_tuple &t, context &h) const;
  bool contains(const t_tuple &t) const;
  std::size_t size() const;
  iterator find(const t_tuple &t, context &h) const;
  iterator find(const t_tuple &t) const;
  range<iterator> lowerUpperRange_0(const t_tuple & /* lower */,
                                    const t_tuple & /* upper */,
                                    context & /* h */) const;
  range<iterator> lowerUpperRange_0(const t_tuple & /* lower */,
                                    const t_tuple & /* upper */) const;
  range<t_ind_0::iterator> lowerUpperRange_2(const t_tuple &lower,
                                             const t_tuple &upper,
                                             context &h) const;
  range<t_ind_0::iterator> lowerUpperRange_2(const t_tuple &lower,
                                             const t_tuple &upper) const;
  range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower,
                                             const t_tuple &upper,
                                             context &h) const;
  range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple &lower,
                                             const t_tuple &upper) const;
  bool empty() const;
  std::vector<range<iterator>> partition() const;
  void purge();
  iterator begin() const;
  iterator end() const;
  void printStatistics(std::ostream &o) const;
};
} // namespace souffle::t_btree_i__0__2__1
namespace souffle::t_btree_i__0__2__1 {
using t_ind_0 = Type::t_ind_0;
using iterator = Type::iterator;
using context = Type::context;
bool Type::insert(const t_tuple &t) {
  context h;
  return insert(t, h);
}
bool Type::insert(const t_tuple &t, context &h) {
  if (ind_0.insert(t, h.hints_0_lower)) {
    return true;
  } else
    return false;
}
bool Type::insert(const RamDomain *ramDomain) {
  RamDomain data[1];
  std::copy(ramDomain, ramDomain + 1, data);
  const t_tuple &tuple = reinterpret_cast<const t_tuple &>(data);
  context h;
  return insert(tuple, h);
}
bool Type::insert(RamDomain a0) {
  RamDomain data[1] = {a0};
  return insert(data);
}
bool Type::contains(const t_tuple &t, context &h) const {
  return ind_0.contains(t, h.hints_0_lower);
}
bool Type::contains(const t_tuple &t) const {
  context h;
  return contains(t, h);
}
std::size_t Type::size() const { return ind_0.size(); }
iterator Type::find(const t_tuple &t, context &h) const {
  return ind_0.find(t, h.hints_0_lower);
}
iterator Type::find(const t_tuple &t) const {
  context h;
  return find(t, h);
}
range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */,
                                        const t_tuple & /* upper */,
                                        context & /* h */) const {
  return range<iterator>(ind_0.begin(), ind_0.end());
}
range<iterator> Type::lowerUpperRange_0(const t_tuple & /* lower */,
                                        const t_tuple & /* upper */) const {
  return range<iterator>(ind_0.begin(), ind_0.end());
}
range<t_ind_0::iterator> Type::lowerUpperRange_2(const t_tuple &lower,
                                                 const t_tuple &upper,
                                                 context &h) const {
  t_comparator_0 comparator;
  int cmp = comparator(lower, upper);
  if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
  }
  return make_range(ind_0.lower_bound(lower, h.hints_0_lower),
                    ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> Type::lowerUpperRange_2(const t_tuple &lower,
                                                 const t_tuple &upper) const {
  context h;
  return lowerUpperRange_2(lower, upper, h);
}
range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower,
                                                 const t_tuple &upper,
                                                 context &h) const {
  t_comparator_0 comparator;
  int cmp = comparator(lower, upper);
  if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {
      fin = pos;
      ++fin;
    }
    return make_range(pos, fin);
  }
  if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
  }
  return make_range(ind_0.lower_bound(lower, h.hints_0_lower),
                    ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> Type::lowerUpperRange_1(const t_tuple &lower,
                                                 const t_tuple &upper) const {
  context h;
  return lowerUpperRange_1(lower, upper, h);
}
bool Type::empty() const { return ind_0.empty(); }
std::vector<range<iterator>> Type::partition() const {
  return ind_0.getChunks(400);
}
void Type::purge() { ind_0.clear(); }
iterator Type::begin() const { return ind_0.begin(); }
iterator Type::end() const { return ind_0.end(); }
void Type::printStatistics(std::ostream &o) const {
  o << " arity 1 direct b-tree index 0 lex-order [0]\n";
  ind_0.printStats(o);
}
} // namespace souffle::t_btree_i__0__2__1
namespace souffle {
class Stratum_W_092de3ee4021ad8d {
public:
  Stratum_W_092de3ee4021ad8d(SymbolTable &symTable, RecordTable &recordTable,
                             bool &pruneImdtRels, bool &performIO,
                             SignalHandler *&signalHandler,
                             std::atomic<std::size_t> &iter,
                             std::atomic<RamDomain> &ctr,
                             std::string &inputDirectory,
                             std::string &outputDirectory,
                             t_btree_i__0__1::Type &rel_W_6746ec4c1affe72f,
                             t_btree_i__0__1::Type &rel_X_a722a93eab38baea,
                             t_btree_i__0__2__1::Type &rel_Y_9b1b019904fc8f67);
  void run([[maybe_unused]] const std::vector<RamDomain> &args,
           [[maybe_unused]] std::vector<RamDomain> &ret);

private:
  SymbolTable &symTable;
  RecordTable &recordTable;
  bool &pruneImdtRels;
  bool &performIO;
  SignalHandler *&signalHandler;
  std::atomic<std::size_t> &iter;
  std::atomic<RamDomain> &ctr;
  std::string &inputDirectory;
  std::string &outputDirectory;
  t_btree_i__0__1::Type *rel_W_6746ec4c1affe72f;
  t_btree_i__0__1::Type *rel_X_a722a93eab38baea;
  t_btree_i__0__2__1::Type *rel_Y_9b1b019904fc8f67;
};
} // namespace souffle
namespace souffle {
Stratum_W_092de3ee4021ad8d::Stratum_W_092de3ee4021ad8d(
    SymbolTable &symTable, RecordTable &recordTable, bool &pruneImdtRels,
    bool &performIO, SignalHandler *&signalHandler,
    std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr,
    std::string &inputDirectory, std::string &outputDirectory,
    t_btree_i__0__1::Type &rel_W_6746ec4c1affe72f,
    t_btree_i__0__1::Type &rel_X_a722a93eab38baea,
    t_btree_i__0__2__1::Type &rel_Y_9b1b019904fc8f67)
    : symTable(symTable), recordTable(recordTable),
      pruneImdtRels(pruneImdtRels), performIO(performIO),
      signalHandler(signalHandler), iter(iter), ctr(ctr),
      inputDirectory(inputDirectory), outputDirectory(outputDirectory),
      rel_W_6746ec4c1affe72f(&rel_W_6746ec4c1affe72f),
      rel_X_a722a93eab38baea(&rel_X_a722a93eab38baea),
      rel_Y_9b1b019904fc8f67(&rel_Y_9b1b019904fc8f67) {}

void Stratum_W_092de3ee4021ad8d::run(
    [[maybe_unused]] const std::vector<RamDomain> &args,
    [[maybe_unused]] std::vector<RamDomain> &ret) {
  signalHandler->setMsg(R"_(W(@generator_0) :- 
   X(n),
   @generator_0 = sum m : { Y(m),m < n }.
in file aggregation-complex.dl [9:1-9:43])_");
  if (!(rel_X_a722a93eab38baea->empty())) {
    [&]() {
      CREATE_OP_CONTEXT(rel_W_6746ec4c1affe72f_op_ctxt,
                        rel_W_6746ec4c1affe72f->createContext());
      CREATE_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt,
                        rel_X_a722a93eab38baea->createContext());
      CREATE_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt,
                        rel_Y_9b1b019904fc8f67->createContext());
      for (const auto &env0 : *rel_X_a722a93eab38baea) {
        Tuple<RamDomain, 1> env1;
        bool shouldRunNested = false;
        shouldRunNested = true;
        RamSigned res0 = 0;
        auto range = rel_Y_9b1b019904fc8f67->lowerUpperRange_2(
            Tuple<RamDomain, 1>{{ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},
            Tuple<RamDomain, 1>{{ramBitCast(env0[0])}},
            READ_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt));
        for (const auto &env1 : range) {
          if ((ramBitCast<RamDomain>(env1[0]) !=
               ramBitCast<RamDomain>(env0[0]))) {
            shouldRunNested = true;
            res0 += ramBitCast<RamSigned>(env1[0]);
          }
        }
        env1[0] = ramBitCast(res0);
        if (shouldRunNested) {
          if ((ramBitCast<RamDomain>(env1[0]) ==
               ramBitCast<RamDomain>(env1[0]))) {
            Tuple<RamDomain, 1> tuple{{ramBitCast(env1[0])}};
            rel_W_6746ec4c1affe72f->insert(
                tuple, READ_OP_CONTEXT(rel_W_6746ec4c1affe72f_op_ctxt));
          }
        }
      }
    }();
  }
  if (performIO) {
    try {
      std::map<std::string, std::string> directiveMap(
          {{"IO", "file"},
           {"attributeNames", "x"},
           {"auxArity", "0"},
           {"name", "W"},
           {"operation", "output"},
           {"output-dir", "."},
           {"params", "{\"records\": {}, \"relation\": {\"arity\": 1, "
                      "\"params\": [\"x\"]}}"},
           {"types", "{\"ADTs\": {}, \"records\": {}, \"relation\": "
                     "{\"arity\": 1, \"types\": [\"i:number\"]}}"}});
      if (!outputDirectory.empty()) {
        directiveMap["output-dir"] = outputDirectory;
      }
      IOSystem::getInstance()
          .getWriter(directiveMap, symTable, recordTable)
          ->writeAll(*rel_W_6746ec4c1affe72f);
    } catch (std::exception &e) {
      std::cerr << e.what();
      exit(1);
    }
  }
  if (pruneImdtRels)
    rel_X_a722a93eab38baea->purge();
  if (pruneImdtRels)
    rel_Y_9b1b019904fc8f67->purge();
}

} // namespace souffle

namespace souffle {
class Stratum_X_cdfd55e9ff53eb8f {
public:
  Stratum_X_cdfd55e9ff53eb8f(SymbolTable &symTable, RecordTable &recordTable,
                             bool &pruneImdtRels, bool &performIO,
                             SignalHandler *&signalHandler,
                             std::atomic<std::size_t> &iter,
                             std::atomic<RamDomain> &ctr,
                             std::string &inputDirectory,
                             std::string &outputDirectory,
                             t_btree_i__0__1::Type &rel_X_a722a93eab38baea);
  void run([[maybe_unused]] const std::vector<RamDomain> &args,
           [[maybe_unused]] std::vector<RamDomain> &ret);

private:
  SymbolTable &symTable;
  RecordTable &recordTable;
  bool &pruneImdtRels;
  bool &performIO;
  SignalHandler *&signalHandler;
  std::atomic<std::size_t> &iter;
  std::atomic<RamDomain> &ctr;
  std::string &inputDirectory;
  std::string &outputDirectory;
  t_btree_i__0__1::Type *rel_X_a722a93eab38baea;
};
} // namespace souffle
namespace souffle {
Stratum_X_cdfd55e9ff53eb8f::Stratum_X_cdfd55e9ff53eb8f(
    SymbolTable &symTable, RecordTable &recordTable, bool &pruneImdtRels,
    bool &performIO, SignalHandler *&signalHandler,
    std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr,
    std::string &inputDirectory, std::string &outputDirectory,
    t_btree_i__0__1::Type &rel_X_a722a93eab38baea)
    : symTable(symTable), recordTable(recordTable),
      pruneImdtRels(pruneImdtRels), performIO(performIO),
      signalHandler(signalHandler), iter(iter), ctr(ctr),
      inputDirectory(inputDirectory), outputDirectory(outputDirectory),
      rel_X_a722a93eab38baea(&rel_X_a722a93eab38baea) {}

void Stratum_X_cdfd55e9ff53eb8f::run(
    [[maybe_unused]] const std::vector<RamDomain> &args,
    [[maybe_unused]] std::vector<RamDomain> &ret) {
  signalHandler->setMsg(R"_(X(1).
in file aggregation-complex.dl [5:1-5:6])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt,
                      rel_X_a722a93eab38baea->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(1))}};
    rel_X_a722a93eab38baea->insert(
        tuple, READ_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt));
  }();
  signalHandler->setMsg(R"_(X(2).
in file aggregation-complex.dl [5:7-5:12])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt,
                      rel_X_a722a93eab38baea->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(2))}};
    rel_X_a722a93eab38baea->insert(
        tuple, READ_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt));
  }();
  signalHandler->setMsg(R"_(X(10).
in file aggregation-complex.dl [5:13-5:19])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt,
                      rel_X_a722a93eab38baea->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(10))}};
    rel_X_a722a93eab38baea->insert(
        tuple, READ_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt));
  }();
  signalHandler->setMsg(R"_(X(11).
in file aggregation-complex.dl [5:20-5:26])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt,
                      rel_X_a722a93eab38baea->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(11))}};
    rel_X_a722a93eab38baea->insert(
        tuple, READ_OP_CONTEXT(rel_X_a722a93eab38baea_op_ctxt));
  }();
}

} // namespace souffle

namespace souffle {
class Stratum_Y_f020167cd6c49997 {
public:
  Stratum_Y_f020167cd6c49997(SymbolTable &symTable, RecordTable &recordTable,
                             bool &pruneImdtRels, bool &performIO,
                             SignalHandler *&signalHandler,
                             std::atomic<std::size_t> &iter,
                             std::atomic<RamDomain> &ctr,
                             std::string &inputDirectory,
                             std::string &outputDirectory,
                             t_btree_i__0__2__1::Type &rel_Y_9b1b019904fc8f67);
  void run([[maybe_unused]] const std::vector<RamDomain> &args,
           [[maybe_unused]] std::vector<RamDomain> &ret);

private:
  SymbolTable &symTable;
  RecordTable &recordTable;
  bool &pruneImdtRels;
  bool &performIO;
  SignalHandler *&signalHandler;
  std::atomic<std::size_t> &iter;
  std::atomic<RamDomain> &ctr;
  std::string &inputDirectory;
  std::string &outputDirectory;
  t_btree_i__0__2__1::Type *rel_Y_9b1b019904fc8f67;
};
} // namespace souffle
namespace souffle {
Stratum_Y_f020167cd6c49997::Stratum_Y_f020167cd6c49997(
    SymbolTable &symTable, RecordTable &recordTable, bool &pruneImdtRels,
    bool &performIO, SignalHandler *&signalHandler,
    std::atomic<std::size_t> &iter, std::atomic<RamDomain> &ctr,
    std::string &inputDirectory, std::string &outputDirectory,
    t_btree_i__0__2__1::Type &rel_Y_9b1b019904fc8f67)
    : symTable(symTable), recordTable(recordTable),
      pruneImdtRels(pruneImdtRels), performIO(performIO),
      signalHandler(signalHandler), iter(iter), ctr(ctr),
      inputDirectory(inputDirectory), outputDirectory(outputDirectory),
      rel_Y_9b1b019904fc8f67(&rel_Y_9b1b019904fc8f67) {}

void Stratum_Y_f020167cd6c49997::run(
    [[maybe_unused]] const std::vector<RamDomain> &args,
    [[maybe_unused]] std::vector<RamDomain> &ret) {
  signalHandler->setMsg(R"_(Y(1).
in file aggregation-complex.dl [7:1-7:6])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt,
                      rel_Y_9b1b019904fc8f67->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(1))}};
    rel_Y_9b1b019904fc8f67->insert(
        tuple, READ_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt));
  }();
  signalHandler->setMsg(R"_(Y(2).
in file aggregation-complex.dl [7:7-7:12])_");
  [&]() {
    CREATE_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt,
                      rel_Y_9b1b019904fc8f67->createContext());
    Tuple<RamDomain, 1> tuple{{ramBitCast(RamSigned(2))}};
    rel_Y_9b1b019904fc8f67->insert(
        tuple, READ_OP_CONTEXT(rel_Y_9b1b019904fc8f67_op_ctxt));
  }();
}

} // namespace souffle

namespace souffle {
class Sf_aggregation_comple : public SouffleProgram {
public:
  Sf_aggregation_comple();
  ~Sf_aggregation_comple();
  void run();
  void runAll(std::string inputDirectoryArg = "",
              std::string outputDirectoryArg = "", bool performIOArg = true,
              bool pruneImdtRelsArg = true);
  void printAll([[maybe_unused]] std::string outputDirectoryArg = "");
  void loadAll([[maybe_unused]] std::string inputDirectoryArg = "");
  void dumpInputs();
  void dumpOutputs();
  SymbolTable &getSymbolTable();
  RecordTable &getRecordTable();
  void setNumThreads(std::size_t numThreadsValue);
  void executeSubroutine(std::string name, const std::vector<RamDomain> &args,
                         std::vector<RamDomain> &ret);

private:
  void runFunction(std::string inputDirectoryArg,
                   std::string outputDirectoryArg, bool performIOArg,
                   bool pruneImdtRelsArg);
  SymbolTableImpl symTable;
  SpecializedRecordTable<0> recordTable;
  Own<t_btree_i__0__1::Type> rel_X_a722a93eab38baea;
  souffle::RelationWrapper<t_btree_i__0__1::Type>
      wrapper_rel_X_a722a93eab38baea;
  Own<t_btree_i__0__2__1::Type> rel_Y_9b1b019904fc8f67;
  souffle::RelationWrapper<t_btree_i__0__2__1::Type>
      wrapper_rel_Y_9b1b019904fc8f67;
  Own<t_btree_i__0__1::Type> rel_W_6746ec4c1affe72f;
  souffle::RelationWrapper<t_btree_i__0__1::Type>
      wrapper_rel_W_6746ec4c1affe72f;
  Stratum_W_092de3ee4021ad8d stratum_W_96372d4eb55a490e;
  Stratum_X_cdfd55e9ff53eb8f stratum_X_946a79c556ce5737;
  Stratum_Y_f020167cd6c49997 stratum_Y_0bb8b615956cbda8;
  std::string inputDirectory;
  std::string outputDirectory;
  SignalHandler *signalHandler{SignalHandler::instance()};
  std::atomic<RamDomain> ctr{};
  std::atomic<std::size_t> iter{};
};
} // namespace souffle
namespace souffle {
Sf_aggregation_comple::Sf_aggregation_comple()
    : symTable(), recordTable(),
      rel_X_a722a93eab38baea(mk<t_btree_i__0__1::Type>()),
      wrapper_rel_X_a722a93eab38baea(0, *rel_X_a722a93eab38baea, *this, "X",
                                     std::array<const char *, 1>{{"i:number"}},
                                     std::array<const char *, 1>{{"n"}}, 0),
      rel_Y_9b1b019904fc8f67(mk<t_btree_i__0__2__1::Type>()),
      wrapper_rel_Y_9b1b019904fc8f67(1, *rel_Y_9b1b019904fc8f67, *this, "Y",
                                     std::array<const char *, 1>{{"i:number"}},
                                     std::array<const char *, 1>{{"n"}}, 0),
      rel_W_6746ec4c1affe72f(mk<t_btree_i__0__1::Type>()),
      wrapper_rel_W_6746ec4c1affe72f(2, *rel_W_6746ec4c1affe72f, *this, "W",
                                     std::array<const char *, 1>{{"i:number"}},
                                     std::array<const char *, 1>{{"x"}}, 0),
      stratum_W_96372d4eb55a490e(
          symTable, recordTable, pruneImdtRels, performIO, signalHandler, iter,
          ctr, inputDirectory, outputDirectory, *rel_W_6746ec4c1affe72f,
          *rel_X_a722a93eab38baea, *rel_Y_9b1b019904fc8f67),
      stratum_X_946a79c556ce5737(
          symTable, recordTable, pruneImdtRels, performIO, signalHandler, iter,
          ctr, inputDirectory, outputDirectory, *rel_X_a722a93eab38baea),
      stratum_Y_0bb8b615956cbda8(
          symTable, recordTable, pruneImdtRels, performIO, signalHandler, iter,
          ctr, inputDirectory, outputDirectory, *rel_Y_9b1b019904fc8f67) {
  addRelation("X", wrapper_rel_X_a722a93eab38baea, false, false);
  addRelation("Y", wrapper_rel_Y_9b1b019904fc8f67, false, false);
  addRelation("W", wrapper_rel_W_6746ec4c1affe72f, false, true);
}

Sf_aggregation_comple::~Sf_aggregation_comple() {}

void Sf_aggregation_comple::runFunction(std::string inputDirectoryArg,
                                        std::string outputDirectoryArg,
                                        bool performIOArg,
                                        bool pruneImdtRelsArg) {

  this->inputDirectory = std::move(inputDirectoryArg);
  this->outputDirectory = std::move(outputDirectoryArg);
  this->performIO = performIOArg;
  this->pruneImdtRels = pruneImdtRelsArg;

  // set default threads (in embedded mode)
  // if this is not set, and omp is used, the default omp setting of number of
  // cores is used.
#if defined(_OPENMP)
  if (0 < getNumThreads()) {
    omp_set_num_threads(static_cast<int>(getNumThreads()));
  }
#endif

  signalHandler->set();
  // -- query evaluation --
  {
    std::vector<RamDomain> args, ret;
    stratum_X_946a79c556ce5737.run(args, ret);
  }
  {
    std::vector<RamDomain> args, ret;
    stratum_Y_0bb8b615956cbda8.run(args, ret);
  }
  {
    std::vector<RamDomain> args, ret;
    stratum_W_96372d4eb55a490e.run(args, ret);
  }

  // -- relation hint statistics --
  signalHandler->reset();
}

void Sf_aggregation_comple::run() { runFunction("", "", false, false); }

void Sf_aggregation_comple::runAll(std::string inputDirectoryArg,
                                   std::string outputDirectoryArg,
                                   bool performIOArg, bool pruneImdtRelsArg) {
  runFunction(inputDirectoryArg, outputDirectoryArg, performIOArg,
              pruneImdtRelsArg);
}

void Sf_aggregation_comple::printAll(
    [[maybe_unused]] std::string outputDirectoryArg) {
  try {
    std::map<std::string, std::string> directiveMap(
        {{"IO", "file"},
         {"attributeNames", "x"},
         {"auxArity", "0"},
         {"name", "W"},
         {"operation", "output"},
         {"output-dir", "."},
         {"params", "{\"records\": {}, \"relation\": {\"arity\": 1, "
                    "\"params\": [\"x\"]}}"},
         {"types", "{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": "
                   "1, \"types\": [\"i:number\"]}}"}});
    if (!outputDirectoryArg.empty()) {
      directiveMap["output-dir"] = outputDirectoryArg;
    }
    IOSystem::getInstance()
        .getWriter(directiveMap, symTable, recordTable)
        ->writeAll(*rel_W_6746ec4c1affe72f);
  } catch (std::exception &e) {
    std::cerr << e.what();
    exit(1);
  }
}

void Sf_aggregation_comple::loadAll(
    [[maybe_unused]] std::string inputDirectoryArg) {}

void Sf_aggregation_comple::dumpInputs() {}

void Sf_aggregation_comple::dumpOutputs() {
  try {
    std::map<std::string, std::string> rwOperation;
    rwOperation["IO"] = "stdout";
    rwOperation["name"] = "W";
    rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, "
                           "\"types\": [\"i:number\"]}}";
    IOSystem::getInstance()
        .getWriter(rwOperation, symTable, recordTable)
        ->writeAll(*rel_W_6746ec4c1affe72f);
  } catch (std::exception &e) {
    std::cerr << e.what();
    exit(1);
  }
}

SymbolTable &Sf_aggregation_comple::getSymbolTable() { return symTable; }

RecordTable &Sf_aggregation_comple::getRecordTable() { return recordTable; }

void Sf_aggregation_comple::setNumThreads(std::size_t numThreadsValue) {
  SouffleProgram::setNumThreads(numThreadsValue);
  symTable.setNumLanes(getNumThreads());
  recordTable.setNumLanes(getNumThreads());
}

void Sf_aggregation_comple::executeSubroutine(
    std::string name, const std::vector<RamDomain> &args,
    std::vector<RamDomain> &ret) {
  if (name == "W") {
    stratum_W_96372d4eb55a490e.run(args, ret);
    return;
  }
  if (name == "X") {
    stratum_X_946a79c556ce5737.run(args, ret);
    return;
  }
  if (name == "Y") {
    stratum_Y_0bb8b615956cbda8.run(args, ret);
    return;
  }
  fatal(("unknown subroutine " + name).c_str());
}

} // namespace souffle
namespace souffle {
SouffleProgram *newInstance_aggregation_comple() {
  return new Sf_aggregation_comple;
}
SymbolTable *getST_aggregation_comple(SouffleProgram *p) {
  return &reinterpret_cast<Sf_aggregation_comple *>(p)->getSymbolTable();
}
} // namespace souffle

#ifndef __EMBEDDED_SOUFFLE__
#include "souffle/CompiledOptions.h"
int main(int argc, char **argv) {
  try {
    souffle::CmdOptions opt(R"(./aggregation-complex.dl)", R"()", R"()", false,
                            R"()", 1);
    if (!opt.parse(argc, argv))
      return 1;
    souffle::Sf_aggregation_comple obj;
#if defined(_OPENMP)
    obj.setNumThreads(opt.getNumJobs());

#endif
    obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
    return 0;
  } catch (std::exception &e) {
    souffle::SignalHandler::instance()->error(e.what());
  }
}
#endif

namespace souffle {
class factory_Sf_aggregation_comple : souffle::ProgramFactory {
public:
  SouffleProgram *newInstance();
  factory_Sf_aggregation_comple();

private:
};
} // namespace souffle
namespace souffle {
SouffleProgram *factory_Sf_aggregation_comple::newInstance() {
  return new Sf_aggregation_comple();
}

factory_Sf_aggregation_comple::factory_Sf_aggregation_comple()
    : ProgramFactory("aggregation_comple") {}

} // namespace souffle
namespace souffle {

#ifdef __EMBEDDED_SOUFFLE__
extern "C" {
factory_Sf_aggregation_comple __factory_Sf_aggregation_comple_instance;
}
#endif
} // namespace souffle
