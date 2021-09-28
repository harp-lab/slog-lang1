/*
 * The Slog (or simplified) b-tree
 * non-thread-safe, insertion only, b-trees
 *
 * see test.cpp for usage
 *
 * Copyright (C) Thomas Gilray, et al, 2020.
 */

//#include <iostream> //for testing
#pragma once
//#include "../../parallel_RA_inc.h" //for u16, etc
#include "../../compat.h" //for u16, etc

#define DEFAULT_BT_WIDTH 32

template<class K, class V, u16 W>
union sbtree_ptr;

template<class K, class V, u16 W>
class insert_msg
{
private:
  struct split_msg
  {
    sbtree_ptr<K,V,W> left; sbtree_ptr<K,V,W> right; K pk; V pv;
    split_msg(const K k, const V v, sbtree_ptr<K,V,W> left, sbtree_ptr<K,V,W> right)
      : left(left), right(right), pk(k), pv(v) {}
  };
  split_msg* sm;
  
public: 
  inline insert_msg() : sm(0) {}
  inline insert_msg(const K k, const V v, const sbtree_ptr<K,V,W> left, const sbtree_ptr<K,V,W> right)
    : sm(new split_msg(k,v,left,right)) { /*std::cout << "[[*]]" << std::endl;*/  }
  inline ~insert_msg() {}
  inline void clear() { if (sm) delete sm; }
  inline operator bool() const { return sm != 0; }
  inline K key() const { return sm->pk; }
  inline V val() const { return sm->pv; }
  inline sbtree_ptr<K,V,W> left() const { return sm->left; }
  inline sbtree_ptr<K,V,W> right() const { return sm->right; }
};

template<class K, class V, u16 W>
class sbtree_node
{
private:
  u16 len;
  K keys[W];
  V values[W];
  sbtree_ptr<K,V,W> children[W+1];
  
  inline u16 find_pos_short_rng(const K key, const u16 start, const u16 end)
  {
    for (u16 i = start; i < end; ++i)
      if (key <= keys[i])
        return i;
    return end;
  }

  inline u16 find_pos_rng(const K key, const u16 start, const u16 end)
  {
    if (end-start > 6)
    {
      const u16 p = start+(end-start)/2;
      if (key <= keys[p])
        return find_pos_rng(key, start, p);
      else
        return find_pos_rng(key, p, end);
    }
    else
      return find_pos_short_rng(key, start, end);
  }
  
  inline u16 find_pos(const K key)
  {
    return find_pos_rng(key, 0, len);
  }
  
  inline void insert_collision(insert_msg<K,V,W>& r, const u16 p)
  {
    /* insert new binary-tree message r into *this */
    for (u16 i = len; i > p; --i)
    {
      keys[i] = keys[i-1];
      values[i] = values[i-1];
      children[i+1] = children[i];
    }
    keys[p] = r.key();
    values[p] = r.val();
    children[p] = r.left();
    children[p+1] = r.right();
    ++len;
    
    r.clear();
  }
  
public:
  inline sbtree_node()
    : len(0), keys{0}, values{0}
  {
  }
  
  inline sbtree_node(const K k, const V v, sbtree_ptr<K,V,W> left, sbtree_ptr<K,V,W> right)
    : len(1), keys{0}, values{0}
  {
    keys[0] = k;
    values[0] = v;
    children[0] = left;
    children[1] = right;
  }

  inline sbtree_node(const insert_msg<K,V,W> r)
    : len(1), keys{0}, values{0}
  {
    keys[0] = r.key();
    values[0] = r.val();
    children[0] = r.left();
    children[1] = r.right();
  }

  inline ~sbtree_node()
  {
  }

  inline V* find(const K k)
  {
    u16 p = find_pos(k);
    if (p < len && keys[p] == k)
      return &(values[p]);
    return children[p].find(k);
  }

  inline void clear()
  {
    for (u16 i = 0; i <= len; ++i)
      children[i].clear();
  }

  inline u16 size()
  {
    return len;
  }
  
  inline sbtree_ptr<K,V,W> childat(u16 i)
  {
    return children[i];
  }
  
  inline sbtree_ptr<K,V,W> firstchild()
  {
    return children[0];
  }

  inline insert_msg<K,V,W> insert(const K k, const V v)
  { 
    u16 p = find_pos(k);
    if (p < len && keys[p] == k)
      values[p] = v;
    else
    {
      insert_msg<K,V,W> r = children[p].insert(k,v);
      if (r)
        insert_collision(r,p);
    }

    /* If at max size, then split and propagate another binary tree up  */
    if (len >= W)
    {
      u16 pv = W/2;
      sbtree_node<K,V,W>* right = new sbtree_node<K,V,W>();
      for (u16 i = pv+1; i < len; ++i)
      {
        right->keys[i-pv-1] = keys[i];
        right->values[i-pv-1] = values[i];
        right->children[i-pv-1] = children[i];
      }
      right->children[len-pv-1] = children[this->len];
      right->len = len-pv-1;
      len = pv;
      return insert_msg<K,V,W>(keys[pv], values[pv], sbtree_ptr<K,V,W>(this), sbtree_ptr<K,V,W>(right));
    }
    else
      return insert_msg<K,V,W>();
  }
};

template<class K, class V, u16 W>
class sbtree_leaf
{
private:
  u16 len;
  K keys[W];
  V values[W];

  inline u16 find_pos_short_rng(const K key, const u16 start, const u16 end)
  {
    for (u16 i = start; i < end; ++i)
      if (key <= keys[i])
        return i;
    return end;
  }

  inline u16 find_pos_rng(const K key, const u16 start, const u16 end)
  {
    if (end-start > 6)
    {
      const u16 p = start+(end-start)/2;
      if (key <= keys[p])
        return find_pos_rng(key, start, p);
      else
        return find_pos_rng(key, p, end);
    }
    else
      return find_pos_short_rng(key, start, end);
  }
  
  inline u16 find_pos(const K key)
  {
    return find_pos_rng(key, 0, len);
  }
  
public:
  inline sbtree_leaf()
    : len(0), keys{0}, values{0}
  {
  }

  inline sbtree_leaf(const K k, const V v)
    : len(1), keys{0}, values{0}
  {
    keys[0] = k;
    values[0] = v;
  }

  inline ~sbtree_leaf() 
  {
  }

  inline V* find(const K k)
  {
    u16 p = find_pos(k);
    if (p < len && keys[p] == k)
      return &values[p];
    return 0;
  }

  inline u16 size()
  {
    return len;
  }

  inline sbtree_ptr<K,V,W> firstchild()
  {
    return sbtree_ptr<K,V,W>();
  }

  inline K keyat(u16 i)
  {
    return keys[i];
  }

  inline V valat(u16 i)
  {
    return values[i];
  }
  
  inline insert_msg<K,V,W> insert(const K k, const V v)
  {
    const u16 p = find_pos(k);

    if (keys[p] == k)
    {
      values[p] = v;
      return insert_msg<K,V,W>();
    }
    
    for (u16 r = len; p < r; --r)
    {
      keys[r] = keys[r-1];
      values[r] = values[r-1];
    }
    keys[p] = k;
    values[p] = v;
    ++len;

    /* split oversized leaf */
    if (len >= W)
    {
      sbtree_leaf<K,V,W>* right = new sbtree_leaf<K,V,W>();
      const u16 pv = len/2;
      for (u16 i = pv; i < len; ++i)
      {
        right->keys[i-pv] = this->keys[i];
        right->values[i-pv] = this->values[i];
      }
      right->len = len-pv;
      len = pv;
      return insert_msg<K,V,W>(keys[pv],values[pv],sbtree_ptr<K,V,W>(this),sbtree_ptr<K,V,W>(right));
    }
    else
      return insert_msg<K,V,W>();
  }
};

template<class K, class V, u16 W = DEFAULT_BT_WIDTH>
class sbtree_root
{
private:
  sbtree_ptr<K,V,W> root_ptr;

public:
  sbtree_root() : root_ptr()
  { 
  }

  ~sbtree_root()  
  { 
  }

  inline V* find(const K k)
  {
    return root_ptr.find(k);
  }

  inline void clear()
  {
    root_ptr.clear();
  } 

  void insert(const K k, const V v)
  {
    if (root_ptr)
    {
      insert_msg<K,V,W> r = root_ptr.insert(k,v);
      if (r)
      {
        root_ptr = new sbtree_node<K,V,W>(r);
        r.clear();
      }
    }
    else
    {
      root_ptr = new sbtree_leaf<K,V,W>(k,v);
    }
  }
  
  class iter
  {
  private:
    u16 len;
    u16 leaf_pos;
    u16 pathi[16];
    sbtree_ptr<K,V,W> path[16];
  public:
    inline iter()
      : len(0), leaf_pos(0), pathi{0}
    {
    }
    
    inline ~iter()
    {
    }
    
    inline void push_inode(u16 i, sbtree_ptr<K,V,W> ptr)
    {
      pathi[len] = i;
      path[len] = ptr;
      ++len;
    }

    inline K key() 
    {
      return path[len-1].forceleaf()->keyat(leaf_pos);
    }

    inline V val() 
    {
      return path[len-1].forceleaf()->valat(leaf_pos);
    }

    inline operator bool() const
    { 
      return len > 0;
    }
  
    inline void next()
    {
      if (path[len-1].forceleaf()->size() <= leaf_pos+1)
      {
        --len;
        while (len > 0)
        {
          sbtree_ptr<K,V,W> parent = path[len-1];
          u16 childpos = pathi[len];
          ++childpos;
          /* there are size()+1 children, so >= */
          if (parent.forcenode()->size() >= childpos)
          {
            push_inode(childpos, parent.forcenode()->childat(childpos));
            while (path[len-1])
            { /* build a path to leftmost leaf */
              push_inode(0,path[len-1].firstchild());
            }
            --len;
            leaf_pos = 0;
            return;
          }
          else
            --len;
        }
      }
      else
        ++leaf_pos;
    }
  };

  inline iter begin()  
  {
    iter it = iter();
    sbtree_ptr<K,V,W> tail = root_ptr;
    while (tail)
    {
      /* build a path to leftmost leaf */
      it.push_inode(0, tail);
      tail = tail.firstchild();
    }
    return it;
  }
};

template<class K, class V, u16 W>
union sbtree_ptr
{
private:
  sbtree_leaf<K,V,W>* leaf;
  sbtree_node<K,V,W>* node;
  u64 bm;

public:
  inline sbtree_ptr() : bm(0)
  {
  }
  
  inline sbtree_ptr(sbtree_leaf<K,V,W>* other)
    : leaf(other)
  {
    bm = bm | 2;
  }
  
  inline sbtree_ptr(sbtree_node<K,V,W>* other)
    : node(other)
  {
  }

  inline ~sbtree_ptr()
  {
  }

  inline V* find(const K k)
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      V* r = leaf->find(k);
      bm = bm | 2;
      return r;
    }
    else if (bm)
      return node->find(k);
    else
    {
      std::cout << "error: find(..) into empty sbtree_ptr\n";
      *((int*)((void*)0)) = 1;
      return 0;
    }
  }

  inline void clear()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_leaf<K,V,W>* _leaf = leaf;
      delete _leaf;
    }
    else
    {
      node->clear();
      delete node;
    }
  } 

  inline sbtree_leaf<K,V,W>* forceleaf()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_leaf<K,V,W>* _leaf = leaf;
      bm = bm | 2;
      return _leaf;
    }
    else
    {
      std::cout << "error: forceleaf() called on non-leaf (" << bm << ")" << std::endl;
      return 0;
    }
  }

  inline sbtree_node<K,V,W>* forcenode()
  {
    if (!(bm & 2))
    {
      return node;
    }
    else
    {
      std::cout << "error: forcenode() called on non-node (" << bm << ")" << std::endl;
      return 0;
    }
  }

  inline sbtree_ptr<K,V,W> firstchild()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_ptr<K,V,W> fc = leaf->firstchild();
      bm = bm | 2;
      return fc;
    }
    else if (bm)
      return node->firstchild();
    else
    {
      std::cout << "error: firstchild of empty sbtree_ptr\n";
      return sbtree_ptr<K,V,W>();
    }
  }

  inline insert_msg<K,V,W> insert(const K k, const V v)
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      insert_msg<K,V,W> r = leaf->insert(k,v);
      bm = bm | 2;
      return r;
    }
    else if (bm)
      return node->insert(k,v);
    else
    {
      std::cout << "error: insert into empty sbtree_ptr\n";
      *((int*)((void*)0)) = 1;
      return insert_msg<K,V,W>();
    }
  }
  
  void operator =(sbtree_leaf<K,V,W>* optr)
  {
    leaf = optr;
    bm = bm | 2;
  }

  void operator =(sbtree_node<K,V,W>* optr)
  {
    node = optr;
  }

  bool operator ==(const sbtree_ptr<K,V,W>& other)
  {
    return bm == other.bm; 
  }

  operator bool()
  {
    return bm != 0;
  }
};


/* V=void specialization */


template<class K, u16 W>
class insert_msg<K,void,W>
{
private:
  struct split_msg
  {
    sbtree_ptr<K,void,W> left; sbtree_ptr<K,void,W> right; K pk;
    split_msg(const K k, sbtree_ptr<K,void,W> left, sbtree_ptr<K,void,W> right)
      : left(left), right(right), pk(k) {}
  };
  split_msg* sm;
  
public: 
  inline insert_msg() : sm(0) {}
  inline insert_msg(const K k, const sbtree_ptr<K,void,W> left, const sbtree_ptr<K,void,W> right)
    : sm(new split_msg(k,left,right)) { /*std::cout << "[[*]]" << std::endl;*/  }
  inline ~insert_msg() {}
  inline void clear() { if (sm) delete sm; }
  inline operator bool() const { return sm != 0; }
  inline K key() const { return sm->pk; }
  inline sbtree_ptr<K,void,W> left() const { return sm->left; }
  inline sbtree_ptr<K,void,W> right() const { return sm->right; }
};

template<class K, u16 W>
class sbtree_node<K,void,W>
{
private:
  u16 len;
  K keys[W];
  sbtree_ptr<K,void,W> children[W+1];
  
  inline u16 find_pos_short_rng(const K key, const u16 start, const u16 end)
  {
    for (u16 i = start; i < end; ++i)
      if (key <= keys[i])
        return i;
    return end;
  }

  inline u16 find_pos_rng(const K key, const u16 start, const u16 end)
  {
    if (end-start > 6)
    {
      const u16 p = start+(end-start)/2;
      if (key <= keys[p])
        return find_pos_rng(key, start, p);
      else
        return find_pos_rng(key, p, end);
    }
    else
      return find_pos_short_rng(key, start, end);
  }
  
  inline u16 find_pos(const K key)
  {
    return find_pos_rng(key, 0, len);
  }
  
  inline void insert_collision(insert_msg<K,void,W>& r, const u16 p)
  {
    /* insert new binary-tree message r into *this */
    for (u16 i = len; i > p; --i)
    {
      keys[i] = keys[i-1];
      children[i+1] = children[i];
    }
    keys[p] = r.key();
    children[p] = r.left();
    children[p+1] = r.right();
    ++len;
    
    r.clear();
  }
  
public:
  inline sbtree_node()
    : len(0), keys{0}
  {
  }
  
  inline sbtree_node(const K k, sbtree_ptr<K,void,W> left, sbtree_ptr<K,void,W> right)
    : len(1), keys{0}
  {
    keys[0] = k;
    children[0] = left;
    children[1] = right;
  }

  inline sbtree_node(const insert_msg<K,void,W> r)
    : len(1), keys{0}
  {
    keys[0] = r.key();
    children[0] = r.left();
    children[1] = r.right();
  }

  inline ~sbtree_node()
  {
  }

  inline bool find(const K k)
  {
    u16 p = find_pos(k);
    if (p < len && keys[p] == k)
      return true;
    return children[p].find(k);
  }

  inline void clear()
  {
    for (u16 i = 0; i <= len; ++i)
      children[i].clear();
  }

  inline u16 size()
  {
    return len;
  }
  
  inline sbtree_ptr<K,void,W> childat(u16 i)
  {
    return children[i];
  }
  
  inline sbtree_ptr<K,void,W> firstchild()
  {
    return children[0];
  }

  inline insert_msg<K,void,W> insert(const K k)
  { 
    u16 p = find_pos(k);
    if (!(p < len && keys[p] == k))
    {
      insert_msg<K,void,W> r = children[p].insert(k);
      if (r)
        insert_collision(r,p);
    }

    /* If at max size, then split and propagate another binary tree up  */
    if (len >= W)
    {
      u16 pv = W/2;
      sbtree_node<K,void,W>* right = new sbtree_node<K,void,W>();
      for (u16 i = pv+1; i < len; ++i)
      {
        right->keys[i-pv-1] = keys[i];
        right->children[i-pv-1] = children[i];
      }
      right->children[len-pv-1] = children[this->len];
      right->len = len-pv-1;
      len = pv;
      return insert_msg<K,void,W>(keys[pv], sbtree_ptr<K,void,W>(this), sbtree_ptr<K,void,W>(right));
    }
    else
      return insert_msg<K,void,W>();
  }
};

template<class K, u16 W>
class sbtree_leaf<K,void,W>
{
private:
  u16 len;
  K keys[W];

  inline u16 find_pos_short_rng(const K key, const u16 start, const u16 end)
  {
    for (u16 i = start; i < end; ++i)
      if (key <= keys[i])
        return i;
    return end;
  }

  inline u16 find_pos_rng(const K key, const u16 start, const u16 end)
  {
    if (end-start > 6)
    {
      const u16 p = start+(end-start)/2;
      if (key <= keys[p])
        return find_pos_rng(key, start, p);
      else
        return find_pos_rng(key, p, end);
    }
    else
      return find_pos_short_rng(key, start, end);
  }
  
  inline u16 find_pos(const K key)
  {
    return find_pos_rng(key, 0, len);
  }
  
public:
  inline sbtree_leaf()
    : len(0), keys{0}
  {
  }

  inline sbtree_leaf(const K k)
    : len(1), keys{0}
  {
    keys[0] = k;
  }

  inline ~sbtree_leaf() 
  {
  }

  inline bool find(const K k)
  {
    u16 p = find_pos(k);
    if (p < len && keys[p] == k)
      return true;
    return false;
  }
  
  inline u16 size()
  {
    return len;
  }

  inline sbtree_ptr<K,void,W> firstchild()
  {
    return sbtree_ptr<K,void,W>();
  }

  inline K keyat(u16 i)
  {
    return keys[i];
  }
  
  inline insert_msg<K,void,W> insert(const K k)
  {
    const u16 p = find_pos(k);

    if (keys[p] == k)
    {
      return insert_msg<K,void,W>();
    }
    
    for (u16 r = len; p < r; --r)
    {
      keys[r] = keys[r-1];
    }
    keys[p] = k;
    ++len;

    /* split oversized leaf */
    if (len >= W)
    {
      sbtree_leaf<K,void,W>* right = new sbtree_leaf<K,void,W>();
      const u16 pv = len/2;
      for (u16 i = pv; i < len; ++i)
      {
        right->keys[i-pv] = this->keys[i];
      }
      right->len = len-pv;
      len = pv;
      return insert_msg<K,void,W>(keys[pv],sbtree_ptr<K,void,W>(this),sbtree_ptr<K,void,W>(right));
    }
    else
      return insert_msg<K,void,W>();
  }
};

template<class K, u16 W>
class sbtree_root<K,void,W>
{
private:
  sbtree_ptr<K,void,W> root_ptr;

public:
  sbtree_root() : root_ptr()
  { 
  }

  ~sbtree_root()  
  { 
  }
  
  inline bool find(const K k)
  {
    return root_ptr.find(k);
  }
  
  inline void clear()
  {
    root_ptr.clear();
  }
  
  void insert(const K k)
  {
    if (root_ptr)
    {
      insert_msg<K,void,W> r = root_ptr.insert(k);
      if (r)
      {
        root_ptr = new sbtree_node<K,void,W>(r);
        r.clear();
      }
    }
    else
    {
      root_ptr = new sbtree_leaf<K,void,W>(k);
    }
  }

  class iter
  {
  private:
    u16 len;
    u16 leaf_pos;
    u16 pathi[16];
    sbtree_ptr<K,void,W> path[16];
  public:
    inline iter()
      : len(0), leaf_pos(0), pathi{0}
    {
    }

    inline ~iter()
    {
    }

    inline void push_inode(u16 i, sbtree_ptr<K,void,W> ptr)
    {
      pathi[len] = i;
      path[len] = ptr;
      ++len;
    }

    inline K key() 
    {
      return path[len-1].forceleaf()->keyat(leaf_pos);
    }

    inline operator bool() const
    { 
      return len > 0;
    }

    inline void next()
    {
      if (path[len-1].forceleaf()->size() <= leaf_pos+1)
      {
        --len;
        while (len > 0)
        {
          sbtree_ptr<K,void,W> parent = path[len-1];
          u16 childpos = pathi[len];
          ++childpos;
          /* there are size()+1 children, so >= */
          if (parent.forcenode()->size() >= childpos)
          {
            push_inode(childpos, parent.forcenode()->childat(childpos));
            while (path[len-1])
            { /* build a path to leftmost leaf */
              push_inode(0,path[len-1].firstchild());
            }
            --len;
            leaf_pos = 0;
            return;
          }
          else
            --len;
        }
      }
      else
        ++leaf_pos;
    }
  };
  
  inline iter begin()  
  {
    iter it = iter();
    sbtree_ptr<K,void,W> tail = root_ptr;
    while (tail)
    {
      /* build a path to leftmost leaf */
      it.push_inode(0, tail);
      tail = tail.firstchild();
    }
    return it;
  }
};

template<class K, u16 W>
union sbtree_ptr<K,void,W>
{
private:
  sbtree_leaf<K,void,W>* leaf;
  sbtree_node<K,void,W>* node;
  u64 bm;

public:
  inline sbtree_ptr() : bm(0)
  {
  }
  
  inline sbtree_ptr(sbtree_leaf<K,void,W>* other)
    : leaf(other)
  {
    bm = bm | 2;
  }
  
  inline sbtree_ptr(sbtree_node<K,void,W>* other)
    : node(other)
  {
  }

  inline ~sbtree_ptr()
  {
  }

  inline bool find(const K k)
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      bool r = leaf->find(k);
      bm = bm | 2;
      return r;
    }
    else if (bm)
      return node->find(k);
    else
    {
      std::cout << "error: find(..) into empty sbtree_ptr\n";
      *((int*)((void*)0)) = 1;
      return false;
    }
  }

  inline void clear()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_leaf<K,void,W>* _leaf = leaf;
      delete _leaf;
    }
    else
    {
      node->clear();
      delete node;
    }
  }

  inline sbtree_leaf<K,void,W>* forceleaf()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_leaf<K,void,W>* _leaf = leaf;
      bm = bm | 2;
      return _leaf;
    }
    else
    {
      std::cout << "error: forceleaf() called on non-leaf (" << bm << ")" << std::endl;
      return 0;
    }
  }

  inline sbtree_node<K,void,W>* forcenode()
  {
    if (!(bm & 2))
    {
      return node;
    }
    else
    {
      std::cout << "error: forcenode() called on non-node (" << bm << ")" << std::endl;
      return 0;
    }
  }

  inline sbtree_ptr<K,void,W> firstchild()
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      sbtree_ptr<K,void,W> fc = leaf->firstchild();
      bm = bm | 2;
      return fc;
    }
    else if (bm)
      return node->firstchild();
    else
    {
      std::cout << "error: firstchild of empty sbtree_ptr\n";
      return sbtree_ptr<K,void,W>();
    }
  }

  inline insert_msg<K,void,W> insert(const K k)
  {
    if (bm & 2)
    {
      bm = bm & 0xfffffffffffffffc;
      insert_msg<K,void,W> r = leaf->insert(k);
      bm = bm | 2;
      return r;
    }
    else if (bm)
      return node->insert(k);
    else
    {
      std::cout << "error: insert into empty sbtree_ptr\n";
      *((int*)((void*)0)) = 1;
      return insert_msg<K,void,W>();
    }
  }
  
  void operator =(sbtree_leaf<K,void,W>* optr)
  {
    leaf = optr;
    bm = bm | 2;
  }

  void operator =(sbtree_node<K,void,W>* optr)
  {
    node = optr;
  }

  bool operator ==(const sbtree_ptr<K,void,W>& other)
  {
    return bm == other.bm; 
  }

  operator bool()
  {
    return bm != 0;
  }
};


