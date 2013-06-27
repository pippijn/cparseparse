template<template<typename> class T>
struct A
{
  typedef typename T<int>::type type;
};

template<typename T>
struct B
{
  typedef typename A<T::template inner>::type type;
};

struct C
{
  template<typename T>
  struct inner
  {
    typedef T type;
  };
};

typedef B<C>::type type;
