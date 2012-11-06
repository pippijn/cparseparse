struct foo
: public __conditional_type<
      __is_integer<_Value>,
      __numeric_traits_integer<_Value>,
      int
    >
{
};
