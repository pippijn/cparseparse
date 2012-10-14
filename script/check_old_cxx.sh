#!/bin/sh

#################################################################
# This will check if we are dealing with the right compiler
#

CXX=${CXX:-g++}

${CXX} -x c++ - > /dev/null 2>&1 <<EOF
#include <vector>
int main()
{
  std::vector<int> x(31);
  for (int y : x)
    ;
}
EOF
