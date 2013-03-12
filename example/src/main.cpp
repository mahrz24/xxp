#include <xxp>
#include <iostream>

struct test
{
  int a;
  int b;
};

namespace xxp
{
  template<>
  struct extract<test>
  {
    static test value(const picojson::value& v)
    {
      test t;
      t.a = extract<int>::value(v.get("a"));
      t.b = extract<int>::value(v.get("b"));
      return t;
    }
  };
}

int main(int argc, char **argv)
{
  xxp::init();

  XDEC_PARAM(double, beta);

  test t;
  XPARAM(t);
  std::cout << "Hello World" << std::endl;

  XDEC_PARAM(std::vector<int>, bla);

  std::cout << "Hello World" << std::endl;
  std::cout << beta << std::endl;
  std::cout << t.a << " " << t.b << std::endl;
  for(int i=0;i<bla.size();i++)
  std::cout << bla[i]  << std::endl;

  XDEBUG(std::cout << "A debug message" << std::endl);
 
}
