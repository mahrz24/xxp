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
 
  xxp::data() << 1.245;
  xxp::data() << "Hallo";
  xxp::store_data();

  xxp::data() << 1.3245;
  xxp::data() << "Nanu";
  xxp::store_data();

  xxp::data_handle alt_logs = xxp::request_file("alternative");
  xxp::data(alt_logs) << "Hallo";
  xxp::data(alt_logs) << 1.2344;
  xxp::store_data(alt_logs);

  xxp::data(alt_logs) << "Hallo Blabla" << xxp::tab << 1.234;
  xxp::data(alt_logs) << 1.123;
  xxp::store_data(alt_logs);
}
