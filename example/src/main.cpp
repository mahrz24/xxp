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
  xxp::init(argc, argv);
/*
  XDEC_PARAM(double, other);
  std::cout << "other: " << other << std::endl;

  xxp::data() << other;
  xxp::data() << "Other";
  xxp::store_data();

  XDO_BEGIN;
  XDEC_PARAM(double, test);
  std::cout << "test: " << test << std::endl;

  xxp::data() << test;
  xxp::data() << "Hallo";
  xxp::store_data();

  xxp::data() << 1.3245;
  xxp::data() << "Nanu";
  xxp::store_data();

  XEND;
*/

  XDO_BEGIN;
  XDEC_PARAM(double, alpha);
  XDEC_PARAM(double, beta);
  std::cout << "alpha: " << alpha << " beta: " << beta << std::endl;
  test t;
  XPARAM(t);
  std::cout << "t.a: " << t.a << " t.b: " << t.b << std::endl;
  XEND;

/*
  XDEC_PARAM(double, beta);

  test t;
  XPARAM(t);
  std::cout << "Hello World" << std::endl;


  std::cout << "Hello World" << std::endl;
  std::cout << beta << std::endl;
  std::cout << t.a << " " << t.b << std::endl;

  XDEBUG(std::cout << "A debug message" << std::endl);
*/


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
  std::cout << "Ending" << std::endl;
}
