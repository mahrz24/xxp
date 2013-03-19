#define XXP_EIGEN_SUPPORT

#include <xxp>
#include <iostream>
#include <unistd.h>

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

  xxp::data_handle alt_logs = xxp::request_file("alternative");
  xxp::data(alt_logs) << "Hallo";
  xxp::data(alt_logs) << 1.2344;
  xxp::store_data(alt_logs);

  std::vector<std::vector<int>> x{{1,2},{3,4}};

  Eigen::VectorXd v(4);
  v.setRandom();

  Eigen::RowVectorXd w(4);
  w.setRandom();

  Eigen::MatrixXd z(4,4);
  z.setRandom();

  xxp::data(alt_logs) << xxp::format(v);
  xxp::store_data(alt_logs);
  xxp::data(alt_logs) << xxp::format(w);
  xxp::store_data(alt_logs);
  xxp::data(alt_logs) << xxp::format(z);
  xxp::store_data(alt_logs);
  xxp::data(alt_logs) << 1.123;
  xxp::store_data(alt_logs);

  XDO_BEGIN;

  xxp::measure_time();

  XDEC_PARAM(Eigen::VectorXd,vec);
  XDEC_PARAM(Eigen::MatrixXd,mat);
  xxp::data() << xxp::format(vec);
  xxp::store_data();
  xxp::data() << xxp::format(mat, xxp::matrix);
  xxp::store_data();
  sleep(1);
  XDEC_PARAM(double, alpha);
  XDEC_PARAM(double, beta);

  test t;
  XPARAM(t);

  //sleep(1);

  xxp::data() << alpha;
  xxp::data() << beta;
  xxp::data() << "Hallo";
  xxp::store_data();

  xxp::measure_time();
  for(int i=0;i<1000;i++)
    alpha *= 1.3;
  xxp::measure_time();
  xxp::store_timing();

  XEND;

}
