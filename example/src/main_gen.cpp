#include <xxp>

int main(int argc, char *argv[])
{
  xxp::init(argc, argv);

  XDO_BEGIN;

  XDEC_PARAM(int, data);

  xxp::data() << data;
  xxp::store_data();

  XEND;

  return 0;
}
