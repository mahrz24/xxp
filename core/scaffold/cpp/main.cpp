#include <xxp>
#include <iostream>

int main(int argc, char **argv)
{
  xxp::init(argc, argv);

  XDO_BEGIN;

  XDEC_PARAM(double, hallo_world);
  std::cout << "Answer to the Ultimate Question of Life"
               ", the Universe, and Everything: " 
	    << hallo_world << std::endl;

  XEND;

}
