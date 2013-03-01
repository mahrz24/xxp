#ifndef _XXP_HEADER_
#define _XXP_HEADER_

#include <string>
#include <vector>
#include <sstream>
#include <iostream>

#include "picojson.h"

#ifdef XXP_DEBUG
#define XDEBUG(x) x
#else
#define XDEBUG(x)
#endif

#define XDEC_PARAM(t,id) t id(xxp::parameter<t>::value(std::string(#id)))
#define XDEC_PARAM_PATH(t,id,p) t id(xxp::parameter<t>::value(std::string(#p)))

#ifdef XXP_WITH_CPP11
#define XPARAM(id) id = xxp::parameter<decltype(id)>::value(std::string(#id))
#define XPARAM_PATH(id,p) id = (xxp::parameter<decltype(id)>::value(std::string(#p)))

#endif


namespace xxp
{
  class state
  {
  public:
    state() {};
    ~state() {};
    picojson::value v;
  };

  namespace core
  {
    inline state& get()
    {
      static state global_state;
      return global_state;
    }
  }

  std::vector<std::string>& split(const std::string &s,
				  char delim,
				  std::vector<std::string> &elems)
  {
    std::stringstream ss(s);
    std::string item;
    while(std::getline(ss, item, delim)) {
      elems.push_back(item);
    }
    return elems;
  }

  std::vector<std::string> split(const std::string &s, char delim)
  {
    std::vector<std::string> elems;
    return split(s, delim, elems);
  }

  template<typename T>
  struct extract
  {
    static T value(const picojson::value& v)
    {
      return v.get<T>();
    }
  };

  template<>
  struct extract<int>
  {
    static int value(const picojson::value& v)
    {
      return int(v.get<double>());
    }
  };

  template<typename T>
  struct extract<std::vector<T>>
  {
    static std::vector<T> value(const picojson::value& v)
    {
      std::vector<T> r;
      for(picojson::array::const_iterator i = 
	    v.get<picojson::array>().begin();
	  i != v.get<picojson::array>().end();
	  i++)
      {
	r.push_back(extract<T>::value(*i));
      }
      return r;
    }
  };

  template<typename T>
  struct path
  {
    static T query(const picojson::value& v,
		   std::vector<std::string> path,
		   const std::string& fullPath)
    {
      if(path.size() == 0)
      {
	return extract<T>::value(v);
      }
      else
      {
	if (v.is<picojson::object>())
	{
	  if(!v.contains(path[0]))
	  {
	    std::cout << "adaptor: error json path " << fullPath
		      << " does not exist (oerr)" << std::endl;
	    exit(1);
	  }
	  std::string p = path[0];
	  path.erase(path.begin());
	  return query(v.get(p), path, fullPath);
	}
	else if(v.is<picojson::array>())
	{
	  int i = atoi(path[0].c_str());
	  if(!v.contains(i))
	  {
	    std::cout << "adaptor: error json path " << fullPath
		      << " does not exist (aerr)" << std::endl;
	    exit(1);
	  }
	  path.erase(path.begin());
	  return query(v.get(i), path, fullPath);
	}
	else
	{
	  std::cout << "adaptor: error json path " << fullPath
		    << " does not exist (verr)" << std::endl;
	  exit(1);
	}
      }
    }
  };

  template<typename T>
  struct parameter
  {
#ifdef XXP_WITH_CPP11
    static T value(const std::string&& id)
#else
    static T value(const std::string id)
#endif
    {
      std::vector<std::string> idPath = split(id, '.');
      return path<T>::query(core::get().v, idPath, id);
    };
  };

  void init()
  {
    XDEBUG(std::cout << "adaptor: started" << std::endl);
    // We get the configuration via std in
    std::cin >> core::get().v;
    std::string err = picojson::get_last_error();
    if (!err.empty()) {
      std::cout << "adaptor: json: " << err << std::endl;
    }
  }
}

#endif
