#ifndef _XXP_HEADER_
#define _XXP_HEADER_

#include <string>
#include <vector>
#include <sstream>
#include <iostream>

#include <boost/asio.hpp>
using boost::asio::local::stream_protocol;

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

enum { max_length = 1024 };

namespace xxp
{
  enum Command { DAT, RQF };
  enum Response { ACK, STR, ERR };

  struct ipc_exception : std::exception 
  {
    const char* what() const noexcept 
    {
      return "IPC error: server side error, wrong or no response identifier given";
    }
  };

  class state
  {
  public:
    state() : first_entry(true) {};
    ~state() 
    {
      s.close();
    };
    picojson::value v;
    stream_protocol::iostream s;
    std::vector<std::string> current_sample;
    std::stringstream sample_buffer;
    bool first_entry;

    void send_command(Command c, std::string &arg, Response &r, std::string &rArg)
    {
      try
      {
	switch(c)
	{
	case DAT:
	  s << "DAT";
	  break;
	case RQF:
	  s << "RQF";
	  break;
	}

	s << arg << std::endl;
	std::string reply;
	std::getline(s,reply);
	XDEBUG(std::cout << "adaptor: reply from xxp: " << reply << std::endl);

	if(reply.size()<3)
	  throw ipc_exception();

	rArg = reply.substr(3);
	std::string responseId = reply.substr(0,3).c_str();

	if(responseId == "ACK")
	{
	  r = ACK;
	  return;
	}

	if(responseId == "STR")
	{
	  r = STR;
	  return;
	}

	throw ipc_exception();
      }
      catch (std::exception& e)
      {
	std::cerr << "Exception: " << e.what() << std::endl;
	exit(4);
      }
    }

    std::stringstream& data()
    {
      if(!first_entry)
      {
	current_sample.push_back(sample_buffer.str());
	sample_buffer.str("");
	sample_buffer.clear();
      }
      first_entry = false;
      return sample_buffer;
    }
    
    void store_data()
    {
      current_sample.push_back(sample_buffer.str());
      sample_buffer.str("");
      sample_buffer.clear();
      first_entry = true;
      std::stringstream data;
      for(std::vector<std::string>::iterator i = current_sample.begin();
	  i != current_sample.end(); i++)
      {
	data << *i;
	if(i != current_sample.end()-1)
	  data << "\t";
      }
      current_sample.clear();
      Response r;
      std::string data_str(data.str());
      std::string dummy;
      send_command(DAT, data_str , r, dummy);
    }
  };

  namespace core
  {
    inline state& get()
    {
      static state global_state;
      return global_state;
    }
  }


  std::stringstream& data()
  {
    return core::get().data();
  }
    
  void store_data()
  {
    core::get().store_data();
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
	    std::cerr << "adaptor: error json path " << fullPath
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
	    std::cerr << "adaptor: error json path " << fullPath
		      << " does not exist (aerr)" << std::endl;
	    exit(1);
	  }
	  path.erase(path.begin());
	  return query(v.get(i), path, fullPath);
	}
	else
	{
	  std::cerr << "adaptor: error json path " << fullPath
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
    std::string socket_file;

    // We get the configuration via std in
    std::cin >> core::get().v;
    std::string err = picojson::get_last_error();
    if (!err.empty()) {
      std::cerr << "adaptor: json: " << err << std::endl;
    }

    // Once the configuration is parsed create a connection
    // via the supplied socket name
    std::cin >> socket_file;

    try
    {
      boost::asio::io_service io_service;
      core::get().s.connect(stream_protocol::endpoint(socket_file));
    }
    catch (std::exception& e)
    {
      std::cerr << "Exception: " << e.what() << std::endl;
      exit(4);
    }

  }
}

#endif
