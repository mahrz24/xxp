#ifndef _XXP_HEADER_
#define _XXP_HEADER_

#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <fstream>

#include <boost/asio.hpp>
using boost::asio::local::stream_protocol;

#include "picojson.h"

#ifdef XXP_DEBUG
#define XDEBUG(x) x
#else
#define XDEBUG(x)
#endif

#define XDEC_PARAM(t,id) t id(xxp::parameter<t>::value(std::string(#id)))
#define XDEC_PARAM_PATH(t,id,p) t \
 id(xxp::parameter<t>::value(std::string(#p)))

#define XPARAM(id) id = xxp::parameter<decltype(id)>::value(std::string(#id))
#define XPARAM_PATH(id,p) id = \
 (xxp::parameter<decltype(id)>::value(std::string(#p)))

#define XDO_BEGIN xxp::core::get().execute([&] () {
#define XEND })

enum { max_length = 1024 };

namespace xxp
{
  enum command { DAT, RQF };
  enum response { ACK, STR, ERR };

  typedef int data_handle;

  const char tab = '\t';

  struct ipc_exception : std::exception 
  {
    const char* what() const noexcept 
    {
      return "IPC error: server side error, "
	"wrong or no response identifier given";
    }
  };

  enum action_type { loop, each };

  struct action
  {
    action_type t;
    
    picojson::value* link;

    std::vector<picojson::value> values;
    double begin;
    double step;
    double end;

  };

  struct action_extractor
  {
    static void extract(picojson::value* super, 
			picojson::value& v, 
			std::vector<action>& action_storage)
    { 
      if (v.is<picojson::object>())
      {
	std::cout << "Looking at object" << std::endl;
	if(v.contains("action") && super != nullptr)
	{
	  action a;
	  a.link = &v;
	  std::string type = v.get("action").get<std::string>();
	  if(type == "loop")
	  {
	    a.t = loop;
	    a.begin = v.get("begin").get<double>();
	    a.step = v.get("step").get<double>();
	    a.end = v.get("end").get<double>();
	  }
	  else if(type == "each")
	  {
	    a.t = each;
	    for(const auto& d : v.get("values").get<picojson::array>())
	      a.values.push_back(d);
	  }
	  action_storage.push_back(a);
	}
	else
	{
	  for(picojson::object::value_type& i : v.get<picojson::object>())
	  {
	    action_extractor::extract(&v, i.second, action_storage);
	  }
	}
      }
      else if(v.is<picojson::array>())
      {
	for(picojson::value& i : v.get<picojson::array>())
	{
	  action_extractor::extract(&v, i, action_storage);
	}
      }
    }
  };

  struct state
  {
    state() : first_entry(true) {};
    ~state() 
    {
      for(auto& i : sample_files)
      {
	i->close();
      }
      s.close();
    };

    picojson::value v;
    std::vector<action> actions;

    stream_protocol::iostream s;
    std::stringstream sample_buffer;
    std::vector<std::shared_ptr<std::ofstream>> sample_files;
    std::vector<bool> sample_first;

    bool first_entry;

    void init(int argc, char ** argv)
    {
      XDEBUG(std::cout << "adaptor: started" << std::endl);
      if(argc != 3)
      {
	std::cerr << "adaptor: wrong number of arguments" << std::endl;
	exit(1);
      }

      std::string socket_file(argv[1]);
      setup_ipc(socket_file);
      parse_config(argv[2]);
    }

    void init_with_mpi(int argc, char ** argv)
    {
      XDEBUG(std::cout << "adaptor: started" << std::endl);
      if(argc != 4)
      {
	std::cerr << "adaptor: wrong number of arguments" << std::endl;
	exit(1);
      }

      std::string socket_file(argv[1]);
      setup_ipc(socket_file);

      if(*argv[3] = 'm') // Master process
      {
	parse_config(argv[2]);

	// Spit all actions out on request
	execute([&] () {
	    std::string next;
	    std::getline(s, next);
	    s << v << std::endl;
	  });
	
	// Close the stream when finished
	s.close();
      }
      else
      {
      }
    }

    void parse_config(char * config)
    {
      std::string err;
      picojson::parse(v, config, config + strlen(config), &err);
      if (!err.empty()) {
	std::cerr << "adaptor: json: " << err << std::endl;
      }

      // Extract actions
      extract_actions();
    }

    void setup_ipc(std::string& socket_file)
    {
       // Setup socket for IPC
      try
      {
	boost::asio::io_service io_service;
	s.connect(stream_protocol::endpoint(socket_file));
      }
      catch (std::exception& e)
      {
	std::cerr << "Exception: " << e.what() << std::endl;
	exit(4);
      }
    }

    void send_command(command c, 
		      std::string &arg, 
		      response &r, 
		      std::string &response_arg)
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
	XDEBUG(std::cout << "adaptor: reply from xxp: " 
	       << reply << std::endl);

	if(reply.size()<3)
	  throw ipc_exception();

	response_arg = reply.substr(3);
	std::string response_id = reply.substr(0,3).c_str();

	if(response_id == "ACK")
	{
	  r = ACK;
	  return;
	}
	if(response_id == "STR")
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

    data_handle request_file(std::string& identifier)
    {
      response r;
      std::string file_path;
      send_command(RQF, identifier , r, file_path);
      if(r!=STR)
      {
	std::cerr << "Requested file path not returned" << std::endl;
	exit(1);
      }
      sample_files.push_back(
	std::make_shared<std::ofstream>(file_path.c_str()));
      sample_first.push_back(true);
      return sample_first.size()-1;
    }

    std::ostream& data(data_handle h = -1)
    {
      if(h==-1)
      {
	if(!first_entry)
	  sample_buffer << tab;
	first_entry = false;
	return dynamic_cast<std::ostream&>(sample_buffer);
      }
      else if(h<sample_files.size())
      {
	if(!sample_first[h])
	  *sample_files[h] << tab;
	sample_first[h] = false;
	return dynamic_cast<std::ostream&>(*sample_files[h]);
      }
    }
    
    void store_data(data_handle h = -1)
    {
      if(h==-1)
      {
	response r;
	std::string dummy;
	std::string data_str(sample_buffer.str());
	sample_buffer.str("");
	sample_buffer.clear();
	first_entry = true;
	send_command(DAT, data_str , r, dummy);
      }
      else if(h<sample_files.size())
      {
	*sample_files[h] << std::endl;
	sample_first[h] = true;
      }
    }

    void extract_actions()
    {
      action_extractor::extract(nullptr, v, actions);
    }

    void execute(std::function<void()> f)
    {
      if(actions.size() == 0)
     	f();
      else
	execute_action(0,f);
    }

    void execute_action(unsigned int i, std::function<void()> f)
    {
      if(i == actions.size())
	f();
      else
      {
	if(actions[i].t == loop)
	{
	  for(double d=actions[i].begin; d<actions[i].end; d+=actions[i].step)
	  {
	    *(actions[i].link) = picojson::value(d);
	    execute_action(i+1,f);
	  }
	}
	else if(actions[i].t == each)
	{
	  for(auto v : actions[i].values)
	  {
	    *(actions[i].link) = v;
	    execute_action(i+1,f);
	  }
	}
      }
    }

  };

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

  namespace core
  {
    inline state& get()
    {
      static state global_state;
      return global_state;
    }
  }

  template<typename T>
  struct parameter
  {
    static T value(const std::string&& id)
    {
      std::vector<std::string> idPath = split(id, '.');
      return path<T>::query(core::get().v, idPath, id);
    };
  };

  data_handle request_file(std::string& identifier)
  {
    return core::get().request_file(identifier);
  }
  
  std::ostream& data(data_handle h = -1)
  {
    return core::get().data(h);
  }
    
  void store_data(data_handle h = -1)
  {
    core::get().store_data(h);
  }

  void init_with_mpi(int argc, char **argv)
  { 
    core::get().init_with_mpi(argc, argv);    
  }

  void init(int argc, char **argv)
  {
    core::get().init(argc, argv);
  }
}

#endif
