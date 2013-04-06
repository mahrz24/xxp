#ifndef _XXP_HEADER_
#define _XXP_HEADER_

#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <fstream>
#include <memory>
#include <zmq.h>
#include <sys/time.h>

#ifdef XXP_EIGEN_SUPPORT
#include <eigen3/Eigen/Eigen>
#endif

#include "picojson.h"

#ifdef XXP_DEBUG
#define XDEBUG(x) x
#else
#define XDEBUG(x)
#endif

#define XDEC_PARAM(t,id) t id=xxp::parameter<t>::value(std::string(#id))
#define XDEC_PARAM_PATH(t,id,p) t \
 id = xxp::parameter<t>::value(std::string(#p))

#define XPARAM(id) id = xxp::parameter<decltype(id)>::value(std::string(#id))
#define XPARAM_PATH(id,p) id = \
 (xxp::parameter<decltype(id)>::value(std::string(#p)))

#define XDO_BEGIN xxp::core::get().execute([&] () {
#define XEND })

enum { max_length = 1024 };

namespace xxp
{
  enum command { DAT = 0, RQF, RQJ, DNE, NOP };

  typedef int data_handle;

  const char tab = '\t';

  enum style { flat, matrix };

  //////////////////////////////////////////////////////////////////////////////
  // Data streams
  //////////////////////////////////////////////////////////////////////////////

  template<typename T>
  struct data_formatter;

  template<typename T>
  struct data_formatter<std::vector<std::vector<T>>>
  {
    static std::string format(std::vector<std::vector<T>> & v, style s = flat)
    {
      std::stringstream out;
      for(std::vector<T>& vv : v)
      {
	for(T& t : vv)
	{
	  out << t;
	  if(t != vv.back())
	    out << tab;
	}
	if(vv != v.back())
	{
	  if(s==flat)
	    out << tab;
	  else
	    out << std::endl;
	}
      }
      return out.str();
    }
  };

  template<typename T>
  struct data_formatter<std::vector<T>>
  {
    static std::string format(std::vector<T> & v, style s)
    {
      std::stringstream out;
      for(typename std::vector<T>::const_iterator t = v.begin();
          t!=v.end();
          ++t)
      {
      	out << *t;
      	if(t != v.end()-1)
      	  out << tab;
      }
      return out.str();
    }
  };

#ifdef XXP_EIGEN_SUPPORT

  template<typename T, int S, int U>
  static std::string format(Eigen::Matrix<T,S,U> & v, style s = flat)
  {
    std::stringstream out;
    for(int i=0; i<v.rows(); i++)
    {
      for(int j=0; j<v.cols(); j++)
      {
	out << v(i,j);
	if(j<v.cols()-1)
	  out << tab;
      }
      if(i<v.rows()-1)
      {
	if(s==flat)
	  out << tab;
	else
	  out << std::endl;
      }
    }
    return out.str();
  };

#endif

  template<typename T>
  std::string format(T t, style s = flat)
  {
    return data_formatter<T>::format(t, s);
  }

  //////////////////////////////////////////////////////////////////////////////
  // Exceptions
  //////////////////////////////////////////////////////////////////////////////

  struct ipc_exception : std::exception
  {
    const char* what() const noexcept
    {
      return "IPC error: server side error, "
	"wrong or no response identifier given";
    }
  };

  //////////////////////////////////////////////////////////////////////////////
  // Action Extraction
  //////////////////////////////////////////////////////////////////////////////

  enum action_type { loop, each, pipe };

  struct action
  {
    action_type t;

    picojson::value* link;

    std::vector<picojson::value> values;
    std::string data_id;
    std::string log_id;
    std::string sink;
    double begin;
    double step;
    double end;

    unsigned int size()
    {
      if(t == loop)
	return ceil((end-begin)/step);
      else if(t == each)
	return values.size();
      else if(t == pipe)
	return 1; // Pipes are on a request only basis
    }
  };

  struct action_extractor
  {
    static void extract(picojson::value* super,
			picojson::value& v,
			std::vector<action>& action_storage)
    {
      if (v.is<picojson::object>())
      {
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
      	  else if(type == "pipe")
      	  {
      	    a.t = pipe;
      	    a.data_id = v.get("data_id").get<std::string>();
      	    a.log_id = v.get("log_id").get<std::string>();
      	    a.sink = v.get("sink").get<std::string>();
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

  //////////////////////////////////////////////////////////////////////////////
  // State (Main Part)
  //////////////////////////////////////////////////////////////////////////////

  struct state
  {
    state() : first_entry(true),
	      zmq_responder(0),
	      zmq_requester(0),
        master_instance(false),
        mpi_mode(false),
	      timing_file(-1)
    {
      zmq_context = zmq_ctx_new();
      gettimeofday(&start_time, NULL);
    };

    ~state()
    {
    };

    picojson::value config;

    std::vector<action> actions;
    std::stringstream sample_buffer;
    std::vector<std::shared_ptr<std::ofstream>> sample_files;
    std::vector<bool> sample_first;

    void * zmq_context;
    void * zmq_responder;
    void * zmq_requester;

    bool first_entry;
    bool master_instance;
    bool mpi_mode;

    data_handle timing_file;

    timeval start_time;
    timeval m_start_time;

    //////////////////////////////////////////////////////////////////////////
    // Initialization

    void init(int argc, char ** argv)
    {
      if(argc < 4)
      {
	std::cerr << "adaptor: wrong number of arguments" << std::endl;
	exit(1);
      }

      if(argv[1][0] == 'm')
      {
	init_with_mpi(argc, argv);
      }
      else
      {
	XDEBUG(std::cout << "adaptor: started" << std::endl);
	std::string ipc_file(argv[2]);
	init_ipc(ipc_file);
	parse_config(argv[3]);
      }
    }

    void init_with_mpi(int argc, char ** argv)
    {
      mpi_mode = true;

      XDEBUG(std::cout << "adaptor: started in mpi mode" << std::endl);
      if(argc != 5)
      {
	std::cerr << "adaptor: wrong number of arguments" << std::endl;
	exit(1);
      }

      std::string ipc_file(argv[2]);

      if(*argv[4] == 'm') // Master process
      {
	master_instance = true;
	std::cout << "adaptor: master process started" << std::endl;

	parse_config(argv[3]);

        zmq_responder = zmq_socket (zmq_context, ZMQ_REP);
	zmq_connect(zmq_responder, ("ipc://" + ipc_file).c_str());

	// Spit all actions out on request
	execute([&] ()
		{
		  // Wait for next request from client
		  wait_for_request();
		  reply_config();
	  });

	wait_for_request();
	reply();

        finalize(0);
      }
      else
      {
	std::cout << "adaptor: worker process started" << std::endl;
	init_ipc(ipc_file);
	parse_config(argv[3]);
      }
    }

    void init_ipc(std::string& ipc_file)
    {
      zmq_requester = zmq_socket(zmq_context, ZMQ_REQ);
      int rc = zmq_connect(zmq_requester, ("ipc://" + ipc_file).c_str());
      XDEBUG(std::cout << "adaptor: ipc setup: " << ipc_file << " "
	     << rc << std::endl);
    }

    std::string get_file_contents(const char *filename)
    {
      std::ifstream in(filename);
      if (in)
      {
	std::string contents;
	in.seekg(0, std::ios::end);
	contents.resize(in.tellg());
	in.seekg(0, std::ios::beg);
	in.read(&contents[0], contents.size());
	in.close();
	return(contents);
      }
      return "";
    }

    void parse_config(const char * config_c)
    {
      parse_raw_config(config_c);

      // Extract actions
      extract_actions();
    }

    void parse_raw_config(const char * config_c)
    {
      std::string err;

      std::string config_str = get_file_contents(config_c);

      picojson::parse(config, config_str.c_str(),
		      config_str.c_str() + config_str.size(), &err);
      if (!err.empty()) {
	std::cerr << "adaptor: json: " << err << std::endl;
      }
    }

    void parse_raw_config_str(const char * config_c)
    {
      std::string err;
      picojson::parse(config, config_c,
		      config_c + strlen(config_c), &err);
      if (!err.empty()) {
	std::cerr << "adaptor: json: " << err << std::endl;
      }
    }


    //////////////////////////////////////////////////////////////////////////
    // Finalization

    void finalize(int exit_code)
    {
      for(auto& i : sample_files)
      {
	i->close();
      }

      if(master_instance)
	zmq_close(zmq_responder);
      else
      {
	// Send a final done
	if(!mpi_mode)
	  send_command(DNE);
	zmq_close(zmq_requester);
      }
      zmq_ctx_destroy(zmq_context);
      std::cout << "adaptor: exiting" << std::endl;
      exit(exit_code);
    }

    //////////////////////////////////////////////////////////////////////////
    // IPC

    void wait_for_request()
    {
      zmq_msg_t request;
      zmq_msg_init(&request);
      zmq_msg_recv(&request, zmq_responder, 0);
      zmq_msg_close(&request);
    }

    void reply()
    {
      zmq_msg_t reply;
      zmq_msg_init_size (&reply, 0);
      zmq_msg_send (&reply, zmq_responder, 0);
      zmq_msg_close (&reply);
    }

    void reply_config()
    {
      //zmq_msg_t reply;
      std::stringstream config_s;
      config_s << config;
      int reply_size = config_s.str().size();
      zmq_send(zmq_responder,
	       config_s.str().c_str(),
	       reply_size,
	       0);
    }

    std::string send_command(command c,
			     const char * arg = NULL)
    {
      switch(c)
      {
      case DAT:
	zmq_send(zmq_requester, "DAT", 3, ZMQ_SNDMORE);
	zmq_send(zmq_requester, arg, strlen(arg), 0);
	break;
      case RQF:
	zmq_send(zmq_requester, "RQF", 3, ZMQ_SNDMORE);
	zmq_send(zmq_requester, arg, strlen(arg), 0);
	break;
      case RQJ:
	zmq_send(zmq_requester, "RQJ", 3, 0);
	break;
      case DNE:
	zmq_send(zmq_requester, "DNE", 3, 0);
	break;
      default:
	break;
      }

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: command: " << c << ": reply error "
		  << errno << std::endl;
      }

      if(reply_size>0)
      {
	std::string reply_str((char*)zmq_msg_data(&reply),reply_size);
	return reply_str;
      }
      return std::string("");
    }

    void setup_pipe(action &a)
    {
      zmq_send(zmq_requester, "PIP", 3, ZMQ_SNDMORE);
      zmq_send(zmq_requester, a.sink.c_str(), a.sink.size(), ZMQ_SNDMORE);
      zmq_send(zmq_requester, a.log_id.c_str(), a.log_id.size(), ZMQ_SNDMORE);
      zmq_send(zmq_requester, a.data_id.c_str(), a.data_id.size(), 0);

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: pipe reply error " << errno << std::endl;
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Action Execution

    void extract_actions()
    {
      // Extract the actions
      action_extractor::extract(nullptr, config, actions);
      if(!mpi_mode)
      {
	// Setup data pipes
	for(action & a : actions)
	  if(a.t == pipe)
	    setup_pipe(a);
      }
    }

    void execute_action(unsigned int i, int s, int& c, std::function<void()> f)
    {
      if(i == actions.size())
      {
	c++;
	std::cout << "adaptor: executing block " << c << "/" << s << std::endl;
	f();
      }
      else
      {
	if(actions[i].t == loop)
	{
	  for(double d=actions[i].begin; d<actions[i].end; d+=actions[i].step)
	  {
	    *(actions[i].link) = picojson::value(d);
	    execute_action(i+1,s, c, f);
	  }
	}
	else if(actions[i].t == each)
	{
	  for(auto v : actions[i].values)
	  {
	    *(actions[i].link) = v;
	    execute_action(i+1, s, c, f);
	  }
	}
	else if(actions[i].t == pipe)
	{
	  execute_action(i+1, s, c, f);
	}
      }
    }

    void execute(std::function<void()> f)
    {
      if(mpi_mode && !master_instance)
      {
	// Setup data pipes
	bool firstJob = true;

	while(true)
	{
	  std::string local_config = send_command(RQJ);

	  if(local_config.empty())
      	    break;

	  if(firstJob)
	  {
	    for(action & a : actions)
	      if(a.t == pipe)
		setup_pipe(a);
	    firstJob = false;
	  }

	  parse_raw_config_str(local_config.c_str());
	  f();

	  send_command(DNE);
	}
      }
      else
      {
	if(actions.size() == 0)
	  f();
	else
	{
	  int action_size = 1;
	  for(auto& a : actions)
	    action_size *= a.size();

	  int cur = 0;

	  execute_action(0, action_size, cur, f);
	}

	if(!mpi_mode)
	{
	  measure_time();
	  store_timing();
	}
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Timing functions

    double time_diff(timeval& t, timeval* res = nullptr)
    {
      timeval end_time;
      long seconds, useconds;
      double duration;

      gettimeofday(&end_time, NULL);
      seconds  = end_time.tv_sec  - t.tv_sec;
      useconds = end_time.tv_usec - t.tv_usec;
      duration = seconds + useconds/1000000.0;

      if(res!=nullptr)
	*res = end_time;

      return duration;
    }

    //////////////////////////////////////////////////////////////////////////
    // Implementations of exposed functions

    bool eof(const char * sink)
    {
      zmq_send(zmq_requester, "EOF", 3, ZMQ_SNDMORE);
      zmq_send(zmq_requester, sink, strlen(sink), 0);

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: eof: reply error " << errno << std::endl;
      }
      return reply_size == 0;
    }

    void block(const char * sink)
    {
      zmq_send(zmq_requester, "BLK", 3, ZMQ_SNDMORE);
      zmq_send(zmq_requester, sink, strlen(sink), 0);

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: block: reply error " << errno << std::endl;
      }

    }

    void unblock(const char * sink)
    {
      zmq_send(zmq_requester, "UBL", 3, ZMQ_SNDMORE);
      zmq_send(zmq_requester, sink, strlen(sink), 0);

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: ublock: reply error " << errno << std::endl;
      }
     }

    template<typename T>
    std::vector<T> request(const char * sink, int lines = 1)
    {
      std::stringstream lines_s;
      lines_s << lines;
      zmq_send(zmq_requester, "RQD", 3, ZMQ_SNDMORE);
      zmq_send(zmq_requester, sink, strlen(sink), ZMQ_SNDMORE);
      zmq_send(zmq_requester, lines_s.str().c_str(), lines_s.str().size(), 0);

      zmq_msg_t reply;
      zmq_msg_init(&reply);
      int reply_size = zmq_msg_recv(&reply, zmq_requester, 0);

      if(reply_size < 0)
      {
	std::cout << "adaptor: request data: reply error " << errno << std::endl;
      }

      std::vector<T> reply_v;

      if(reply_size>0)
      {
	std::string reply_str((char*)zmq_msg_data(&reply),reply_size);
	std::stringstream reply_s(reply_str);
	while(!reply_s.eof())
	{
	  T t;
	  reply_s >> t;
	  reply_v.push_back(t);
	}
      }
      return reply_v;
    }

    void measure_time()
    {
      if(timing_file == -1)
	timing_file = request_file("time");

      if(sample_first[timing_file])
      {
	*sample_files[timing_file] << time_diff(start_time, &m_start_time);
  	sample_first[timing_file] = false;
      }
      else
      {
	*sample_files[timing_file] << tab << time_diff(m_start_time);
      }
    }

    void store_timing()
    {
      store_data(timing_file);
    }

    data_handle request_file(const char * identifier)
    {

      std::string file_path = send_command(RQF, identifier);
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
	std::string data_str(sample_buffer.str());
	sample_buffer.str("");
	sample_buffer.clear();
	first_entry = true;
	send_command(DAT, data_str.c_str());
      }
      else if(h<sample_files.size())
      {
	*sample_files[h] << std::endl;
	sample_first[h] = true;
      }
    }

  };

  //////////////////////////////////////////////////////////////////////////////
  // Parameter Declaration Helper
  //////////////////////////////////////////////////////////////////////////////

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

#ifdef XXP_EIGEN_SUPPORT
  template<typename T>
  struct extract<Eigen::Matrix<T, 1, Eigen::Dynamic>>
  {
    static Eigen::Matrix<T,1,Eigen::Dynamic> value(const picojson::value& v)
    {
      Eigen::Matrix<T,1,Eigen::Dynamic> r(v.get<picojson::array>().size());
      int j;
      for(picojson::array::const_iterator i =
	    v.get<picojson::array>().begin();
	  i != v.get<picojson::array>().end();
	  i++)
      {
	r(j) = extract<T>::value(*i);
	j++;
      }
      return r;
    }
  };

  template<typename T>
  struct extract<Eigen::Matrix<T, Eigen::Dynamic, 1>>
  {
    static Eigen::Matrix<T,Eigen::Dynamic,1> value(const picojson::value& v)
    {
      Eigen::Matrix<T,Eigen::Dynamic,1> r(v.get<picojson::array>().size());
      int j = 0;
      for(picojson::array::const_iterator i =
	    v.get<picojson::array>().begin();
	  i != v.get<picojson::array>().end();
	  i++)
      {
	r(j) = extract<T>::value(*i);
	j++;
      }
      return r;
    }
  };

  template<typename T>
  struct extract<Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>>
  {
    static Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic>
      value(const picojson::value& v)
    {
      Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> r(
	v.get<picojson::array>().size(),
	v.get<picojson::array>().begin()->get<picojson::array>().size());
      r.setZero();

      int j = 0;
      for(picojson::array::const_iterator i =
	    v.get<picojson::array>().begin();
	  i != v.get<picojson::array>().end();
	  i++)
      {
	int l = 0;
	for(picojson::array::const_iterator k =
	      i->get<picojson::array>().begin();
	    k != i->get<picojson::array>().end();
	    k++)
	{
	  r(j,l) = extract<T>::value(*k);
	  l++;
	}
	j++;
      }
      return r;
    }
  };

#endif

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

  //////////////////////////////////////////////////////////////////////////////
  // Exposed functions
  //////////////////////////////////////////////////////////////////////////////

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
      return path<T>::query(core::get().config, idPath, id);
    };
  };

  bool eof(const char * sink)
  {
    return core::get().eof(sink);
  }

  void block(const char * sink)
  {
    core::get().block(sink);
  }

  void unblock(const char * sink)
  {
    core::get().unblock(sink);
  }

  template<typename T>
  std::vector<T> request(const char * sink, int lines = 1)
  {
    return core::get().request<T>(sink, lines);
  }

  data_handle request_file(const char * identifier)
  {
    return core::get().request_file(identifier);
  }

  void measure_time()
  {
    return core::get().measure_time();
  }

  void store_timing()
  {
    return core::get().store_timing();
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

  void finalize(int exit_code)
  {
    core::get().finalize(0);
  }
}

#endif
