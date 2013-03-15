#ifndef _PROCESS_H_
#define _PROCESS_H_

#include <boost/asio.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <sys/wait.h>
#include <cstdlib>
#include <unistd.h>
#include <csignal>
#include <pthread.h>

using boost::asio::local::stream_protocol;

void * server(void *p);

struct child_process
{
  child_process(char * path, int r, std::string config, bool master) : rank(r) 
  {
    std::stringstream socket_name_s;
    socket_name_s << "/tmp/mpibridge." << rank 
    << "." << boost::uuids::random_generator()();
    
    socket_name = socket_name_s.str();

    pthread_t accept_thread;


    if(pthread_create(&accept_thread, NULL, server, this)) 
    {
      std::cerr << "mpibridge: error: creating thread failed" << std::endl;
    }

    // Forking is ok here even with multi-threading.
    // There are no shared resources, threading is 
    // simply needed to avoid 
    pid = fork();
    
    if (pid == pid_t(0))
    {
      // Execute the program
      boost::filesystem::path p(path);
      std::string mode("w");
      if(master)
	mode = "m";

      execl(path, p.filename().c_str(), 
	    socket_name_s.str().c_str(), 
	    config.c_str(), 
	    mode.c_str(), (char*)NULL);
      
      kill(getppid(), SIGUSR1);
      exit(EXIT_FAILURE);
    }
    else if (pid > pid_t(0))
    {
      if(pthread_join(accept_thread, NULL)) 
      {
	std::cerr << "mpibridge: error: joining thread failed" << std::endl;
      }
    }
    else
    {
      std::cerr << "mpibridge: error: fork failed" << std::endl;
      exit(EXIT_FAILURE);
    }

  }

  void close_connection()
  {
    stream.close();
  }

  bool is_running()
  {
    int status;
    return waitpid(pid, &status, WNOHANG) > 0;
  }

  boost::asio::io_service io_service;
  stream_protocol::iostream stream;
  std::string socket_name;
  int rank;
  pid_t pid;
};

void * server(void *p)
{
  child_process * process = static_cast<child_process*>(p);

  std::cout << "mpibridge: server started (=" << process->rank << ")" 
	    << std::endl;
  
  unlink(process->socket_name.c_str());
  stream_protocol::endpoint ep(process->socket_name);
  stream_protocol::acceptor acceptor(process->io_service, ep);
  acceptor.accept(*(process->stream.rdbuf()));
  std::cout << "mpibridge: connection established (=" << process->rank << ")" 
	    << std::endl;

  return NULL;
}

#endif /* _PROCESS_H_ */
