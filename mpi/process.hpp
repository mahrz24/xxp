#ifndef _PROCESS_H_
#define _PROCESS_H_

#include <boost/asio.hpp>
#include <boost/filesystem/path.hpp>

using boost::asio::local::stream_protocol;

struct child_process
{
  child_process(char * path, int rank, std::string config, bool master) 
  {
    std::stringstream socket_name;
    socket_name << "/tmp/mpibridge." << rank;

    pid_t pid;
    pid = fork();
    
    if (pid == pid_t(0))
    {
      // Execute the program
      boost::filesystem::path p(path);
      std::string mode("w");
      if(master)
	mode = "m";

      execl(path, p.filename().c_str(), 
	    socket_name.str().c_str(), 
	    config.c_str(), 
	    mode.c_str(), (char*)NULL);
      

      kill(getppid(), SIGUSR1);
      exit(EXIT_FAILURE);
    }
    else if (pid > pid_t(0))
    {
      std::cout << "Server started" << std::endl;

      unlink(socket_name.str().c_str()); // Remove previous binding.
      stream_protocol::endpoint ep(socket_name.str());
      stream_protocol::acceptor acceptor(io_service, ep);
      acceptor.accept(*stream.rdbuf());
      std::cout << "Connection established" << std::endl;
    }
    else
    {
      std::cerr << "Error: fork failed" << std::endl;
      exit(EXIT_FAILURE);
    }

  }

  void close_connection()
  {

  }

  boost::asio::io_service io_service;
  stream_protocol::iostream stream;

};

#endif /* _PROCESS_H_ */
