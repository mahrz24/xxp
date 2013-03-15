#ifndef _PROCESS_H_
#define _PROCESS_H_

#include <boost/filesystem.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <sys/wait.h>
#include <cstdlib>
#include <unistd.h>
#include <csignal>
#include <sstream>

struct child_process
{
  child_process(char * path, int r, std::string config, bool master) : rank(r) 
  {
    std::stringstream ipc_file_s;
    ipc_file_s << "/tmp/mpibridge." << rank 
    << "." << boost::uuids::random_generator()();
    ipc_file = ipc_file_s.str();

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
      
      sleep(1);

      execl(path, p.filename().c_str(), 
	    ipc_file.c_str(), 
	    config.c_str(), 
	    mode.c_str(), (char*)NULL);
      
      kill(getppid(), SIGUSR1);
      exit(EXIT_FAILURE);
    }
    else if (pid < pid_t(0))
    {
      std::cerr << "mpibridge: error: fork failed" << std::endl;
      exit(EXIT_FAILURE);
    }
  }

  bool is_running()
  {
    int status;
    waitpid(pid, &status, WNOHANG);
    return !(WIFEXITED(status) || WIFSIGNALED(status) || WIFSTOPPED(status));
  }

  std::string ipc_file;
  int rank;
  pid_t pid;
};

#endif /* _PROCESS_H_ */
