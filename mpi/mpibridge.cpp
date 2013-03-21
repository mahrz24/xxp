#ifndef _MPIBRIDGE_H_
#define _MPIBRIDGE_H_

#include <zmq.h>
#include <mpi.h>
#include <glob.h>
#include <fstream>
#include <map>
#include <queue>
#include "process.hpp"

enum tag {
  die_tag = 42,
  job_tag,
  done_tag,
  cmd_tag,
  blk_tag,
  resp_tag,
};

enum cmd {
  pip = 1,
  rqd,
  blk,
  ubl,
  eof
};

struct data_pipe
{
  data_pipe()
  {
  }

  data_pipe(const data_pipe& other) : data_dir(other.data_dir),
				      data_files(other.data_files),
				      owner(other.owner)
  {
  }

  data_pipe(const char * logid, const char * dataid, const char * logdir)
  {
    std::stringstream data_glob;
    data_glob << logdir << "/../../" << logid << "/data/" << dataid << "*";
    data_files = glob(data_glob.str());
  }

  std::string data_dir;
  std::vector<std::string> data_files;
  std::ifstream handle;
  std::queue<int> owner;

  std::vector<std::string> glob(const std::string& pat)
  {
    glob_t glob_result;
    ::glob(pat.c_str(),GLOB_TILDE,NULL,&glob_result);
    std::vector<std::string> ret;
    for(unsigned int i=0;i<glob_result.gl_pathc;++i){
      ret.push_back(std::string(glob_result.gl_pathv[i]));
    }
    globfree(&glob_result);
    return ret;
  }

  void open()
  {
    if(data_files.size() > 0)
    {
      std::string file = data_files.back();
      data_files.pop_back();
      handle.open(file.c_str());
    }
  }

  bool is_eof()
  {
    if(data_files.size() > 0)
      return false;

    int pos = handle.tellg();
    std::string line;
    std::getline(handle, line);

    if(handle.eof() && data_files.size() == 0)
      return true;

    handle.seekg(pos);

    if(!handle.is_open())
      return true;

    return false;
  }

  std::string read_lines(int n)
  {
    if(handle.eof())
    {
      handle.close();
      open();
    }
 
    if(handle.is_open())
    {
      std::stringstream lines_s;
      for(int i=0;i<n;i++)
      {
	std::string line;
	std::getline(handle, line);
	lines_s << line;

	if(i!=n-1)
	  lines_s << std::endl;
      }

      if(handle.eof())
	return read_lines(n);

      return lines_s.str();
    }
    return "";
  }
};

struct master_process
{
  void * zmq_context;
  void * zmq_requester;
  int processes;
  int active;
  bool no_more_jobs;
  std::map<std::string, data_pipe> pipes;

  master_process(char * master, char * global_config, char * logdir) : 
    active(0), 
    no_more_jobs(false)
  {
    MPI_Status status;

    // Get some info about the MPI environment
    MPI_Comm_size(MPI_COMM_WORLD, &processes);

    // Start the master process instance
    child_process cp(master,0,global_config,true);

    // Setup IPC with master process instance
    zmq_context = zmq_ctx_new();
    zmq_requester = zmq_socket(zmq_context, ZMQ_REQ);
    zmq_bind(zmq_requester, ("ipc://" + cp.ipc_file).c_str());

    // Request jobs for each process
    std::cout << "mpibridge: started with " << processes << " processes" 
	      << std::endl;
    for (int p=1; p<processes; ++p) 
    {
      if(push_job(p, cp))
	active++;
    }

    // While stuff is to be done
    while(active)
    {
      int cmd;
      // Receive a command or a work finished message
      MPI_Recv(&cmd, 1, MPI_INT, 
	       MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

      if (status.MPI_TAG == done_tag)
      {
	if(!push_job(status.MPI_SOURCE, cp))
	  active--;
      }
      else if(status.MPI_TAG == cmd_tag)
      {
	if(cmd == pip)
	{
	  int sink_size;
	  MPI_Recv(&sink_size, 1, MPI_INT, 
		 status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  char * sink_c = new char[sink_size+1];
	  MPI_Recv(sink_c, sink_size, MPI_CHAR, 
		   status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  sink_c[sink_size] = 0;

	  int logid_size;
	  MPI_Recv(&logid_size, 1, MPI_INT, 
		 status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  char * logid_c = new char[logid_size+1];
	  MPI_Recv(logid_c, logid_size, MPI_CHAR, 
		   status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  logid_c[logid_size] = 0;

	  int dataid_size;
	  MPI_Recv(&dataid_size, 1, MPI_INT, 
		 status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  char * dataid_c = new char[dataid_size+1];
	  MPI_Recv(dataid_c, dataid_size, MPI_CHAR, 
		   status.MPI_SOURCE, cmd_tag, MPI_COMM_WORLD, &status);
	  dataid_c[dataid_size] = 0;

	  std::string sink(sink_c);

	  if(pipes.find(sink) == pipes.end())
	  {
	    data_pipe p(logid_c, dataid_c, logdir);
	    pipes.insert(std::pair<std::string, data_pipe>(sink,p));
	    pipes[sink].open();
	  }

	  delete logid_c;
	  delete dataid_c;
	  delete sink_c;
	} 
	else if(cmd == rqd)
	{

	  int p = status.MPI_SOURCE;
	  int sink_size;
	  MPI_Recv(&sink_size, 1, MPI_INT, 
		 p, cmd_tag, MPI_COMM_WORLD, &status);
	  char * sink_c = new char[sink_size+1];
	  MPI_Recv(sink_c, sink_size, MPI_CHAR, 
		   p, cmd_tag, MPI_COMM_WORLD, &status);
	  sink_c[sink_size] = 0;

	  int n;
	  MPI_Recv(&n, 1, MPI_INT, 
		 p, cmd_tag, MPI_COMM_WORLD, &status);

	  std::string sink(sink_c);

	  if(pipes.find(sink) != pipes.end())
	  {	    
	    std::string data = pipes[sink].read_lines(n);
	    int data_size = data.size();
	    MPI_Send(&data_size, 1, MPI_INT, 
		     p, cmd_tag, MPI_COMM_WORLD);
	    MPI_Send((char*)data.c_str(), data_size, MPI_CHAR, 
		     p, cmd_tag, MPI_COMM_WORLD);
	  }
	  else
	  {
	    std::cerr << "mpibridge: error: unknown pipe" << std::endl;
	  }

	  delete sink_c;
	} 
	else if(cmd == eof)
	{
	  int p = status.MPI_SOURCE;
	  int sink_size;
	  MPI_Recv(&sink_size, 1, MPI_INT, 
		 p, cmd_tag, MPI_COMM_WORLD, &status);
	  char * sink_c = new char[sink_size+1];
	  MPI_Recv(sink_c, sink_size, MPI_CHAR, 
		   p, cmd_tag, MPI_COMM_WORLD, &status);
	  sink_c[sink_size] = 0;

	  std::string sink(sink_c);

	  if(pipes.find(sink) != pipes.end())
	  {
	    int data_eof = pipes[sink].is_eof();

	    MPI_Send(&data_eof, 1, MPI_INT, 
		     p, cmd_tag, MPI_COMM_WORLD);

	  }
	  else
	  {
	    std::cerr << "mpibridge: error: unknown pipe" << std::endl;
	  }

	  delete sink_c;
	} 
	else if(cmd == blk)
	{
	  int p = status.MPI_SOURCE;
	  int sink_size;
	  MPI_Recv(&sink_size, 1, MPI_INT, 
		 p, cmd_tag, MPI_COMM_WORLD, &status);
	  char * sink_c = new char[sink_size+1];
	  MPI_Recv(sink_c, sink_size, MPI_CHAR, 
		   p, cmd_tag, MPI_COMM_WORLD, &status);
	  sink_c[sink_size] = 0;

	  std::string sink(sink_c);

	  std::map<std::string, data_pipe>::iterator pipe = pipes.find(sink);
	  if(pipe != pipes.end())
	  {
	    if(pipe->second.owner.empty())
	      MPI_Send(0,0, MPI_INT, p, blk_tag, MPI_COMM_WORLD); 
	    pipe->second.owner.push(p);
	  }
	  else
	  {
	    std::cerr << "mpibridge: error: unknown pipe" << std::endl;
	  }

	  delete sink_c;
	} 
	else if(cmd == ubl)
	{
	  int p = status.MPI_SOURCE;
	  int sink_size;
	  MPI_Recv(&sink_size, 1, MPI_INT, 
		 p, cmd_tag, MPI_COMM_WORLD, &status);
	  char * sink_c = new char[sink_size+1];
	  MPI_Recv(sink_c, sink_size, MPI_CHAR, 
		   p, cmd_tag, MPI_COMM_WORLD, &status);
	  sink_c[sink_size] = 0;

	  std::string sink(sink_c);

	  std::map<std::string, data_pipe>::iterator pipe = pipes.find(sink);
	  if(pipe != pipes.end())
	  {
	    if(pipe->second.owner.front() == p)
	    {
	      pipe->second.owner.pop();
	      if(!pipe->second.owner.empty())
		MPI_Send(0,0, MPI_INT, pipe->second.owner.front() , blk_tag, MPI_COMM_WORLD); 
	    }
	  }
	  else
	  {
	    std::cerr << "mpibridge: error: unknown pipe" << std::endl;
	  }

	  delete sink_c;
	} 
	else 
	{
	  std::cerr << "mpibridge: error: unknown command" << std::endl;
	  break;
	}
      }
      else
      {
	std::cerr << "mpibridge: error: expected done or cmd tag" << std::endl;
      }

    }

    for(int p=0;p<processes;p++)
    {
      MPI_Send(0,0, MPI_INT, 
	     p, die_tag, MPI_COMM_WORLD);
    }

    zmq_close(zmq_requester);
    zmq_ctx_destroy(zmq_context);
  }

  bool push_job(int rank, child_process& cp)
  {
    if(no_more_jobs)
    {
      return false;
    }
    request();
    std::string job;

    zmq_msg_t reply;
    zmq_msg_init(&reply);
    int job_size = zmq_msg_recv(&reply, zmq_requester, 0);

    if(job_size<=0)
    {
      std::cout << "mpibridge: no more jobs" << std::endl;
      no_more_jobs = true;
      return false;
    }

    std::cout << "mpibridge: sending job (0->" << rank << ")" << std::endl;

    // Send out job configurations
    MPI_Send(&job_size, 1, MPI_INT, 
	     rank, job_tag, MPI_COMM_WORLD);
    MPI_Send(zmq_msg_data(&reply), job_size, MPI_CHAR, 
	     rank, job_tag, MPI_COMM_WORLD);
    
    zmq_msg_close(&reply);

    return true;
  }

  void request()
  {
    zmq_msg_t request;
    zmq_msg_init_size (&request, 0);
    zmq_msg_send (&request, zmq_requester, 0);
    zmq_msg_close (&request);
  }
};

struct worker_process
{
  void * zmq_context;
  void * zmq_responder;
  int rank;
  std::string log_dir;
  std::vector<std::string> data_filenames;

  worker_process(char * worker, char * global_config, char * l) : log_dir(l)
  {
    MPI_Status status;
    int job_size;
    char * job_c;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    child_process cp(worker, rank, global_config, false);

    // Setup IPC with worker process instance
    zmq_context = zmq_ctx_new();
    zmq_responder = zmq_socket(zmq_context, ZMQ_REP);
    zmq_bind(zmq_responder, ("ipc://" + cp.ipc_file).c_str());

    std::cout << "mpibridge: started on " << rank 
	      << std::endl;

    std::string main_data_filename = add_data_file("main");
    std::ofstream main_data_file(main_data_filename.c_str());

    while(true)
    {
      // Receive a job
      MPI_Recv(&job_size, 1, MPI_INT, 
	       0, MPI_ANY_TAG , MPI_COMM_WORLD, &status);

      if (status.MPI_TAG == die_tag) 
      {
	// A last RQJ comes in
	std::string cmd  = wait_for_cmd();
	// Empty reply to finalize worker instance
	reply();
	zmq_close(zmq_responder);
	zmq_ctx_destroy(zmq_context);
	
	// Close the main data file
	main_data_file.close();

	return;
      }

      job_c = new char[job_size+1];
      MPI_Recv(job_c, job_size, MPI_CHAR, 
	       0, job_tag, MPI_COMM_WORLD, &status);
      job_c[job_size] = 0;

      std::cout << "mpibridge: received job (" << rank << "<-0) " 
		<< std::endl;
  
      // Receive command until worker instance is done
      while(true)
      {
	std::string cmd = wait_for_cmd();
	// Should start with job request
	if(cmd == "RQJ")
	{
	  reply_string(job_c);
	}
	else if(cmd == "DNE")
	{
	  reply();
	  break;
	}
	else if(cmd == "DAT")
	{
	  main_data_file << receive_argument() << std::endl;
	  reply();
	}
	else if(cmd == "PIP")
	{
	  std::string sink = receive_argument();
	  std::string logid = receive_argument();
	  std::string dataid = receive_argument();
	  int mpi_cmd = pip;
	  int sink_size = sink.size();
	  int logid_size = logid.size();
	  int dataid_size = dataid.size();

	  MPI_Send(&mpi_cmd, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&sink_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)sink.c_str(), sink_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&logid_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)logid.c_str(), logid_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&dataid_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)dataid.c_str(), dataid_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  reply();
	}
	else if(cmd == "EOF")
	{
	  std::string sink = receive_argument();
	  int mpi_cmd = eof;
	  int sink_size = sink.size();

	  MPI_Send(&mpi_cmd, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&sink_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)sink.c_str(), sink_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  
	  int pipe_eof;
	  MPI_Recv(&pipe_eof, 1, MPI_INT, 
	       0, cmd_tag , MPI_COMM_WORLD, &status);

	  if(pipe_eof)
	    reply();
	  else
	    reply_string("1");
	}
	else if(cmd == "BLK")
	{
	  std::string sink = receive_argument();
	  int mpi_cmd = blk;
	  int sink_size = sink.size();

	  MPI_Send(&mpi_cmd, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&sink_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)sink.c_str(), sink_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  
	  MPI_Recv(0, 0, MPI_INT, 
	       0, blk_tag , MPI_COMM_WORLD, &status);

	  reply();
	}
	else if(cmd == "UBL")
	{
	  std::string sink = receive_argument();
	  int mpi_cmd = ubl;
	  int sink_size = sink.size();

	  MPI_Send(&mpi_cmd, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&sink_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)sink.c_str(), sink_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  
	  reply();
	}
	else if(cmd == "RQD")
	{
	  std::string sink = receive_argument();
	  std::string nstr = receive_argument();
	  int n = atoi(nstr.c_str());

	  int mpi_cmd = rqd;
	  int sink_size = sink.size();

	  MPI_Send(&mpi_cmd, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&sink_size, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);
	  MPI_Send((char*)sink.c_str(), sink_size, MPI_CHAR, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  MPI_Send(&n, 1, MPI_INT, 
	     0, cmd_tag, MPI_COMM_WORLD);

	  int data_size;
	  MPI_Recv(&data_size, 1, MPI_INT, 
	       0, cmd_tag , MPI_COMM_WORLD, &status);

	  char * data_c = new char[data_size+1];
	  MPI_Recv(data_c, data_size, MPI_CHAR, 
		   0, cmd_tag, MPI_COMM_WORLD, &status);
	  data_c[data_size] = 0;

	  reply_string(data_c);

	  delete data_c;
	}
	else if(cmd == "RQF")
	{
	  std::string tag = receive_argument();
	  std::string file = add_data_file(tag.c_str());
	  reply_string(file.c_str());
	}
	else
	{
	  std::cerr << "mpibridge: unknown command received: " << cmd
		    << std::endl;
	  reply();
	}
      }
      
      delete[] job_c;

      MPI_Send(0,0, MPI_INT, 0, done_tag, MPI_COMM_WORLD);
    }
  }

  std::string add_data_file(const char * tag)
  {
    std::stringstream data_fn_s;
    data_fn_s << log_dir << "/" 
	      << tag << "." << rank << "." 
	      << data_filenames.size() << ".dat";
    data_filenames.push_back(data_fn_s.str());
    return data_fn_s.str();
  }

  std::string receive_argument()
  {
    zmq_msg_t request;
    zmq_msg_init(&request);
    int arg_size = zmq_msg_recv(&request, zmq_responder, 0);

    std::string arg((char*)zmq_msg_data(&request),arg_size);
    zmq_msg_close(&request);

    return arg;
  }


  std::string wait_for_cmd()
  {
    zmq_msg_t request;
    zmq_msg_init(&request);
    int cmd_size = zmq_msg_recv(&request, zmq_responder, 0);

    if(cmd_size != 3)
      std::cerr << "mpibridge: command size wrong " << cmd_size << std::endl;

    std::string cmd((char*)zmq_msg_data(&request),3);
    zmq_msg_close(&request);

    return cmd;
  }

  void reply()
  {
    zmq_msg_t reply;
    zmq_msg_init_size (&reply, 0);
    zmq_msg_send (&reply, zmq_responder, 0);
    zmq_msg_close (&reply);
  }

  void reply_string(const char * str)
  {
    zmq_msg_t reply;
    std::stringstream str_s;
    str_s << str;
    int reply_size = str_s.str().size();
    zmq_send(zmq_responder, 
	     str_s.str().c_str(),
	     reply_size, 
	     0);
  }
};

int main(int argc, char *argv[])
{
  int cur_rank;
  MPI_Status status;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &cur_rank);

  if(argc<4)
  {
    std::cerr << "mpibridge: too few arguments" << std::endl;
    exit(1);
  }
  // Am I the headnode (?) 
  if(cur_rank == 0)
  {
    master_process m(argv[1], argv[2], argv[3]);
  }
  else
  {
    // Simply start child processes 
    worker_process w(argv[1], argv[2], argv[3]);
  }
  std::cout << "mpibridge: finalizing on " << cur_rank << std::endl;
  MPI_Finalize ();
  std::cout << "mpibridge: exiting on " << cur_rank << std::endl;
  return 0;
}


#endif /* _MPIBRIDGE_H_ */
