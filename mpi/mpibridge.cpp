#ifndef _MPIBRIDGE_H_
#define _MPIBRIDGE_H_

#include <zmq.h>
#include <mpi.h>
#include <fstream>
#include "process.hpp"

enum tag {
  die_tag = 42,
  job_tag,
  done_tag,
  cmd_tag,
  resp_tag,
};

struct master_process
{
  void * zmq_context;
  void * zmq_requester;
  int processes;
  int active;
  bool no_more_jobs;

  master_process(char * master, char * global_config) : 
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
      int cmd_size;
      // Receive a command or a work finished message
      MPI_Recv(&cmd_size, 1, MPI_INT, 
	       MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

      if (status.MPI_TAG == done_tag)
      {
	if(!push_job(status.MPI_SOURCE, cp))
	  active--;
      }
      else
      {
	std::cerr << "mpibridge: error: expected done tag" << std::endl;
      }

    }

    // We're done here
    for (int p=1; p<processes; ++p) 
    {
      // Stop all workers
      MPI_Send(0, 0, MPI_INT, 
	       p, die_tag, MPI_COMM_WORLD);
    }

    zmq_close(zmq_requester);
    zmq_ctx_destroy(zmq_context);
  }

  bool push_job(int rank, child_process& cp)
  {
    if(no_more_jobs)
      return false;

    request();
    std::string job;

    zmq_msg_t reply;
    zmq_msg_init(&reply);
    int job_size = zmq_msg_recv(&reply, zmq_requester, 0);

    if(job_size==0)
    {
      std::cout << "mpibridge: (empty) no more jobs" << std::endl;
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

    std::cout << "mpibridge: started on process " << rank 
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
	wait_for_cmd();
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
	else if(cmd == "RQF")
	{
	  std::string tag = receive_argument();
	  std::string file = add_data_file(tag.c_str());
	  reply_string(file.c_str());
	}
	else
	{
	  std::cerr << "mpibridge: unknown command received" 
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
    master_process m(argv[1], argv[2]);
  }
  else
  {
    // Simply start child processes 
    worker_process w(argv[1], argv[2], argv[3]);
  }

  MPI_Finalize ();
  return 0;
}


#endif /* _MPIBRIDGE_H_ */
