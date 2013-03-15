#ifndef _MPIBRIDGE_H_
#define _MPIBRIDGE_H_

#include <mpi.h>
#include "process.hpp"

enum tag {
  die_tag = 42,
  job_tag,
  done_tag,
  cmd_tag,
  resp_tag,
};

bool push_job(int rank, child_process& cp)
{
  // Read job
  // No new jobs anymore?
  if(cp.stream.eof() || !cp.is_running())
  {
    std::cout << "mpibridge: No more jobs" << std::endl;
    return false;
  }
  cp.stream << "NXT" << std::endl;
  std::string job;
  std::getline(cp.stream, job);

  if(job.empty())
  {
    std::cout << "mpibridge: no more jobs" << std::endl;
    return false;
  }

  std::cout << "mpibridge: sending job (0->" << rank << ")" << std::endl;

  int job_size = job.size()+1;
    
  char * job_c = new char[job_size];
  strcpy(job_c,job.c_str());

  // Send out job configurations
  MPI_Send(&job_size, 1, MPI_INT, rank, job_tag, MPI_COMM_WORLD);
  MPI_Send(job_c, job_size, MPI_CHAR, rank, job_tag, MPI_COMM_WORLD);
    
  delete[] job_c;

  return true;
}

void master_process(char * master)
{
  MPI_Status status;
  int processes;
  int active = 0;

  // Start the master process instance
  child_process cp_master(master,0,"{ \"test\" : { \"action\" : \"loop\", \"begin\" : 0.1,  \"step\" : 0.1,  \"end\" : 0.61} }",true);

  // Request jobs for each process
  MPI_Comm_size(MPI_COMM_WORLD, &processes);
  std::cout << "mpibridge: started with " << processes << " processes" 
	    << std::endl;
  for (int p=1; p<processes; ++p) 
  {
    if(push_job(p, cp_master))
      active++;
  }

  // While stuff is to be done
  while(active)
  {
    int cmd_size;
    // Receive a command or a work finished message
    MPI_Recv(&cmd_size, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, 
	     MPI_COMM_WORLD, &status);

    if (status.MPI_TAG == done_tag)
    {
      if(!push_job(status.MPI_SOURCE, cp_master))
	active--;
    }
    else
    {
      MPI_Status cmd_status;
      // Receive command 
      char * cmd_c = new char[cmd_size];
      MPI_Recv(cmd_c, cmd_size, MPI_CHAR, status.MPI_SOURCE, 
	       cmd_tag, MPI_COMM_WORLD, &cmd_status);
      
      std::string resp("ACK");

      int resp_size = resp.size()+1;
      char * resp_c = new char[resp_size];
      strcpy(resp_c,resp.c_str());

      MPI_Send(&resp_size, 1, MPI_INT,  status.MPI_SOURCE, 
	       resp_tag, MPI_COMM_WORLD);
      MPI_Send(resp_c, resp_size, MPI_CHAR, status.MPI_SOURCE, 
	       resp_tag, MPI_COMM_WORLD);
      delete[] resp_c;
    }

  }

  // We're done here
  for (int p=1; p<processes; ++p) 
  {
    // Stop all workers
    MPI_Send(0, 0, MPI_INT, p, die_tag, MPI_COMM_WORLD);
  }

  // Close the connection to the master socket
  cp_master.close_connection();

}

void worker_process(char * worker, int rank)
{
  MPI_Status status;
  int job_size;
  char * job_c;

  while(true)
  {
    MPI_Recv(&job_size, 1, MPI_INT, 0, MPI_ANY_TAG , MPI_COMM_WORLD, &status);

    if (status.MPI_TAG == die_tag) 
      return;

    job_c = new char[job_size];
    MPI_Recv(job_c, job_size, MPI_CHAR, 0, job_tag, MPI_COMM_WORLD, &status);
    
    std::cout << "mpibridge: received job (" << rank << "<-0)" << std::endl;
    child_process cp_worker(worker, rank, job_c, false);
    
    while(cp_worker.is_running())
    {
      // Read in command
      std::string cmd;
      std::getline(cp_worker.stream, cmd);

      int cmd_size = cmd.size()+1;
      char * cmd_c = new char[cmd_size];
      strcpy(cmd_c,cmd.c_str());

      MPI_Send(&cmd_size, 1, MPI_INT, 0, cmd_tag, MPI_COMM_WORLD);
      MPI_Send(cmd_c, cmd_size, MPI_CHAR, 0, cmd_tag, MPI_COMM_WORLD);
      delete[] cmd_c;

      // Pass on response
      int resp_size;
      MPI_Recv(&resp_size, 1, MPI_INT, 0, resp_tag , MPI_COMM_WORLD, &status);

      char * resp_c = new char[resp_size];
      MPI_Recv(resp_c, cmd_size, MPI_CHAR, 0, resp_tag, MPI_COMM_WORLD, &status);

      cp_worker.stream << resp_c << std::endl;
      delete[] resp_c;
    }
    
    delete[] job_c;

    MPI_Send(0,0, MPI_INT, 0, done_tag, MPI_COMM_WORLD);
  }
}

int main(int argc, char *argv[])
{
  int cur_rank;
  MPI_Status status;
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &cur_rank);

  if(argc<2)
  {
    std::cerr << "mpibridge: too few arguments" << std::endl;
    exit(1);
  }
  // Am I the headnode (?) 
  if(cur_rank == 0)
  {
    master_process(argv[1]);
  }
  else
  {
    // Simply start child processes 
    worker_process(argv[1], cur_rank);
  }

  MPI_Finalize ();
  return 0;
}


#endif /* _MPIBRIDGE_H_ */