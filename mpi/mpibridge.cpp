#ifndef _MPIBRIDGE_H_
#define _MPIBRIDGE_H_

#include <mpi.h>
#include "process.hpp"

enum tags {
  die_tag = 42,
  job_tag,
  done_tag,
  cmd_tag
};

// #define DIE_TAG 41
// #define JOB_TAG 42
// #define DONE_TAG 43
// #define CMD_TAG 44

bool push_job(int process, child_process& cp)
{
  // Read job
  // No new jobs anymore?
  if(cp.stream.eof())
  {
    std::cout << "No more jobs" << std::endl;
    return false;
  }
  cp.stream << "NXT" << std::endl;
  std::string job;
  std::getline(cp.stream, job);

  if(job.empty())
  {
    std::cout << "No more jobs" << std::endl;
    return false;
  }

  std::cout << "Sending job: " << job << std::endl;

  int job_size = job.size()+1;
    
  char * job_c = new char[job_size];

  strcpy(job_c,job.c_str());

  // Send out job configurations
  MPI_Send(&job_size, 1, MPI_INT, process, job_tag, MPI_COMM_WORLD);
  MPI_Send(job_c, job_size, MPI_CHAR, process, job_tag, MPI_COMM_WORLD);
    
  delete[] job_c;

  return true;
}

void master_process(char * master)
{
  MPI_Status status;
  int processes;
  int active = 0;

  // Start the master process instance
  child_process cp_master(master,0,"{ \"Testconfig\" : { \"action\" : \"loop\", \"begin\" : 0.1,  \"step\" : 0.1,  \"end\" : 0.61} }",true);

  // Request jobs for each process
  MPI_Comm_size(MPI_COMM_WORLD, &processes);
  std::cout << "Hello mpi bridge started with n procs: " << processes << std::endl;
  for (int p=1; p<processes; ++p) 
  {
    if(push_job(p, cp_master))
      active++;
  }

  // While stuff is to be done
  while(active)
  {
    int cmd;
    // Receive a command or a work finished message
    MPI_Recv(&cmd, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, 
	     MPI_COMM_WORLD, &status);

    if (status.MPI_TAG == done_tag)
    {
      if(!push_job(status.MPI_SOURCE, cp_master))
	active--;
    }
    else
    {
      // Receive command 
    }

  }

  // We're done here
  for (int p=1; p<processes; ++p) 
  {
    // Stop all workers
    MPI_Send(0, 0, MPI_INT, p, die_tag, MPI_COMM_WORLD);
  }

}

void worker_process(char * worker)
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

    std::cout << "Received job: " << job_c << std::endl;

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
    worker_process(argv[1]);
  }

  MPI_Finalize ();
  return 0;
}


#endif /* _MPIBRIDGE_H_ */
