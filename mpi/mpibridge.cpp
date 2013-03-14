#ifndef _MPIBRIDGE_H_
#define _MPIBRIDGE_H_

#include <mpi.h>

#include <cstdlib>
#include <unistd.h>
#include <csignal>

#include <iostream>
#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/filesystem/path.hpp>

#define DIE_TAG 41
#define JOB_TAG 42
#define DONE_TAG 43
#define CMD_TAG 44

// #define JOB_MAX_SIZE 4096


#define CHILD_STDIN_READ pipefds_input[0]
#define CHILD_STDIN_WRITE pipefds_input[1]
#define CHILD_STDOUT_READ pipefds_output[0]
#define CHILD_STDOUT_WRITE pipefds_output[1]
#define CHILD_STDERR_READ pipefds_error[0]
#define CHILD_STDERR_WRITE pipefds_error[1]

namespace io = boost::iostreams;

struct child_process
{
  child_process(char * path) : stdin(&stdin_sb), stdout(&stdout_sb), stderr(&stderr_sb)
  {
    int pipe_status;
    pipe_status = pipe(pipefds_input);
    if (pipe_status == -1)
    {
      exit(EXIT_FAILURE);
    }

    pipe_status = pipe(pipefds_output);
    if (pipe_status == -1)
    {
      exit(EXIT_FAILURE);
    }

    pipe_status = pipe(pipefds_error);
    if (pipe_status == -1)
    {
      exit(EXIT_FAILURE);
    }

    pid_t pid;
    // Create child process; both processes continue from here
    pid = fork();

    if (pid == pid_t(0))
    {
      dup2 (CHILD_STDIN_READ,0);
      dup2 (CHILD_STDOUT_WRITE,1);
      dup2 (CHILD_STDERR_WRITE,2);
      // Close in the child the unused ends of the pipes
      close(CHILD_STDIN_WRITE);
      close(CHILD_STDOUT_READ);
      close(CHILD_STDERR_READ);

      // Execute the program
      boost::filesystem::path p(path);
	
      execl(path, p.filename().c_str(), (char*)NULL);

      // We should never reach this point
      // Tell the parent the exec failed
      kill(getppid(), SIGUSR1);
      exit(EXIT_FAILURE);
    }
    else if (pid > pid_t(0))
    {
      // Close in the parent the unused ends of the pipes
      close(CHILD_STDIN_READ);
      close(CHILD_STDOUT_WRITE);
      close(CHILD_STDERR_WRITE);

      stdin_sb.open(io::file_descriptor_source(CHILD_STDIN_WRITE, 
					    io::never_close_handle));
      stdout_sb.open(io::file_descriptor_source(CHILD_STDOUT_READ, 
					     io::never_close_handle));
      stderr_sb.open(io::file_descriptor_source(CHILD_STDERR_READ, 
					    io::never_close_handle));
    }
    else
    {
      std::cerr << "Error: fork failed" << std::endl;
      exit(EXIT_FAILURE);
    }

  }

  void close_pipes()
  {
    stdout_sb.close();
    stdin_sb.close();
    stderr_sb.close();
    close(CHILD_STDIN_WRITE);
    close(CHILD_STDOUT_READ);
    close(CHILD_STDERR_READ);
  }

  io::stream_buffer<io::file_descriptor_source> stderr_sb;
  io::stream_buffer<io::file_descriptor_source> stdout_sb;
  io::stream_buffer<io::file_descriptor_source> stdin_sb;

  std::istream stderr;
  std::istream stdout;
  std::ostream stdin;


  int pipefds_input[2];
  int pipefds_output[2];
  int pipefds_error[2];
};

bool push_job(int process, child_process& cp)
{
  // Read job
  std::string job("Testjob");
  int job_size = job.size()+1;
    
  char * job_c = new char[job_size];

  strcpy(job_c,job.c_str());

  // Send out job configurations
  MPI_Send(&job_size, 1, MPI_INT, process, JOB_TAG, MPI_COMM_WORLD);
  MPI_Send(job_c, job_size, MPI_CHAR, process, JOB_TAG, MPI_COMM_WORLD);
    
  std::cout << "Sending job: " << job_c << std::endl;

  delete[] job_c;

  return true;
}

void master_process(char * master)
{
  MPI_Status status;
  int processes;
  int active = 0;

  // Start the master process instance
  child_process cp_master(master);

  cp_master.stdin << "{ \"Test config\" : 5.0 }" << std::endl;

  // Request jobs for each process
  MPI_Comm_size(MPI_COMM_WORLD, &processes);
  std::cout << "Hello mpi bridge started with n procs: " << processes << std::endl;
  for (int p=1; p<processes; ++p) 
  {
    if(push_job(p, cp_master))
      active++;;
  }

  // While stuff is to be done
  while(active)
  {
    int cmd;
    // Receive a command or a work finished message
    MPI_Recv(&cmd, 1, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, 
	     MPI_COMM_WORLD, &status);

    if (status.MPI_TAG == DONE_TAG)
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
    MPI_Send(0, 0, MPI_INT, p, DIE_TAG, MPI_COMM_WORLD);
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

    if (status.MPI_TAG == DIE_TAG) 
      return;

    job_c = new char[job_size];
    MPI_Recv(job_c, job_size, MPI_CHAR, 0, JOB_TAG, MPI_COMM_WORLD, &status);

    std::cout << "Received job: " << job_c << std::endl;

    delete[] job_c;
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
