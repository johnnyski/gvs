#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>
#include <errno.h>


pid_t chpid=0;
char  tmp_file[256];

void usage(char *prog)
{
  fprintf(stderr,"USAGE: %s [-v] [-P pathname_for_lookup_tables] param_file uf_file out_file\n", prog);
  exit(-1);
}

void rainest_handler(int sig)
{
  extern pid_t chpid;
  extern char  tmp_file[256];
  struct stat filestat;

  fprintf(stderr, "Got signal %d. Abort.\n", sig);
  
  /* Remove tmp file if it exists */
  if (stat(tmp_file, &filestat) == 0)
	remove(tmp_file);

  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit (-2);
  }
  exit(-1); 
}

main(int argc, char ** argv)
{
  extern void trap_signals(void (*handler)(int));
  extern void rainest_handler(int sig);
  /*
  extern int errno;
  */
  char  cmd[1000];
  int   stat;
  char  *param_file, *infile, *outfile;
  int   c;
  extern char *optarg;
  extern int optind,optopt;
  char  option_str[256], verbose_opt[10];
  int   verbose = 0;
  char  *p1_argv[8];
  int   i=0;
  struct stat file_stat;

  /* trap signals to exit with the right code */
  trap_signals(rainest_handler);

  memset(option_str, '\0', 256);
  memset(verbose_opt, '\0', 10);
  p1_argv[i++] = "out.x";
  while ((c = getopt(argc,argv,"P:v"))!=-1){
	switch (c) {
	case 'v':  verbose = 1; 
	  strcat(option_str, "-v ");
	  strcat(verbose_opt, "-v ");
	  p1_argv[i++] = argv[optind-1]; 
	  break;
	case 'P': 
	  strcat(option_str, "-P ");
	  strcat(option_str, optarg);
	  p1_argv[i++] = argv[optind-2]; 
	  p1_argv[i++] = argv[optind-1]; 
	  break;
    case '?': fprintf(stderr, "option -%c is undefined\n", optopt);
          usage(argv[0]);
    case ':': fprintf(stderr, "option -%c requires an argument\n",optopt);
          usage(argv[0]);
    default: break;
    }

  }

  if ( argc - optind != 3)  
    usage(argv[0]);
  param_file = argv[optind++];
  infile = argv[optind++];
  outfile = argv[optind++];
  
  /* 
  ** Preprocess the input file. This phase also performs some tests on 
  ** the validity of the input data. However, you may need to perform 
  ** some additional tests on the UF file before this point.
  **
  ** The preprocess will fail if the data is bad.
  */

  sprintf(tmp_file, "nrainest.%d.tmp", getpid());

  sprintf(cmd,"prep_proc %s -t CZ -i 20 %s %s",verbose_opt, infile, tmp_file);

  if (verbose) 
	fprintf(stderr, "%s: Executing ...%s\n", argv[0], cmd);

  /* Use execlp() instead of system() to get the exact program's exit code--
   * system() doesnot return the exit code of the program.
   */
  if ((chpid = fork()) == 0) {
	/* child */
	if (verbose) {
	  stat = execlp("prep_proc", "prep_proc", "-t", "CZ", "-i", "20", "-v",
				  infile, tmp_file, (char *) 0);
	}
	else {
	  stat = execlp("prep_proc", "prep_proc", "-t", "CZ", "-i", "20", 
				  infile, tmp_file, (char *)0);
	}
	if (verbose)
	  fprintf(stderr, "Errno = %d\n", errno);
	exit(stat);

  }
  /* parent */
  wait (&stat); /* Wait until child is finished. **/
  if (stat != 0) {
	remove(tmp_file);
	if (stat == -1) {
	  fprintf(stderr,"Failed executing prep_proc. Exiting %s. Stat: %d\n", argv[0], stat);

	  exit(-1);
	}
	else if (stat == -2) {
	  fprintf(stderr,"Aborted executing prep_proc. Exiting %s. Stat: %d\n", argv[0], stat);

	  exit(-2);
	}
  }

  if (lstat(tmp_file, &file_stat) == -1) {
	fprintf(stderr, "prep_proc produced no file.\n");
	exit(-1);
  }
  /* Now run the estimation algorithm */

  sprintf(cmd,"out.x %s %s %s %s", option_str, param_file, tmp_file, outfile);
  if (verbose) 
	fprintf(stderr, "%s: Executing ...%s\n", argv[0], cmd);

  /* Use execlp() instead of system() to get the exact program's exit code--
   * system() doesnot return the exit code of the program.
   */
  if ((chpid = fork()) == 0) {
	/* child */
	p1_argv[i++] = param_file;
	p1_argv[i++] = tmp_file;
	p1_argv[i++] = outfile;
	p1_argv[i++] = (char *) NULL;

	stat = execvp("out.x", p1_argv);
	exit(stat);
  }
  /* parent */
  wait (&stat); /* Wait until child is finished. **/
  /* Clean up temporary files */
  remove(tmp_file);

  if (stat != 0) {
	if (stat == -1) {
	  fprintf(stderr, "Failed running the estimation algorithm. stat = %d.\n", stat);
	  exit(-1);
	}
	else if (stat == -2) {
	  fprintf(stderr, "Aborted running the estimation algorithm. stat = %d.\n", stat);
	  exit(-2);
	}
  }

  exit (0);
}
