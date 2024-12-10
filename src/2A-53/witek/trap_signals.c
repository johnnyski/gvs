/*
 * trap_signals.c
 *
 * By:    Thuy Nguyen
 *        NASA/TRMM Office
 *        Thuy.Nguyen@gsfc.nasa.gov
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/stat.h>



void handler(int sig)
{

  fprintf(stderr, "Got signal %d. Abort.\n", sig);

  if (sig == SIGINT || sig == SIGKILL || sig == SIGSTOP) {
	exit (-2);
  }
  exit(-1); 
}



void trap_signals(void (*handler)(int))
{
  signal(SIGINT, handler);
  signal(SIGFPE, handler);
  signal(SIGKILL, handler);
  signal(SIGILL, handler);
  signal(SIGSTOP, handler);
  signal(SIGSEGV, handler);

}
