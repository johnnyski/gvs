int julian(int mo, int day, int year);

/***********************************************************************/
/*                                                                     */
/*                                                                     */
/*                            julian                                   */
/*                                                                     */
/*                                                                     */
/***********************************************************************/
static int daytab[2][13] = {
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
  {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
};

int julian(int mo, int day, int year)
{
  int leap;

  leap = (year%4 == 0 && year%100 != 0) || year%400 == 0;
  return day + daytab[leap][mo-1];
}

