#include <stdio.h>
#include <math.h>

/****************************************************************
  Tim Grose
  January 29, 1991
  Miss Manners Data Generator

  The purpose of this program is to generate a file of make
  statements that can be used as an input data set for the 
  Miss Manners OPS5c program.

  All input to this program will be interactively obtained from
  the user.  The file of make statements will be written to 
  file manners.dat.

  The user specifies how many guests there will be.  Each guest's
  name will be a unique integer.  Each guest is assigned a sex at
  random.  The user can specify the total number of hobbies it is
  possible for a guest to have, and a lower limit of the number
  of hobbies for a guest.  For instance, if the user chooses 10
  hobbies and a lower limit of 3 hobbies, each guest will have
  between 3 and 10 hobbies.  The hobbies will be designated with
  an integer.  Finally, the user can specify the number of seats
  available.

  The sex of the guests is assigned so that approximately half of
  the guests are male and half are female.
*****************************************************************/

main()
{
  FILE *fopen(), *output_file;
  int seed, max_hobby, min_hobby, guest_num, seat_num, max_male, max_female;
  int chosen_hobby, count, number, rand_num, hobby_num, hobby_count;
  int hobbies[100], male_count, female_count;
  char sex,fname[20];
  float num;

  printf("Miss Manners Data Generator\n\n");
  printf("How many guests will there be? ");
  scanf("%d",&guest_num);

  strcpy(fname,"manners_");
  sprintf(&fname[8],"%d",guest_num);
  strcat(fname,".dat");

  output_file = fopen(fname,"w");
  if (output_file == NULL)
    {
	write("\n\nError opening output file\n");
	exit(20);
    }


  printf("How many seats are there? ");
  scanf("%d", &seat_num);
  printf("What is the maximum number of hobbies? ");
  scanf("%d", &max_hobby);
  printf("What is the minimum number of hobbies? ");
  scanf("%d", &min_hobby);

  seed = 10;
  srand(seed);
  
  male_count = 0;
  female_count = 0;
  max_male = guest_num / 2;
  max_female = guest_num - max_male;

  /****************************************************************/
  /* For each guest, assign a sex and the hobbies for that guest. */
  /****************************************************************/

  for(count = 1; count <= guest_num; ++count)
     {
       rand_num = rand();

       if (rand_num < 1073741824)
         sex = 'm';
       else
         sex = 'f';

       if ((sex == 'm') && (male_count == max_male))
         sex = 'f';

       if ((sex == 'f') && (female_count == max_female))
         sex = 'm';

       if (sex == 'm')
         male_count = male_count + 1;

       if (sex == 'f')
         female_count = female_count + 1;

      for(hobby_count = 1; hobby_count <= max_hobby; ++hobby_count)
        hobbies[hobby_count-1] = -1;

 /************************************************************/
 /* Determine the number of hobbies for this guest, then     */
 /* choose them.                                             */
 /************************************************************/

      rand_num = rand();
      num = (float) rand_num / 2147483647.0;
      hobby_num = (float) min_hobby + num *  (float) (max_hobby-min_hobby+1);

      for(hobby_count=1; hobby_count <= hobby_num; ++hobby_count)
        {
          do
            {
              rand_num = rand();
              num = (float) rand_num / 2147483647.0;
              chosen_hobby = 1.0 + num * (float) max_hobby;
            }
          while(hobbies[chosen_hobby - 1] != -1);

          hobbies[chosen_hobby - 1] = chosen_hobby;

          fprintf(output_file, "(make guest ^name %d ^sex %c ^hobby %d)\n",
                               count, sex, chosen_hobby);
        }

     }

  fprintf(output_file, "(make last_seat %d)\n", seat_num);
  fprintf(output_file, "(make count 1)\n");
  fprintf(output_file, "(make context start)\n");

} 
