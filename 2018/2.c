#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

void
analyze_word(char *word, int *two_count, int *three_count)
{
  int has_two=0;
  int has_three=0;
  int freq['z' - 'a'] = { 0 };

  for(int i=0;; i++)
  {
    if (word[i] == 0)
      break;
    freq[word[i] - 'a']++;
  }

  for(int j=0; j< 'z' - 'a'; j++)
  {
    if(freq[j] == 2)
      has_two=1;
    if(freq[j] == 3)
      has_three=1;
  }
  if(has_two)
    *two_count = 1+*two_count;
  if(has_three)
    *three_count = 1+*three_count;
}

int
main(int argc, char *argv[])
{
  int two_count = 0;
  int three_count = 0; 
  char *line = NULL;    
  size_t len = 0;

  FILE *in = fopen("./2.input", "r");
  if (in == NULL)
    perror("Could not open file.");  

  while(getline(&line, &len, in) != -1) {
    analyze_word(line, &two_count, &three_count);
  }
  printf("answer: %i\n", two_count * three_count);
  return EXIT_SUCCESS;
}
