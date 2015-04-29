#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

//This is my work for the programming problem 2.26 in the
//book Operating Systems Concepts 9th Edition.

//It copies a file from one place to another and
// checks for every possible failure condition.
int main()
{
  char *srcName = (char*)malloc(sizeof(char) * 80);
  printf("Where do you want to copy from?\n");
  scanf("%s",srcName);
  char *destName = (char*)malloc(sizeof(char) * 80);
  printf("Where do you want to copy to?\n");
  scanf("%s",destName);
  if (access (srcName, F_OK) != 0)
  {
    printf("The file doesn't exist.\n");
    return -1;
  }
  else if (access(srcName, R_OK) != 0)
  {
    printf("We don't have read permission.\n");
    return -1;
  }
  else if (access(destName, F_OK) != 0)
  {
    printf("Destination doesn't exist.\n");
    return -1;
  }
  else if (access(destName, W_OK) != 0){
    printf("We don't have write permission.\n");
    return -1;
  }
  else if(remove(destName) != 0)
  {
    printf("Unable to delete the file.\n");
    return -1;
  }
  FILE *file = fopen(destName, "w");
  if(file == NULL)
  {
    printf("Unable to open destination.\n");
    return -1;    
  }
  FILE *reader=fopen(srcName,"r");
  char temp;
  do{
    temp = (char)fgetc(reader);
    int code;
    if(temp != EOF)
      code = fputc(temp,file);
    if(code == EOF){
      printf("Error occurred during write");
      temp = EOF;
    }
  }while(temp !=EOF);
  fclose(file);
  fclose(reader);
}
