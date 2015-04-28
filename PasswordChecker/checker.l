%{
  #include <string.h>
%}

%% 
[a-z][A-Z][a-z]{1,6}\n[a-z][A-Z0-9\[#@^]{5,9}$  {
    int numInts = 0;
    int numSpec = 0;
    char *pass = strchr(yytext, '\n');//Get the inputted password.
    int index = 0;
    char cur = pass[index];
    while (cur != 0){
      //Count the number of ints and special chars in the password
      if(isdigit(cur))
        numInts++;
      else if (!(isdigit(cur) || isalpha(cur) || isspace(cur)))
        numSpec++;
      index++;
      cur = pass[index];
    }
    if (numInts == 0)
    {
      printf("Password doesn't contain enough numbers.\n");
    }
    if(numSpec == 0)
    {
      printf("Password doesn't contain enough special chars.\n");
    }
    if(numSpec != 0 && numInts != 0){
      printf("Password and username valid.\n");
    }
  }

. {
    printf("Password or username invalid.\n");
  }

%%

int main(){
  printf("Type in the username on the first line and the password on the next.\n");
  yylex();
}
