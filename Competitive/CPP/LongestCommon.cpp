#include <algorithm> 
#include <iostream>
#include <stdio.h>
#include <string.h>

//This solution was accepted 2014/04/04
//This solution is for UVA Judge problem 10192.
//This is a longest common substring problem.
int main(){
  int max;
  int caseNum=1;
  while (true)
  {
    std::string momStr, dadStr;
    //Read the first string.
    std::getline(std::cin, momStr);
    //If it's # break.
    if(momStr[0] == '#')
      break;
    //Then read the second string.
    std::getline(std::cin, dadStr);
    //Create a matrix of len(firststring) rows and len(secndstring) cols.
    int *mat = new int[momStr.length() * dadStr.length()];
    //for each row in the matrix
    int rows = momStr.length();
    int cols = dadStr.length();

    if (rows && cols)
    {
      for (int r = 0; r < rows; r++)
      {
        //for each column in the matrix
        for (int c = 0; c < cols; c++)
        {
          //Note: check for out-of-bound errors.
          //Case row 0, col 0
          if (r == 0 && c == 0)
          {
            mat[r * cols + c] = 0;
            if (momStr[0] == dadStr[0])
            {
              mat[0 * cols + c] = 1;
            }
          }
          //Case row 0, col>0
          else if (r == 0 && c != 0)
          {
            if (momStr[r] == dadStr[c])
            {
              mat[r * cols + c] = std::max(mat[r * cols + c - 1], 1);
            }
            else
            {
              mat[r * cols + c]=mat[r * cols + c - 1];
            }
          }
          //Case row>0, col 0
          else if (r != 0 && c == 0)
          {
            if (momStr[r] == dadStr[c])
            {
              mat[r * cols + c] = std::max(mat[(r - 1) * cols + c], 1);
            }
            else
            {
              mat[r * cols + c] = mat[(r - 1) * cols + c];
            }
          }
          //Case row>0, col>0
          else if(r > 0 && c > 0)
          {
            mat[r * cols + c] = std::max(mat[(r - 1) * cols + c],
                                         mat[r * cols + c - 1]);        
            if(momStr[r] == dadStr[c])
              mat[r * cols + c] = std::max(mat[r * cols + c],
                                           mat[(r - 1) * cols + c - 1] + 1);
          }
        }
      }
      max = mat[momStr.length() * dadStr.length() - 1];
    }
    else
    {
      max = 0;
    }
    //
    printf("Case #%d: you can visit at most %d cities.\n", caseNum, max);
    caseNum++;
    free(mat);
  }
}
