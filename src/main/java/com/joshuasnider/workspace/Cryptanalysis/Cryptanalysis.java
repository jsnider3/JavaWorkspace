package com.joshuasnider.workspace.Cryptanalysis;

public class Cryptanalysis {

  public static int[] lettercnt = new int[26];

   public static int[] lettercnt(String cyphertext) {

     for (int y = 0; y < 25; y++) {
      lettercnt[y]=0;
     }
     for (int x =  0; x < cyphertext.length(); x++) {
       char c = cyphertext.charAt(x);
       if(c == 'A'){
         lettercnt[0]++;
       }
       if(c == 'B'){
         lettercnt[1]++;
       }
       if(c == 'C'){
         lettercnt[2]++;
       }
       if(c == 'D'){
         lettercnt[3]++;
       }
       if(c == 'E'){
         lettercnt[4]++;
       }
       if(c == 'F'){
         lettercnt[5]++;
       }
       if(c == 'G'){
         lettercnt[6]++;
       }
       if(c == 'H'){
         lettercnt[7]++;
       }
       if(c == 'I'){
         lettercnt[8]++;
       }
       if(c == 'J'){
         lettercnt[9]++;
       }
       if(c == 'K'){
         lettercnt[10]++;
       }
       if(c == 'L'){
         lettercnt[11]++;
       }
       if(c == 'M'){
         lettercnt[12]++;
       }
       if(c == 'N'){
         lettercnt[13]++;
       }
       if(c == 'O'){
         lettercnt[14]++;
       }
       if(c == 'P'){
         lettercnt[15]++;
       }
       if(c == 'Q'){
         lettercnt[16]++;
       }
       if(c == 'R'){
         lettercnt[17]++;
       }
       if(c == 'S'){
         lettercnt[18]++;
       }
       if(c == 'T'){
         lettercnt[19]++;
       }
       if(c == 'U'){
         lettercnt[20]++;
       }
       if(c == 'V'){
         lettercnt[21]++;
       }
       if(c == 'W'){
         lettercnt[22]++;
       }
       if(c == 'X'){
         lettercnt[23]++;
       }
       if(c == 'Y'){
         lettercnt[24]++;
       }
       if(c == 'Z'){
         lettercnt[25]++;
       }
       if(c == 'a'){
         lettercnt[0]++;
       }
       if(c == 'b'){
         lettercnt[1]++;
       }
       if(c == 'c'){
         lettercnt[2]++;
       }
       if(c == 'd'){
         lettercnt[3]++;
       }
       if(c == 'e'){
         lettercnt[4]++;
       }
       if(c == 'f'){
         lettercnt[5]++;
       }
       if(c == 'g'){
         lettercnt[6]++;
       }
       if(c == 'h'){
         lettercnt[7]++;
       }
       if(c == 'i'){
         lettercnt[8]++;
       }
       if(c == 'j'){
         lettercnt[9]++;
       }
       if(c == 'k'){
         lettercnt[10]++;
       }
       if(c == 'l'){
         lettercnt[11]++;
       }
       if(c == 'm'){
         lettercnt[12]++;
       }
       if(c == 'n'){
         lettercnt[13]++;
       }
       if(c == 'o'){
         lettercnt[14]++;
       }
       if(c == 'p'){
         lettercnt[15]++;
       }
       if(c == 'q'){
         lettercnt[16]++;
       }
       if(c == 'r'){
         lettercnt[17]++;
       }
       if(c == 's'){
         lettercnt[18]++;
       }
       if(c == 't'){
         lettercnt[19]++;
       }
       if(c == 'u'){
         lettercnt[20]++;
       }
       if(c == 'v'){
         lettercnt[21]++;
       }
       if(c == 'w'){
         lettercnt[22]++;
       }
       if(c == 'x'){
         lettercnt[23]++;
       }
       if(c == 'y'){
         lettercnt[24]++;
       }
       if(c == 'z'){
         lettercnt[25]++;
       }
     }
    return lettercnt;
  }
}
