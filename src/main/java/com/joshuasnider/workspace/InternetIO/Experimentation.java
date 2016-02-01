package com.joshuasnider.workspace.internetio;

import java.util.Scanner;

public class Experimentation {
  public static void main (String[] args){
    String max="20111230";
    String currentComic = "20111230";
    System.out.println(max.compareTo(currentComic)!=-1);
  }

  public static String getNextDay(String input){
    int[] array = new int[]{31,28,31,30,31,30,31,31,30,31,30,31};
    int year = Integer.parseInt(input.substring(0,4));
    int month = Integer.parseInt(input.substring(4,6));
    int day = Integer.parseInt(input.substring(6));
    day++;
    if(month==2&&year%4==0){
      if(day>29){
        month++;
        day=1;
      }
    }
    else if(day>array[month-1]){
      month++;
      day=1;
    }

    if(month==13){
      month=1;
      year++;
    }
    //System.out.println(year+" "+month+" "+day);
    String result;
    if(month<10){
      result=Integer.toString(year)+"0"+Integer.toString(month);
      //System.out.println(result);
    }
    else {result=Integer.toString(year)+Integer.toString(month);}
    //System.out.println(result);
    if(day<10){
      result=result+"0"+Integer.toString(day);
    }
    else{result=result+Integer.toString(day);}
    return result;
  }
}
