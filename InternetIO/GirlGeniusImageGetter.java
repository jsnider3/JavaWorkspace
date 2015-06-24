
public class GirlGeniusImageGetter extends ComicGetter{
	public static String title = "http://www.girlgeniusonline.com/ggmain/strips/ggmain";//+.jpg
	public static void main(String[] args){
		String date=getStartDate();
		try{
			while(true){
				String fileLoc =title+date+".jpg";
				String saveName=fileLoc.substring(52);
				CommonFunctions.saveImage(fileLoc,saveName);
				date=getNextDay(date);
				date=getNextDay(date);
				fileLoc =title+date+".jpg";
				saveName=fileLoc.substring(52);
				CommonFunctions.saveImage(fileLoc,saveName);
				date=getNextDay(date);
				date=getNextDay(date);
				fileLoc =title+date+".jpg";
				saveName=fileLoc.substring(52);
				CommonFunctions.saveImage(fileLoc,saveName);
				date=getNextDay(date);
				date=getNextDay(date);
				date=getNextDay(date);
			}
			//System.out.println((title+date+".jpg").substring(52));
		} catch(Exception e){
			System.out.println(date);
		}
	}
	
	public static String getStartDate(){
		return "20100104";
	}
	
	public static String getNextDate(String date){
		do{
			date=getNextDay(date);
		}
		while(dateAlgorithm(date)!=1&&dateAlgorithm(date)!=3&&dateAlgorithm(date)!=5);
		return date;
	}
	
	public static int dateAlgorithm(String date){//TODO Figure out how this works and add comments.
		int year =Integer.parseInt(date.substring(0,4));
		int month=Integer.parseInt(date.substring(4,6));
		int day  =Integer.parseInt(date.substring(6));
		int Y=year;
		if(month==1||month==2){
			Y=year-1;
		}
		int d=day;
		int m=((month+9)%12)+1;
		int y=Integer.parseInt(Integer.toString(year).substring(2));
		int c=Integer.parseInt(Integer.toString(year).substring(0,2));
		return (int) ((d+(2.6*m-0.2)+(5*y/4)-(7*c/4))%7);
	}
}
