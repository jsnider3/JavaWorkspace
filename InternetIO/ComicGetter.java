import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;


public class ComicGetter {
	
	public static void main(String[] args){
		for(int i=1;i<=500;i++){
			String image="foxhound_"+String.format("%03d", i)+".png";
			String foxhound="http://www.doctorshrugs.com/foxhound/images/"+image;
			String file="C:/Copy/Webcomics/The Last Days of Foxhound/"+image;
			saveImage(foxhound,file);
		}
	}
	
	public static final String getNextDay(String input){
		int[] array = new int[]{31,28,31,30,31,30,31,31,30,31,30,31};
		int year = Integer.parseInt(input.substring(0,4));//"2000");
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
		String result;
		if(month<10){
			result=Integer.toString(year)+"0"+Integer.toString(month);
		}
		else {result=Integer.toString(year)+Integer.toString(month);}
		if(day<10){
			result=result+"0"+Integer.toString(day);
		}
		else{result=result+Integer.toString(day);}
		return result;
	}

	public static void saveImage(String fileLoc, String title){
		try {
			ReadableByteChannel in1 = Channels.newChannel(new URL(fileLoc).openStream());
			FileOutputStream out = new FileOutputStream(title);
	        out.getChannel().transferFrom(in1, 0, 1 << 24);
	    	} catch (IOException e) {
	    		e.printStackTrace();
	    	}
	}
}













