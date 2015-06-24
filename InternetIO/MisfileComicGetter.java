import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.URLConnection;
import java.net.URL;

public class MisfileComicGetter extends ComicGetter{
	public static final String dir = "C:/Copy/Comics/Misfile/";
	public static void main(String[] args){
		boolean debug=true;
		try{//http://www.misfile.com/overlay.php?pageCalled=110
			int max=getMax();
			for(int count=getMin()+1;count<=max;count++){
				saveImage("http://www.misfile.com/overlay.php?pageCalled="+count,dir+Integer.toString(count)+".jpeg");
				//count++;
				if(count%100==0&&debug){
					System.out.println("Count is currently "+count);
				}
			}
		} catch (Exception e){
			e.printStackTrace();
			//System.out.println("Exception");
			//done=true;
		}
	}
	
	public static int getMin(){//Get the lowest comic number I don't have.
		String[] list = new File(dir).list();
		if(list==null)
			return 0;
		int rtrn=1;
		for(String str : list){
			try{
				int num = Integer.parseInt(str.substring(0,str.indexOf(".")));
				if(num>rtrn){
					rtrn=num;
				}
			}catch(Exception e){
				
			}
		}return rtrn;
	}
	
	public static int getMax() throws Exception{//Get the number of the most recent comic.
		URL url = new URL("http://www.misfile.com");
		URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
		String input;// = new StringBuffer();
        while ((input =in.readLine()) != null){
        	if(input.contains("<img src=\"overlay.php?pageCalled=2034\">")){
        		String str = input.substring(input.indexOf("pageCalled=")+11);
        		String temp="";
        		for(String c:str.split("")){
        			if("0123456789".contains(c)){
        				temp+=c;
        			}
        			else{
        				in.close();
        				return Integer.parseInt(temp);
        			}
        		}
        	}
        }
		in.close();
		return 2000;
		//throw new Exception("Comic number could not be found.");//If this is returned something went wrong
	}
}
