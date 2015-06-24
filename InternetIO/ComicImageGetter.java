import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;


public class ComicImageGetter extends ComicGetter {
	public static String webpage;
	public static void main(String[] args) throws Exception{
		//System.out.println(detectFormat("http://imgs.xkcd.com/comics/making_things_difficult.png"));
		//getSchlock();
		//getXKCD();
		//Run the below ASAP.
		saveImage(getXKCDFileLoc(getHTML(webpage+826)),"XKCD",826);
	}
	
	public static void getXKCD() throws Exception{
		int NewestComic =getNewestXKCDComic();
		webpage="http://www.xkcd.com/";
		System.out.println(NewestComic);
		for(int x=1;x<NewestComic+1;x++){
			if(x==404)x=405;
			String fileLoc=getXKCDFileLoc(getHTML(webpage+x));
        	saveImage(fileLoc,"XKCD",x);
        	//System.out.println(x);
        	//System.out.println(fileLoc);
        }
	}
	
	public static void getSchlock(){
		/* The first schlock mercenary comic was released on
		 * 2006/06/12. Since then comics have been released daily.
		 * There are no days without a posted comic.
		 * The format for the url for each comic is
		 * http://static.schlockmercenary.com/comics/schlock
		 * followed by a number which corresponds to the date in
		 * year month day form terminated with the format.
		 * Thus the comic for June 12th, 2000 is found at
		 * http://static.schlockmercenary.com/comics/schlock20000612.png
		 * Don't forget leap years.
		 */
		
		/*
		 * This code generates FileNotFoundExceptions when currentComic equals 20000618,
		 * 20000625, 20000702, 20000709, 20000716, 20000723, 20000730, 20000806, 20000813
		 * 20000820, 20000827, 20000903. It probably continues but I stopped it at 20000907.
		 * All of those days are Sundays and in all cases the comic can be reached by adding
		 * an a after the number in the hyperlink.
		 */
		String max="20111230";//=getNewestSchlockComic()
		String currentComic = "20000612";
		while((max.compareTo(currentComic))!=-1){
			try {
				ReadableByteChannel in1 = Channels.newChannel(new URL("http://static.schlockmercenary.com/comics/schlock"+currentComic+".png").openStream());
				FileOutputStream out = new FileOutputStream(currentComic+".png");//comicnumber+"-"+comicname+"."+detectFormat(fileLoc));
		        out.getChannel().transferFrom(in1, 0, 1 << 24);
		        out.close();
		    	} catch (FileNotFoundException e) {
		    		e.printStackTrace();
		    	} catch (IOException e) {
					e.printStackTrace();
				}
			System.out.println(currentComic);
			currentComic=getNextDay(currentComic);
		}
	}
	
	private static String getNewestSchlockComic() {
		//TODO Add this function
		return null;
	}
	
	public static int getNewestXKCDComic() throws Exception{
		URL xkcd = new URL(webpage);
		URLConnection webpage = xkcd.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String input;
        StringBuffer html = new StringBuffer();
        while ((input =in.readLine()) != null){html.append(input);}
        input=html.toString();
        int num = input.indexOf("|&lt");
        num = input.indexOf("href=",num);
        int end = input.indexOf("/",num+7);
        int comicnumber = Integer.parseInt(input.substring(num+7,end));
        return comicnumber+1;
	}
	
	public static StringBuffer getHTML(String page) throws Exception{
		URL url = new URL(page);
        URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String input;// = new StringBuffer();
        StringBuffer html = new StringBuffer();//Html is where we'll store the html source.
        while ((input =in.readLine()) != null){html.append(input);}
        return html;
        /*Everything after this should be put in getXKCDFileLoc().
        input=html.toString();
        int start=input.indexOf("http://imgs.xkcd.com/comics");
        int end=input.indexOf('"',start);
        String fileLoc = input.substring(start,end);
        return fileLoc;*/
	}
	
	public static String getXKCDFileLoc(StringBuffer html){
		String input=html.toString();
        int start=input.indexOf("http://imgs.xkcd.com/comics");
        int end=input.indexOf('"',start);
        String fileLoc = input.substring(start,end);
        return fileLoc;
	}
	
	public static void saveImage(String fileLoc, String Name, int comicnumber){//TODO Remove this.
		try {
		ReadableByteChannel in1 = Channels.newChannel(new URL(fileLoc).openStream());
		String comicname=fileLoc.substring(fileLoc.lastIndexOf("/")+1);
        FileOutputStream out = new FileOutputStream(comicnumber+"-"+comicname+"."+detectFormat(fileLoc));
        out.getChannel().transferFrom(in1, 0, 1 << 24);
        out.close();
    	} catch (IOException e) {
    		e.printStackTrace();
    	}
	}
	
	public static String detectFormat(String fileLoc){
		return fileLoc.substring(fileLoc.lastIndexOf(".")+1);
		//return output;
	}
}
