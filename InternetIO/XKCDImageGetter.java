import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;


public class XKCDImageGetter extends ComicGetter{
	public static void main(String[] args) throws Exception{
		int NewestComic =getNewestComic();
		System.out.println(NewestComic);
		for(int x=1;x<NewestComic;x++){
			if(x==404)x++;
			String fileLoc=getHTML(x);
        	System.out.println(x);
        	saveImage(fileLoc,x);
        	System.out.println(fileLoc);
        }
	}
	
	public static int getNewestComic() throws Exception{
		URL xkcd = new URL("http://www.xkcd.com/");
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
	
	public static String getHTML(int num) throws Exception{
		URL url = new URL("http://www.xkcd.com/"+num);
        URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String input;// = new StringBuffer();
        StringBuffer html = new StringBuffer();//Html is where we'll store the html source.
        while ((input =in.readLine()) != null){html.append(input);}
        input=html.toString();
        int start=input.indexOf("http://imgs.xkcd.com/comics");
        int end=input.indexOf('"',start);
        String fileLoc = input.substring(start,end);
        return fileLoc;
	}
	
	public static void saveImage(String fileLoc,int comicnumber){
		String comicname=fileLoc.substring(28);
		ComicGetter.saveImage(fileLoc,comicnumber+comicname+".png");
	}
}














