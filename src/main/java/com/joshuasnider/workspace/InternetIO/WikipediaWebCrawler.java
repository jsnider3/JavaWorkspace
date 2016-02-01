package com.joshuasnider.workspace.internetio;

//FIXME What the fuck I don't even.
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;

//Searching on wikipedia in a browser is done with GET statements
//Those get statements are of the form GET "/wiki/Special:Search?search=" SEARCHTERM "&go=Go HTTP/1.1" where SEARCHTERM is a variable
//Naively, you should be able to search wikipedia by just going to wikipedia.org/wiki/Special:Search?search=SEARCHTERM&go=Go
//But as it turns out searching is handled by wikimediafoundation.org so you need to type in wikimediafoundation.org/BLAHBLAHBLAH.
public class WikipediaWebCrawler {
	public static void main(String[] args) throws IOException{

    	URL url = new URL("http://3.bp.blogspot.com/-TgP56qWVDZg/Tcv25DELJMI/AAAAAAAAABc/3VPJbgpyDw4/s1600/party_hard_cat2.gif");
    			//"http://www.wikimediafoundation.org/wiki/Special:Search?search=federal+style&go=Go");
        URLConnection webpage = url.openConnection();
        
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String inputLine;// = new StringBuffer();
        StringBuffer html = new StringBuffer();//Html is where we'll store the html files
        //Method 1
        /*while ((inputLine =in.readLine()) != null){html.append(inputLine+"\n");}
        System.out.println(html);//This displays the code in the console.
        //Test
        FileWriter txt = new FileWriter("RainbowDashWithGuitar.jpg", false);
		BufferedWriter out = new BufferedWriter(txt);
		out.write(html.toString());//This line clears the file.
		*/
		//Test
        
        
        //Method 2
		InputStream inputStream = webpage.getInputStream();
        byte[] bytes = new byte[1024];
		int len = 0;
		ArrayList<Byte> test = new ArrayList<Byte>();
		File output = new File(url.toString()+".html");
		//System.setOut(new PrintStream(new FileOutputStream("system_out.gif")));
		PrintStream FileOut = new PrintStream(new FileOutputStream("file.gif"));
		while((len = inputStream.read(bytes)) > 0){
			//test.append(bytes);
			FileOut.write(bytes,0,len);
			//System.out.write(bytes, 0, len);
			//html.append(bytes.toString());//This line sucks ass. It justs prints out "[B@1bf216a" over and over again.
		}
		
		
		//System.out.println(html);
        //Talk to the webpage
        /*webpage.setDoOutput(true);
        * PrintStream outStream = new PrintStream(webpage.getOutputStream());
        * outStream.println("GET /wiki/Special:Search?search=federal+style&go=Go HTTP/1.1");
        * outStream.close();
        */
		//System.out.println(html.toString());
        in.close();//This closes the connection.
	}
}
