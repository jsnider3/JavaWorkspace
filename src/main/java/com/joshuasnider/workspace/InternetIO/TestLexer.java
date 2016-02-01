package com.joshuasnider.workspace.internetio;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Random;


public class TestLexer extends Lexer {
	
	public static int counter=16; public static String website;
	
	public static void main(String[] args) throws IOException{
		String url = "www.microsoft.com";
		String directory="";
		Lexer lexer = new Lexer("http://www.microsoft.com");//+url);
		System.out.println(new File(url).mkdir());
		String fileName="microsoft.html";
		//System.out.println(new URL(url).seperator);
		File webpage = lexer.getWebpage(url+"/"+fileName);
		//lexer.convertRelativeLinksToAbsolute(webpage);
		/*ArrayList<String> links = lexer.getListOfLinksStrict(webpage);
		//lexer.print(webpage);
		lexer.print(links);
		URL url = new URL("http://www.images.cnn.com/whatsup.html");
		System.out.println("PATH:"+url.getPath());
		System.out.println("HOST:"+url.getHost());
		System.out.println("AUTHORITY:"+url.getAuthority());*/
		//lexer.print(webpage);
		//webCrawl();
		
	}
	
	public static boolean webCrawl() {
		try {Lexer lexer = new Lexer();
		//String website ="http://www.cnn.com";
		if(website==null){website="http://www.fox.com";}
			lexer.setURL(website);
		
		String fileName="webCrawl.html";
		File webpage = lexer.getWebpage(fileName);
		ArrayList<String> links = lexer.getListOfLinksStrict(webpage);
		if(counter>0){
			Random rng = new Random();
			int get=(int) (rng.nextDouble()*links.size());
			//lexer.print(webpage);
			website=links.get(get);
			System.out.println(website);
			int cnt=0;
			while(webCrawl()==false){
				cnt++;
				if(cnt==3)return false;
			}
			counter--;
		}
		return true;
		//lexer.print(links);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
}

