package com.joshuasnider.workspace.internetio;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.net.Socket;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;

import javax.swing.JOptionPane;


public class PageGetterTwo {
	public static void main(String[] args){
		Socket client;
		String sitename = JOptionPane.showInputDialog("What website do you want to get?");//"www.cs.gmu.edu";
		//http://www.giantitp.com/Images/CafePress2011/News_Ornament2.gif
		String get = null;
		if(sitename.charAt(5)=='/'){//Removes http:// from the beginning of URLs.
			sitename=sitename.substring(7);
		}
		if(sitename.charAt(sitename.length()-1)=='/'){
			get ="/";
			sitename=sitename.substring(0,sitename.length()-1);
		}
		else{
			get = JOptionPane.showInputDialog("What file do you want to get?");//"/~sean/index.html HTTP/1.0";
		}
		//System.out.println(sitename);
		try {
			client = new Socket(sitename, 80);//This is where we connect to the website.
			PrintStream output = new PrintStream(client.getOutputStream());//This allows us to send output. to the website
			InputStream input = client.getInputStream();//This is where we receive input.
			output.println("GET " +get + " HTTP/1.0");
			output.println("User-Agent:  WebCrawler");
			output.println();
			byte[] bytes = new byte[1024];
			int len = 0;
			while((len = input.read(bytes)) > 0)
				System.out.write(bytes, 0, len);
			client.close();
		} catch (UnknownHostException e) {
				e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
