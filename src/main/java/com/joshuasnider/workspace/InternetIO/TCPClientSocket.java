package com.joshuasnider.workspace.internetio;

import java.net.*;
import java.io.*;
public class TCPClientSocket {
	public static void main(String[] args){
		Socket client;
		try {
			client = new Socket("www.wikimediafoundation.org", 80);
			PrintStream out = new PrintStream(client.getOutputStream());
			InputStream in = client.getInputStream();
			String theFile = "/wiki/Special:Search?search=federal+style&go=Go";
			out.println("GET " + theFile + " HTTP/1.1");
			out.println("User-Agent: WebCrawler");
			out.println();
			byte[] bytes = new byte[1024];
			int len = 0;
			while((len = in.read(bytes)) > 0)
				System.out.write(bytes, 0, len);
			client.close();
		} catch (UnknownHostException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
