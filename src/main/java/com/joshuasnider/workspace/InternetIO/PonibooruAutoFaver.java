package com.joshuasnider.workspace.internetio;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

//According to fiddler, by sending "POST /change_favorite HTTP/1.1"
//to a server while connected to a webpage you invert the boolean 
//that represents whether or not you've faved something.

public class PonibooruAutoFaver {
	public static void main(String[] args) throws IOException{

    	URL url = new URL("http://www.ponibooru.org/post/view/98011");//The IP Address for ponibooru.org is 64.90.57.145
        URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String inputLine;// = new StringBuffer();
        StringBuffer html = new StringBuffer();//Html is where we'll store the html files
        while ((inputLine =in.readLine()) != null){html.append(inputLine);}
        System.out.println(html);//This displays the code in the console.
        //System.out.println(inputLine);
        in.close();//This closes the connection.
	}
}
