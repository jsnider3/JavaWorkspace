import java.io.*;
import java.net.*;

import javax.swing.JOptionPane;

public class PageGetter extends Lexer{
	
	
	public PageGetter(String input) throws MalformedURLException {
		super(input);
		// TODO Auto-generated constructor stub
	}

	public PageGetter(URL input) {
		super(input);
		// TODO Auto-generated constructor stub
	}

	public static void main(String[] args){
		Socket client;
		String sitename = JOptionPane.showInputDialog("What website do you want to get?");//"www.cs.gmu.edu";
		//http://www.giantitp.com/Images/CafePress2011/News_Ornament2.gif
		String get = null;
		//System.out.println(sitename.substring(0,7));
		//System.out.println(sitename.substring(7));
		if(sitename.charAt(5)=='/'){
			sitename=sitename.substring(7);
		}
		if(sitename.charAt(sitename.length()-1)=='/'){
			get ="/";
			sitename=sitename.substring(0,sitename.length()-1);
		}
		else{
			get = JOptionPane.showInputDialog("What file do you want to get?");//"/~sean/index.html HTTP/1.0";
		}
		System.out.println(sitename);
		try {
			client = new Socket(sitename, 80);//This is where we connect to the website.
			PrintStream output = new PrintStream(client.getOutputStream());//This allows us to send output. to the website
			InputStream input = client.getInputStream();//This is where we receive input.
			output.println("GET / HTTP/1.1");// + get + " HTTP/1.1");
			output.println("User-Agent:  WebBrowser");
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
