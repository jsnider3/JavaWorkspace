package com.joshuasnider.workspace.internetio;

import java.net.*;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.io.*;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

public class URLConnectionReader {
	//public static URL url; public static URLConnection webpage; public static BufferedReader in;
	public static StringBuffer html; public static URL imagesource;
    public static void main(String[] args) throws Exception  {
        printHtml();//I love having readable code.
        saveWebpageAsHTMLFile();
    }
    
    public static void printHtml()throws Exception{
    	URL url = new URL("http://www.cnn.com/");
        URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        String inputLine;// = new StringBuffer();
        html = new StringBuffer();//Html is where we'll store the html files
        while ((inputLine =in.readLine()) != null){html.append(inputLine);}
        System.out.println(html);//This displays the code in the console.
        //System.out.println(inputLine);
        in.close();//This closes the connection.
    }
    
    public static void saveWebpageAsHTMLFile() throws IOException{
    	FileWriter fstream = new FileWriter("out.html");
    	//BufferedWriter out = new BufferedWriter(fstream);
    	fstream.write(html.toString());
    	fstream.close();
    }
    
    public static void getImage(){
		try {
			imagesource = new URL("http://pinkie.ponychan.net/chan/files/src/132287935236.gif");
			/* Test Images:
			 * http://www.cs.gmu.edu/~sean/sean.jpg
			 * http://pinkie.ponychan.net/chan/files/src/132255438967.png
			 * http://pinkie.ponychan.net/chan/files/thumb/132169880059s.jpg
			 * http://pinkie.ponychan.net/chan/files/src/132287935236.gif
			*/
			
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
	}
	
	/*private static void showImage() {
        //ImageIcon icon = new ImageIcon(imagesource);
        //JLabel label = new JLabel(icon, JLabel.CENTER);
        boolean userResponse=true;
        JScrollBar scrollBar=new JScrollBar();
        //label.add(scrollBar);
        //JOptionPane.showMessageDialog(null, label, "icon", -1);
        if(userResponse){
        	saveImage();
        }
        else{nextImage();}
    }*/
	
	public static void showAndSaveImage() {
		String format = getImageFormat();
		
		try {
		    ReadableByteChannel in = Channels.newChannel(imagesource.openStream());
        	FileOutputStream out = new FileOutputStream("temp."+format);
        	out.getChannel().transferFrom(in, 0, 1 << 24);
        	out.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		ImageIcon icon = new ImageIcon("temp."+format);
		JLabel label = new JLabel(icon, JLabel.CENTER);
		JOptionPane.showMessageDialog(null, label, "icon", -1);
	}
	
	private static String getImageFormat() {
		String imagetitle = imagesource.toString();
		int len = imagetitle.length();
		return	imagetitle.substring(len-3,len);
	}

	public static void nextImage() {
		//This 
	}
}
