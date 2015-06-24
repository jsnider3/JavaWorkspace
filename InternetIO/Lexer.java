
import java.io.*;

import javax.swing.*;
import java.net.*;
import java.util.ArrayList;



public class Lexer
    {
	protected URL url; //protected File file;
	//Start of constructors.
	public Lexer(){
		
	}
	
	public Lexer(String input) throws MalformedURLException{
		try {
			url=new URL(input);
		} catch (MalformedURLException e) {
			url=new URL("http://"+input);//e.printStackTrace();
		}
	}
	
	public Lexer(URL input){
		url=input;
	}
	
	//Start of mutators.
	public void setURL(URL input){
		url=input;
	}
	
	
	public void setURL(String input) throws MalformedURLException{
		try {
			url=new URL(input);
		} catch (MalformedURLException e) {
			url=new URL("http://"+input);
		}
	}
	
	//Some accessors.
	public URL getURL(){
		return url;
	}
	
	//Here's the methods that actually do some stuff.
	
	public File getWebpage(String filename) throws IOException{
		URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        InputStream inputStream = webpage.getInputStream();
        byte[] bytes = new byte[1024];
		int len;
		File file = new File(filename);//url.toString()+
		PrintStream FileOut = new PrintStream(new FileOutputStream(file));
		while((len = inputStream.read(bytes)) > 0){
			FileOut.write(bytes,0,len);
		}
		FileOut.close();
        in.close();
		return file;
	}

	public ArrayList<String> getWebpage() throws IOException{
		URLConnection webpage = url.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        ArrayList<String> page = new ArrayList<String>();
		String input;// = new StringBuffer();
        while ((input =in.readLine()) != null){page.add(input);}
		in.close();
		return page;
	}
	
	public ArrayList<String> getWebpage(URL get) throws IOException{
		URLConnection webpage = get.openConnection();
        BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
        ArrayList<String> page = new ArrayList<String>();
		String input;// = new StringBuffer();
        while ((input =in.readLine()) != null){page.add(input);}
		in.close();
		return page;
	}
	
	public static ArrayList<String> getListOfImages(StringBuffer html){
		ArrayList<String> links=new ArrayList<String>();
		/*while(index<html.length()){*/
		for(int index=0;index<html.length();index++){
			index =html.indexOf("src=",index);
			if(index==-1){break;}//If there are no more "img" strings.
			int start=html.indexOf(Character.toString('"'),index+3);
			int end=html.indexOf(Character.toString('"'),start+1);
			System.out.println(html.substring(start+1,end));//+100));
			System.out.println(index);
			//index++;
			links.add(html.substring(start,end));
		}
		return links;
	}
	
	public ArrayList<String> getListofLinks(StringBuffer html){
		ArrayList<String> links=new ArrayList<String>();
		//int index=0;
		//while(index<html.length()){
		for(int index=0;index<html.length();index++){
			index =html.indexOf("href=",index);
			if(index==-1){break;}//If there are no more "href=" strings.
			int start=html.indexOf(Character.toString('"'),index+3);
			int end=html.indexOf(Character.toString('"'),start+1);
			//index++;
			links.add(html.substring(start+1,end));
		}
		return links;
	}
	
	public File convertRelativeLinksToAbsolute(File input) throws IOException{
		ArrayList<String> storage = convertFileToStringArray(input);
		///This code is ripped straight from the ArrayList version of this code
		String hostname = url.getHost();
		for(String string : storage){
			int temp=string.indexOf("href=\"");
			if(temp!=-1){
				if(string.substring(temp+6,temp+10)!="http"&&string.charAt(temp+6)=='/'){
					string=string.substring(0,temp+6)+"http://"+hostname+string.substring(temp+6);
				}
			}
			temp=string.indexOf("href='");
			if(temp!=-1){
				if(string.substring(temp+6,temp+10)!="http"&&string.charAt(temp+6)=='/'){
					string=string.substring(0,temp+6)+"http://"+hostname+string.substring(temp+6);
				}
			}
		}
		FileWriter writer = new FileWriter(input);
		for(String string:storage){
			writer.write(string+"\n");
		}
		writer.close();
		return input;
	}
	
	private ArrayList<String> convertFileToStringArray(File input) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader(input));
		ArrayList<String> output = new ArrayList<String>();
		String string;
		while((string = bufferedReader.readLine()) != null) { 
			output.add(string); 
		}
		bufferedReader.close();
		return output;
	}

	public ArrayList<String> convertRelativeLinksToAbsolute(ArrayList<String> links){
		for(int x=links.size()-1;x>0;x--){
			String temp=links.get(x);
			//if(temp.isEmpty()){links.remove(x);}
			//System.out.println(temp.charAt(0));
			//try{if(temp.charAt(0)!='/'){links.remove(x);}}
			//catch(StringIndexOutOfBoundsException e){}
			
		}
		for(int y=0;y<links.size();y++ ){
			String string=links.get(y);
			String website=url.toString();
			website=website.substring(0,website.length()-1);
			if(string.length()<4){
				links.set(y,website+string);
			}
			else if(string.substring(0,4)!="http:"){
				links.set(y,website+string);
			}
		}
		return links;
	}
	
	public ArrayList<String> getThingsInTags(){
		//Eventually this will do something.
		ArrayList<String> result = new ArrayList<String>();
		return result;
	}
	
	public static void print(String file) throws IOException{
		BufferedReader bufferedReader = new BufferedReader(new FileReader(file));
		String string;
		while((string = bufferedReader.readLine()) != null) { 
			System.out.println(string); 
		}
		bufferedReader.close();
	}
	
	public static void print(File file) throws IOException{
		BufferedReader bufferedReader = new BufferedReader(new FileReader(file));
		String string;
		while((string = bufferedReader.readLine()) != null) { 
			System.out.println(string); 
		}
		bufferedReader.close();
	}
	
	public static void print(ArrayList<String> strings) {
		for(String str : strings){
			System.out.println(str);
		}
	}
	
	public static void print(URL url){
		try {
			URLConnection webpage = url.openConnection();
			BufferedReader in = new BufferedReader(new InputStreamReader(webpage.getInputStream()));
			String input;
			while (( input =in.readLine()) != null){System.out.println(input);}} 
		catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public static File nameFile (File file, String rename){
		file.renameTo(new File(rename));
		return file;
	}

	public static ArrayList<String> getListOfLinks(File file) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader(file));
		String string;
		ArrayList<String> results = new ArrayList<String>();
		while((string = bufferedReader.readLine()) !=null) { 
			//String href="href\"";
			int num=string.indexOf("href=\"");
			if(num !=-1){
				int start =string.indexOf('"',num)+1;
				int end = string.indexOf('"',start);
				//System.out.println("DEBUG:"+start+" "+end+" "+ string);
				String link=string.substring(start,end);
				results.add(link);
			}
		}
		bufferedReader.close();
		return results;
	}
	
	public ArrayList<String> getListOfLinksStrict(File file) throws IOException {
		ArrayList<String> input = getListOfLinks(file);
		ArrayList<String> results = new ArrayList<String>();
		ArrayList<String> relative = new ArrayList<String>();
		for(int x=0;x<input.size();x++){
			String result = input.get(x);
			if(result.charAt(0)=='h'){
				results.add(input.get(x));
			}
			if(result.length()>1&&result.charAt(0)=='/'){
				relative.add(input.get(x));
			}
		}
		relative=purgeDuplicates(relative);
		this.convertRelativeLinksToAbsolute(relative);
		//results.add("\nThe following are from the relative links.\n");
		results=purgeDuplicates(results);
		results.addAll(relative);
		return results;
	}

	private ArrayList<String> purgeDuplicates(ArrayList<String> input) {
		ArrayList<String> results= new ArrayList<String>();
		results.add(input.get(0));
		for(int x=1;x<input.size();x++){
			//System.out.println(input.get(x)+" num: "+results.indexOf(input.get(x)));
			if(results.indexOf(input.get(x))==-1){
				results.add(input.get(x));
			}
		}
		return results;
	}
}