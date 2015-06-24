import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;


public class JosephMcCabeGetter {
	public static void main(String[] args) throws Exception{
		//I'm trying to books embedded in a series of websites of the form http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/book_##.html ## is between 01 and 19
		for(int count=2;count<3;count++){
			String page="";
			if(count<10){
				page=CommonFunctions.getWebpageAsString("http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/book_0"+count+".html");
			}
			else{
				page=CommonFunctions.getWebpageAsString("http://www.infidels.org/library/historical/joseph_mccabe/big_blue_books/book_"+count+".html");
			}
			int delete = page.indexOf("Order books by and about Joseph McCabe now.");
			int len = "Order books by and about Joseph McCabe now.".length();
			page = page.substring(delete+len);
			page = removeHtml(page);
			saveFile(page,count);
			//print(page);
		}
	}
	
	private static void print(ArrayList<String> s){
		for(String str : s){
			System.out.println(str);
		}
	}
	
	private static ArrayList<String> removeHtml(ArrayList<String> p){
		int x;
		for(x=0;x<p.size();x++){
			String str=p.get(x);
			/*if(str.contains("Top of Page")){
				break;//TODO There's a bug that makes my program get stuck near the end of the page. This code is my solution
			}*/
			int start=str.indexOf("<");
			int cnt=0;
			while(start!=-1&&cnt<5){
				int end=str.indexOf(">");
				str=str.substring(start,end).equals("<P")?str.substring(0,start)+"\n\n\t"+str.substring(end+1):str.substring(0,start)+str.substring(end+1);
				start=str.indexOf("<");
				cnt++;
			}
			p.set(x,str);
		}
		for(int cnt=p.size()-1;cnt>=x;cnt--){
			p.remove(cnt);
		}
		return p;
	}

	private static void saveFile(String page,int x) throws IOException{
		String title = page.substring(0,page.indexOf("\n"));
		title.trim();
		//File file = new File(title+".txt");
		FileWriter txt = new FileWriter(x+"."+title+".txt", false);
		BufferedWriter out = new BufferedWriter(txt);
		out.write(page);
		out.close();
		txt.close();
	}
}
