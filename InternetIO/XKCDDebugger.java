import java.io.File;


public class XKCDDebugger {
	static boolean[] test;
	static int[] names;
	public static void main(String[] args){
		test= new boolean[1000];
		names = stripNames(getNames());
		initTest();
		printMissing();
	}

	private static void printMissing(){
		for(int x=0;x<1000;x++){
			if(test[x]==false){
				System.out.println(x);
			}
		}
	}

	private static void initTest() {
		for(boolean b:test){
			b=false;
		}
		for(int name:names){
			test[name]=true;
		}
	}

	private static String[] getNames() {
		File directory = new File("C:/Copy/Pictures/XKCD");
		return directory.list();
	}
	private static int[] stripNames(String[] input){
		//for(String in:input){
			//in=in.substring(0,in.indexOf('-'));
		//}
		int[] output=new int[input.length];
		for(int x=0;x<input.length;x++){
			output[x]=Integer.parseInt(input[x].substring(0,input[x].indexOf('-')));
		}
		return output;
	}
}
