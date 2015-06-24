
import java.util.Scanner;
public class Cipher {
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		String text = "";
		int encode = 0;
		int key;
		int i = 0;
		System.out.println("Basic Cipher by Josh Snider");
		System.out.println("Do you want to encode or decode?");
		text = input.nextLine();
		//System.out.println(key);
		if(text.equals("encode")){
			encode = 1;
			//System.out.println(text + encode);
		}
		else if(text.equals("decode")){
			encode = 0;
			//System.out.println(text + encode);
		}
		System.out.println("Type in the text.");
        text = input.nextLine();
        System.out.println("What's the key?");
		key = input.nextInt();
		//System.out.println(text);
        if(encode == 1){//This is for encoding.
           	for (i = 0; i < text.length(); i++) {
        		char c = text.charAt(i);
        		if( c >= 'a' && c <= 'z'){
        			c += key;
        			if(c > 122){
        				c -= 26;
        			}
        		}
        		else if( c >= 'A' && c <= 'Z'){
        			c += key;
        			if(c > 90){
        				c -= 26;
        			}
        		}
        		key ++;
        		if(key > 25){
        			key -= 26;
        		}
            System.out.print(c);
           	}
        }
        if(encode == 0){//This is for decoding
        	//The decoding process is currently buggy.
        	for (i = 0; i < text.length(); i++) {
        		char c = text.charAt(i);
        		if( c >= 'a' && c <= 'z'){
        			c -= key;
        			if(c < 97){
        				c += 26;
        			}
        		}
        		else if( c >= 'A' && c <= 'Z'){
        			c -= key;
        			if(c < 65){
        				c += 26;
        			}
        		}
        		key ++;
        		if(key > 25){
        			key -= 26;
        		}
            System.out.print(c);
           	}
        }
    }
}
