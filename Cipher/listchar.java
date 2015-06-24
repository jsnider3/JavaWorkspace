
public class listchar {
	public static void main(String[] args) {
		char c = 127;
		while(c > 32)//the characters < 32 are control characters can't be displayed properly.
			{
			System.out.println(c);
			c --;
		}
	}
}
