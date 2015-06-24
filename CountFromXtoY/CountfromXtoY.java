import java.util.Scanner;
public class CountfromXtoY {
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		int x, y;
		String name = "";
		System.out.println("Enter the higher number: ");
		x = input.nextInt();
		
		System.out.println("Enter the lower number: ");
		y = input.nextInt();
				
		while (x > y){
			
			System.out.println(x);
			x --;
		}
	}

}
