
public class SizeCalculator {
	public static void main(String[] args){
		int FoundingSize=3;
		int avgDescendentNumber=4;
		int max=1000000;
		int total=FoundingSize;
		int Generations = 0;
		int cnt = 0;
		while (cnt<max)
		{ cnt=total + total*avgDescendentNumber;
		Generations++;}
		System.out.println(Generations);
	}
}
