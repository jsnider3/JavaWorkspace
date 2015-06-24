import java.lang.Math;
public class UnitCircle {
	public static void main(String[] args){
		System.out.println("pi equals "+Math.PI);
		String[] divisors= {"0","pi/6","pi/4","pi/3","pi/2","2pi/3","3pi/4","5pi/6","pi"};
		double[] radians={0,Math.PI/6,Math.PI/4,Math.PI/3,Math.PI/2,(2*Math.PI)/3,(3*Math.PI)/4,(5*Math.PI)/6,Math.PI};
		for(int x=0;x<9;x++){
			System.out.println("Cos("+divisors[x]+") equals "+round(Math.cos(radians[x]))+". Sin("+divisors[x]+") equals "+round(Math.sin(radians[x])));
		}
	}
	
	public static double round(double input){
		input = input-(input%.01);
		return input;
		
	}
}
