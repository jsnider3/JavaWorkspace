import java.lang.Math;
import javax.swing.JOptionPane;

public class PlanetaryMotion {
	public static String[] names={"Mercury","Venus","Earth","Mars","Jupiter","Saturn","Uranus","Neptune"};
	public static double[] radii={/*Mercury*/0.4,/*Venus*/0.72,/*Mercury*/1.0,/*Mars*/1.52,/*Jupiter*/5.204,/*Saturn*/9.582,/*Uranus*/19.229,/*Neptune*/30.103};
	public static double[] periods={/*Mercury*/88.0,/*Venus*/224.7,/*Earth*/365.25,/*Mars*/686.971,/*Jupiter*/4332.59,/*Saturn*/10759.22,/*Uranus*/30799.095,/*Neptune*/60190};
	public static double[] radians={/*Mercury*/0.0,/*Venus*/0.0,/*Earth*/0.0,/*Mars*/0.0,/*Jupiter*/0.0,/*Saturn*/0.0,/*Uranus*/0.0,/*Neptune*/0.0};
	//This corresponds to the date ??/??/????.
	
	public static void main(String[] args){
		//To calculate the x coordinate of a planet take the cosine
		//of the time multiplied by 2pi divided by the orbital period.
		//To calculate the y coordinate do the same but with the sine.
		String temp=JOptionPane.showInputDialog("Type in a name of a planet.");
		int planet = -1;
		for(int x=0;x<8;x++){
			if(temp.compareTo(names[x])==0){
				planet = x;
				break;
			}
		}
		if(planet==-1){System.out.println("That's not a recognized planet.");}
		else{
			/*System.out.println("Giving coordinates for the planet "+names[planet]+".");
			for(double x=0;x<1000;x++){
				double[] coordinates =getCoordinates(planet,x);
				System.out.println("The X-Coordinate on Day "+x+" is "+coordinates[0]+" AUs.");
				System.out.println("The Y-Coordinate is "+coordinates[1]+" AUs.");
			}	*/
			
			//Debugging rocks.
			
			/*System.out.println("Giving radians for the planet " +names[planet]+".");
			for(int x=0;x<366;x++){
				advancePlanets(-1);
				System.out.println(radianPos[planet]);
			}*/
			
			/*advancePlanets(100.0);
			double[][] test =getCoordinates();
			for(int x=0;x<8;x++){
				System.out.println(test[x][0]+" "+test[x][1]);
			}*/
		}
	}
	
	public static double[] getCoordinates(int planet){//,double time){
		double radius=radii[planet];
		//double period=periods[planet];
		double[] output = new double[2];
		//output[0]=Math.cos((double)2*time*Math.PI/period)*radius;
		//output[1]=Math.sin((double)2*time*Math.PI/period)*radius;
		output[0]=Math.cos(radians[planet])*radius;
		output[1]=Math.sin(radians[planet])*radius;
		return output;
		
	}
	
	public static double[][] getCoordinates(){
		double[][] output = new double[8][2];
		for(int x=0;x<8;x++){
			output[x][0]=getCoordinates(x)[0];
			output[x][1]=getCoordinates(x)[1];
		}
		return output;
	}	
	
	public static void advancePlanets(double time){
		for(int x=0;x<8;x++){
			double period=periods[x];
			radians[x]=(radians[x]+2*time*Math.PI/period)%(2*Math.PI);
		}
	}

}
