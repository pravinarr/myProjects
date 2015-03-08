package solve;
/**
 * Class : ValueSet : For storing the intermediate and final result 
 * Created By : Praveenkumar Ramasamy
 * For : NetApp interview 
 * Date : 03/07/2015
 */

public class ValueSet {

	//City name 
	private String cityName;
	
	//State name 
	private String statename;
	
	//Population in 2010
	private double popTwentyTen;
	
	//Population in 2011
	private double popTwentyEleven;
	
	//Population in 2012
	private double popTwentyTwelve;
	
	//Population change in 2010 - 2011
	private double percentChange1;

	//Population change in 2010 - 2012
	private double percentChange2;
	
	//Population change in 2011 - 2012
	private double percentChange3;

	/**
	 * Getter for the cityName 
	 * @return the cityName
	 */
	public String getCityName() {
		return cityName;
	}

	/**
	 *Setter for the cityName 
	 * @param cityName the cityName to set
	 */
	public void setCityName(String cityName) {
		this.cityName = cityName;
	}

	/**
	 * Getter for the statename 
	 * @return the statename
	 */
	public String getStatename() {
		return statename;
	}

	/**
	 *Setter for the statename 
	 * @param statename the statename to set
	 */
	public void setStatename(String statename) {
		this.statename = statename;
	}

	/**
	 * Getter for the population in 2010 
	 * @return the popTwentyTen
	 */
	public double getPopTwentyTen() {
		return popTwentyTen;
	}

	/**
	 *Setter for the population in 2010 
	 * @param popTwentyTen the popTwentyTen to set
	 */
	public void setPopTwentyTen(double popTwentyTen) {
		this.popTwentyTen = popTwentyTen;
	}

	/**
	 * Getter for the population in 2011
	 * @return the popTwentyEleven
	 */
	public double getPopTwentyEleven() {
		return popTwentyEleven;
	}

	/**
	 *Setter for the population in 2011 
	 * @param popTwentyEleven the popTwentyEleven to set
	 */
	public void setPopTwentyEleven(double popTwentyEleven) {
		this.popTwentyEleven = popTwentyEleven;
	}

	/**
	 * Getter for the population in 2012
	 * @return the popTwentyTwelve
	 */
	public double getPopTwentyTwelve() {
		return popTwentyTwelve;
	}

	/**
	 *Setter for the population in 2012
	 * @param popTwentyTwelve the popTwentyTwelve to set
	 */
	public void setPopTwentyTwelve(double popTwentyTwelve) {
		this.popTwentyTwelve = popTwentyTwelve;
	}

	/**
	 * Getter for the percentage change between 2010 - 2011 
	 * @return the percentChange1
	 */
	public double getPercentChange1() {
		return percentChange1;
	}

	/**
	 *Setter for the percentage change between 2010 - 2011
	 * @param percentChange1 the percentChange1 to set
	 */
	public void setPercentChange1(double percentChange1) {
		this.percentChange1 = percentChange1;
	}

	/**
	 * Getter for the percentage change between 2010 - 2012
	 * @return the percentChange2
	 */
	public double getPercentChange2() {
		return percentChange2;
	}

	/**
	 *Setter for the percentage change between 2010 - 2012
	 * @param percentChange2 the percentChange2 to set
	 */
	public void setPercentChange2(double percentChange2) {
		this.percentChange2 = percentChange2;
	}

	/**
	 * Getter for the percentage change between 2011 - 2012
	 * @return the percentChange3
	 */
	public double getPercentChange3() {
		return percentChange3;
	}

	/**
	 *Setter for the percentage change between 2011 - 2012
	 * @param percentChange3 the percentChange3 to set
	 */
	public void setPercentChange3(double percentChange3) {
		this.percentChange3 = percentChange3;
	}

	
}
