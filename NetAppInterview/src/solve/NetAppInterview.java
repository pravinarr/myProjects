package solve;
/**
 * Class : NetAppInterview :for the solving the questions given in the interview 
 * Created By : Praveenkumar Ramasamy
 * For : NetApp interview 
 * Date : 03/07/2015
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class NetAppInterview {

	/**
	 * Variable for the population list
	 */
	private Map<String, double[]> population = new HashMap<String, double[]>();

	/**
	 * Getter for the population
	 * @return population Map<String, double[]> 
	 */
	public Map<String, double[]> getPopulation() {
		return population;
	}

	/**
	 * Setter for the population
	 * @param population Map<String, double[]>
	 */
	public void setPopulation(Map<String, double[]> population) {
		this.population = population;
	}

	/**
	 * Method to load the csv file 
	 * @param fileName
	 * @throws IOException
	 */
	public void loadcsv(String fileName) throws IOException {
		@SuppressWarnings("resource")
		BufferedReader br = new BufferedReader(new FileReader(fileName));
		String line;
		br.readLine();
		while ((line = br.readLine()) != null) {
			line.replace("\"", "");
			String[] splits = line.split(",");
			double[] popul = { Double.valueOf(splits[2]),
					Double.valueOf(splits[3]), Double.valueOf(splits[4]) };
			population.put(splits[0] + "," + splits[1], popul);
		}
	}

	/**
	 * Method to get the top cities
	 * 1)can choose the option for no of cities in the result using the noOfCities parameter
	 * 2)Can choose whether the result list should contain highest or lowest using minMaxflag
	 * @param citypercentages
	 * @param minMaxFlag
	 * @param noOfCities
	 * @return
	 */
	public List<ValueSet> getTopCities(List<ValueSet> citypercentages,
			boolean minMaxFlag, int noOfCities) {
		List<ValueSet> topList = new ArrayList<ValueSet>();

		for (ValueSet set : citypercentages) {
			if (topList.size() < noOfCities) {
				topList.add(set);
			} else {
				for (ValueSet temp : topList) {
					if (minMaxFlag) {
						if (temp.getPercentChange2() < set.getPercentChange2()) {
							temp = getMinMaxPercentage(topList, minMaxFlag);
							topList.remove(temp);
							topList.add(set);
							break;
						}
					} else {
						if (temp.getPercentChange2() > set.getPercentChange2()) {
							temp = getMinMaxPercentage(topList, minMaxFlag);
							topList.remove(temp);
							topList.add(set);
							break;
						}
					}
				}
			}
		}
		return topList;
	}

	/**
	 * Method to get the lowest or highest in the list
	 * @param highList
	 * @param minMaxFlag
	 * @return ValueSet
	 */
	protected ValueSet getMinMaxPercentage(List<ValueSet> highList,
			boolean minMaxFlag) {

		ValueSet out = highList.get(0);
		for (ValueSet temp : highList) {

			if (temp.getPercentChange2() < out.getPercentChange2()
					&& minMaxFlag == true) {
				out = temp;
			} else if (temp.getPercentChange2() > out.getPercentChange2()
					&& minMaxFlag == false) {
				out = temp;
			}
		}
		return out;

	}

	/**
	 * Method to calculate the citoes percentage change
	 * @return List<ValueSet>
	 */
	public List<ValueSet> getMetroCitiesPercentChange() {

		List<ValueSet> highPopulation = new ArrayList<ValueSet>();
		for (Entry<String, double[]> entry : getPopulation().entrySet()) {
			if (entry.getValue()[1] > 50000 || entry.getValue()[2] > 50000) {

				ValueSet set = new ValueSet();
				String key = entry.getKey().replace("\"", "");
				set.setCityName(key.split(",")[0]);
				set.setStatename(key.split(",")[1]);

				if (entry.getValue()[1] > 50000) {
					set.setPercentChange1(((entry.getValue()[1] - entry
							.getValue()[0]) / (entry.getValue()[0])) * 100);
				}
				if (entry.getValue()[2] > 50000) {
					set.setPercentChange2(((entry.getValue()[2] - entry
							.getValue()[0]) / (entry.getValue()[0])) * 100);
					set.setPercentChange3(((entry.getValue()[2] - entry
							.getValue()[1]) / (entry.getValue()[1])) * 100);
				}
				set.setPopTwentyTen(entry.getValue()[0]);
				set.setPopTwentyEleven(entry.getValue()[1]);
				set.setPopTwentyTwelve(entry.getValue()[2]);
				highPopulation.add(set);
			}
		}
		return highPopulation;
	}

	/**
	 * Method to print the cities list
	 * @param result List<ValueSet>
	 */
	public void printResult(List<ValueSet> result) {
		for (ValueSet set : result) {
			System.out.println("CityName: "+set.getCityName() + " ,Statename :" + set.getStatename()
								+", PercentChange :"+ set.getPercentChange2() );
		}
		System.out.println();
	}

	/**
	 * Method to get the cumulative population for the metrocities
	 * @param input List<ValueSet>
	 * @return result Map<String,double[]>
	 */
	public Map<String, double[]> getStatesPopulation(List<ValueSet> input) {
		Map<String, double[]> result = new HashMap<String, double[]>();

		for (ValueSet temp : input) {
			if (result.containsKey(temp.getStatename())) {
				double[] val = result.get(temp.getStatename());
				val[0] = val[0] + temp.getPopTwentyTen();
				val[1] = val[1] + temp.getPopTwentyEleven();
				val[2] = val[2] + temp.getPopTwentyTwelve();
				result.put(temp.getStatename(), val);
			} else {
				double[] val = { temp.getPopTwentyTen(),
						temp.getPopTwentyEleven(), temp.getPopTwentyTwelve() };
				result.put(temp.getStatename(), val);
			}
		}
		return result;
	}

	/**
	 * Method to get the top states
	 * 1) Can choose number states using the noOfStates paramenter
	 * @param cumulativePopul Map<String,double[]>
	 * @param noOfStates int
	 * @return result Map<String,Double>
	 */
	public Map<String, Double> getTopStates(
			Map<String, double[]> cumulativePopul, int noOfStates) {
		Map<String, Double> result = new HashMap<String, Double>();
		for (Entry<String, double[]> entry : cumulativePopul.entrySet()) {
			if (result.size() < noOfStates) {
				result.put(entry.getKey(), ((entry.getValue()[2] - entry
						.getValue()[0]) / (entry.getValue()[0])) * 100);
			} else {
				double changePercent = ((entry.getValue()[2] - entry.getValue()[0]) / (entry
						.getValue()[0])) * 100;
				Entry<String, Double> minEntry = result.entrySet().iterator()
						.next();
				for (Entry<String, Double> resultEntry : result.entrySet()) {
					if (minEntry.getValue() > resultEntry.getValue()) {
						minEntry = resultEntry;
					}
				}
				if (minEntry.getValue() < changePercent) {
					result.remove(minEntry.getKey());
					result.put(entry.getKey(), changePercent);
				}
			}

		}
		return result;

	}

	/**
	 * Main Program
	 * @param args
	 */
	public static void main(String[] args) {
		NetAppInterview net = new NetAppInterview();
		try {
			net.loadcsv(System.getProperty("user.dir")+"/src/test.csv");
			List<ValueSet> temp = net.getMetroCitiesPercentChange();
			System.out
			.println("**********Top five Cities with highest population growth***************");
			net.printResult(net.getTopCities(temp, true, 5));
			System.out
					.println("**********Top five Cities with shirinking population growth*************");
			net.printResult(net.getTopCities(temp, false, 5));
			System.out
			.println("***********Top Five states with highest population growth****************");
			for (Entry<String, Double> resultEntry :net.getTopStates(net.getStatesPopulation(temp), 5).entrySet()) {
				System.out.println("State name "+resultEntry.getKey()+" and PercentChange :"+resultEntry.getValue());
			}
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
