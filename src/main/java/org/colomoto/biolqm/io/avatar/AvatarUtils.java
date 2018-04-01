package org.colomoto.biolqm.io.avatar;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.List;

/**
 * Facilities for the management of logical models for simulations
 * 
 * @author Rui Henriques
 * @author Pedro T. Monteiro
 */
public final class AvatarUtils {

	public final static int DEC_PLACES = 4;

	/**
	 * Creates a state (int array) where components are initialized as -1
	 * 
	 * @param nstates
	 *            the number of components
	 * @return the free state (int array with components initialized with -1)
	 */
	public static int[] getFreeChildren(int nstates) {
		int[] children = new int[nstates];
		for (int i = 0; i < nstates; i++)
			children[i] = -1;
		return children;
	}

	/**
	 * Creates a state (byte array) where components are initialized as -1
	 * 
	 * @param ncomponents
	 *            the number of components
	 * @return the free state (byte array with components initialized with -1)
	 */
	public static byte[] getFreeState(int ncomponents) {
		byte[] array = new byte[ncomponents];
		for (int i = 0; i < ncomponents; i++)
			array[i] = -1;
		return array;
	}

	/**
	 * Creates an identity node array (e.g. [0,1,2,3..,n])
	 * 
	 * @param nstates
	 *            the number of components
	 * @return the identity node array
	 */
	public static int[] getIdentityChildren(int nstates) {
		int[] children = new int[nstates];
		for (int i = 0; i < nstates; i++)
			children[i] = i;
		return children;
	}

	/**
	 * Creates a state where only a single component is initialized (others as -1)
	 * 
	 * @param nstates
	 *            number of components
	 * @param node
	 *            the component to be initialized
	 * @return the array with the initialize 'node' component (e.g.
	 *         [-1,-1,..,-1,p,-1,..-1]=
	 */
	public static int[] getChildrenWithSingleNode(int nstates, int node) {
		int[] children = new int[nstates];
		for (int i = 0; i < nstates; i++)
			children[i] = node;
		return children;
	}

	/**
	 * List of states to string
	 * 
	 * @param list
	 *            the list of states (byte arrays)
	 * @return a string representing a list of states
	 */
	public static String toString(List<byte[]> list) {
		String result = "{";
		for (byte[] vec : list) {
			result += toString(vec);
		}
		return result + "}";
	}

	/**
	 * Byte array to string
	 * 
	 * @param vector
	 *            the byte array
	 * @return textual representation of the byte array
	 */
	public static String toString(byte[] vector) {
		String result = "[";
		for (int i = 0; i < vector.length; i++) {
			result += vector[i];
		}
		return result + "]";
	}

	public static String toOpenString(byte[] vector) {
		if (vector.length == 0)
			return "";
		String result = "" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result;
	}

	/**
	 * Integer array to string
	 * 
	 * @param vector
	 *            the integer array
	 * @return textual representation of the integer array
	 */
	public static String toString(int[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result + "]";
	}

	/**
	 * Long array to string
	 * 
	 * @param vector
	 *            the long array
	 * @return textual representation of the long array
	 */
	public static String toString(long[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result + "]";
	}

	/**
	 * BigInteger array to string
	 * 
	 * @param vector
	 *            the BigInteger array
	 * @return textual representation of the BigInteger array
	 */
	public static String toString(BigInteger[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result + "]";
	}

	/**
	 * Double array to string
	 * 
	 * @param vector
	 *            the double array
	 * @return textual representation of the double array
	 */
	public static String toString(double[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + round(vector[i]);
		return result + "]";
	}

	/**
	 * String array to string
	 * 
	 * @param vector
	 *            the String array
	 * @return textual representation of the String array
	 */
	public static String toString(String[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result + "]";
	}

	/**
	 * Boolean array to string
	 * 
	 * @param vector
	 *            the Boolean array
	 * @return textual representation of the Boolean array
	 */
	public static String toString(boolean[] vector) {
		if (vector.length == 0)
			return "[]";
		String result = "[" + vector[0];
		for (int i = 1; i < vector.length; i++)
			result += "," + vector[i];
		return result + "]";
	}

	/**
	 * Integer matrix to string
	 * 
	 * @param matrix
	 *            the matrix of integers
	 * @return textual representation of the matrix of integers
	 */
	public static String toString(int[][] matrix) {
		StringBuffer result = new StringBuffer("{");
		for (int i = 0; i < matrix.length; i++)
			result.append(toString(matrix[i]) + "\n");
		return result.toString() + "}";
	}

	/**
	 * Double matrix to string
	 * 
	 * @param matrix
	 *            the matrix of doubles
	 * @return textual representation of the matrix of doubles
	 */
	public static String toString(double[][] matrix) {
		StringBuffer result = new StringBuffer("{");
		for (int i = 0; i < matrix.length; i++)
			result.append(toString(matrix[i]) + "\n");
		return result.toString() + "}";
	}

	/**
	 * Converts a list of integers to an array
	 * 
	 * @param list
	 *            the list of integers
	 * @return the converted array of integers
	 */
	public static int[] toArray(List<Integer> list) {
		int[] array = new int[list.size()];
		for (int i = 0, l = list.size(); i < l; i++)
			array[i] = list.get(i);
		return array;
	}

	/**
	 * Converts an integer array to a byte array
	 * 
	 * @param vector
	 *            the integer array
	 * @return the casted byte array
	 */
	public static byte[] toByteArray(int[] vector) {
		byte[] array = new byte[vector.length];
		for (int i = 0, l = vector.length; i < l; i++)
			array[i] = (byte) vector[i];
		return array;
	}
	
	public static double round(double value, int decimalPlaces) {
	    BigDecimal bd = new BigDecimal(value);
	    bd = bd.setScale(decimalPlaces, RoundingMode.HALF_UP);
	    return bd.doubleValue();
	}
	public static double round(double value) {
		return round(value, DEC_PLACES);
	}
}
