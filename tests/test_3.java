// Java program to find LCM of two numbers.
// SUPPORT OF 2D ARRAY AND MULTIPLE METHODS BEING CALLED 
class gfg {
	// Gcd of u and v using recursive method
	static int GCD(int u, int v)
	{
		if (u == 0)
			return v;
		return GCD(v % u, u);
	}

	// LCM of two numbers
	static int LCM(int u, int v)
	{
		return (u / GCD(u, v)) * v;
	}

	// The Driver method
	public static void main(String[] args)
	{
		int u = 25, v = 15;
		System.out.println("LCM of " + u + " and " + v
						+ " is " + LCM(u, v));
	}
}
class GFG {
	public static void main(String[] args)
	{
		// declares an Array of integers.
		int[] arr;
		int [][] arr2 = new int[5][5];

		// allocating memory for 5 integers.
		arr = new int[5];
		// initialize the first elements of the array
		arr[0] = 10;
		arr2[0][0] = 1;

		// initialize the second elements of the array
		arr[1] = 20;

		// // so on...
		arr[2] = 30;
		arr[3] = 40;
		arr[4] = 50;

		// accessing the elements of the specified array
		while(i<arr.length)
		{
			System.out.println("Element at index " + i + " : " + arr[i]);
			i++;
		}
	}
}
