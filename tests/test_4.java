// A sample program for searching element in arraysimport java.util.Scanner;

//CONDITIONALS, WHILE AND ARRAY
public class Main{
	public static void main(String[] args) 
	{
	int search_ele = 15;
	int n = 5, flag = 0;
	Scanner s = new Scanner(System.in);
	int intArr[] = new int[n];
	Scanner sc = new Scanner(System.in);
	System.out.println("Enter the array elements : ");
	while(i<n)
	{
		intArr[i] = sc.nextInt();
		i++;
	}
	System.out.println("The Array elements are: ");
	while(i<n)
	{
		// intArr[i] = sc.nextInt();
		System.out.print(intArr[i] + " ");
		i++;
	}
	while(i<5)
	{
		if(intArr[i] == search_ele)
		{
			flag = 1;
			break;
		}
		i++;
	}
	if(flag == 1)
	{
	System.out.println("\nElement Found");
	}
	else
	{
	System.out.println("\nElement not found");
	}
	
	}
   }