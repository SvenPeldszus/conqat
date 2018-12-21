public class AssemblyWatcher
{
	static void Main(string[] args)
	{ 
		try 
		{
			System.Console.WriteLine("code");
		} 
		catch (Exception e) 
		{
			System.Console.WriteLine("handle");
		} 
		finally 
		{
			System.Console.WriteLine("finally");
		}
	}
}

