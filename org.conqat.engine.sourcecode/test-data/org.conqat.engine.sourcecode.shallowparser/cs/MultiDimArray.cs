namespace My.Namespace
{
	public class MyClass : MyOtherClass
	{
		private object[,] _fooBar;	

		public object[,] FooBar
		{
			get
			{
				return _fooBar;
			}
		}
	}

	public enum Color
	{
		Red,
		Green	
    }
}

