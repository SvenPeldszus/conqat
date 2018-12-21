namespace Foo.Bar
{
    using System;
    using System.Collections.Generic;
    using System.IO;    
    using System.Xml;
 
    public static class Baz
    {
        private const string DirectTypeDependency = "";

        private static XmlOutputWriter outputWriter = new XmlOutputWriter();

		public Baz () {
			// init
		}

        public static void Main(string[] args)
        {
            bool run = ParseCommandlineParameters(args);
            if (run)
            {
                Run();
            }
        }

		/// <summary></summary>
		private double seconds;

    	public double Hours
	    {
	        get { return seconds / 3600; }
	        set { seconds = value * 3600; }
	    }		
    }
}

