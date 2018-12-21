namespace Foo.Bar
{
    using System;
    using System.Collections.Generic;
    using System.IO;    
    using System.Xml;
 
 	/// my class
    public static class Baz
    {
        public static void Main(string[] args)
        {
        	int a = 0;
        	Del d = delegate()
            {
                System.Console.Write("Hello, ");
                System.Console.WriteLine("World!");
            };
        }
    }
}

