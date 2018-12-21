
namespace MyApplication

{
    /// <summary>
    /// foo
    /// </summary>

    public class Class1
    {

#if TEST
        public int foo
#else
        private int foo
#endif
        {
            get
            {
		// Console.writeln ("Value: " + foo);
                return foo;
            }
        }

    }
}
