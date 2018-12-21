public class HashCodeAndEqualsTest {

#if TEST
    public int foo
#else
    private int foo
#endif
    {
        get
        {
            return foo;
        }
    }
    
    private int bar;

}
