public class HashCodeAndEqualsTest {

	public override int GetHashCode()
    {
        return 0;
    }
	
	private class InnerClass() {
	
		public override bool Equals(Object obj)
	    {
			return true;
	    }
	}

}
