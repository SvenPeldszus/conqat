package mypackage;

public class MyUtilityClass {

	public static int computeLoanInterest(int amt, float interestRate, int term) {
		// System.out.println("foo");
	}

    public static native void exportStaticMethod() /*-{
	    $wnd.computeLoanInterest =
	       $entry(@mypackage.MyUtilityClass::computeLoanInterest(IFI));
	}-*/;

}