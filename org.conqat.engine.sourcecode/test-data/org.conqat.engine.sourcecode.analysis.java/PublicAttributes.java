public class PublicAttributes {
	
	public int foo = 0;
	
	@AConQATFieldParameter()
	public int bar = 0;
	
	@AConQATFieldParameter()
	@Override
	public int zomg = 1337;
	
	@AConQATFieldParameter()
	/** A method comment */
	public int bar = 0;
}
