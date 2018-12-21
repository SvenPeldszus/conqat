// lots of newlines in the class to simplify debugging
public class Foo extends Bar {

	@Override
	public void doSchedule() {
		this.
			lock(new LockRunner(this) {

			@Override
			protected void doRefresh() {
				Foo.
					this.
						logger.
							log("Log me");
			}
		});
	}

}
