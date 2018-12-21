public class Foo extends Bar {

	@Override
	public void doSchedule() {
		Tools.lock(new LockRunner(this) {

			@Override
			protected void doRefresh() {
				if (!getE().isEmpty()) {
					if (Log.tracer().isDebug()) {
						Foo.this.logger.log("Log me");
					}
					refreshAll();
				}
			}
		});
	}

}
