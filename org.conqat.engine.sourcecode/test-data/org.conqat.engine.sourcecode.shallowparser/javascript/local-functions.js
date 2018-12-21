function a() {
	for ( var i = 0; i < function(x) {
		return x * x;
	}(i); ++i) {
		console.log(i);
	}
}
