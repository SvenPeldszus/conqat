// test data for the FindingsBasedMethodAssessor
// we use JavaScript for the test data, as it is the most flexible language in terms of local functions

// simple function with 5 LOC and 3 SLOC
function simple() {
	var a;

	var b;
	// comment
	var c;
}

// function with nested functions. 6 LOC (we do not count initial empty lines),
// 5 SLOC.
function nested() {

	var a;

	function sub1() { // 1 (S)LOC
		var a;
	}

	var b;

	var c1;
	var c2;

	function sub2() { // 1 (S)LOC
		var a;
	}

	function sub3() { // 1 (S)LOC
		var a;
	}

	var d;
}

// This examples shows how we can have more (S)LOC in methods than in the file.
// The inner functions of "ugly" have 1 (S)LOC, the function itself has 3 lines
// but also code before, after, and between the inner function so we get 7
// (S)LOC reported overall. Running an auto-formatted will also result in 7 lines here.
function ugly(x) {
	var a;
	return map2(x, function(y) {return y * y;}, function(z) {return z + 1;});
	var b;
}
