report modules_test.

module foo input.
	write 'foo'.
endmodule.

module bar output.
	write 'bar'.
endmodule.

module default.
	write 'default without input keyword'.
endmodule.

module foo.
module bar.
module default.
