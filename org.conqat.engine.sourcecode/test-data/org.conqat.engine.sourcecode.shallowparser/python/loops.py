while a < n:
	print a
	
for i in xrange(1, 10, 2):
	print i
	
foo = [1, 2, 3, 3]
for b in foo:
	print b
	for x in range(5, 10):
		print x
		
for x in [x for x in range(1, 10)]:
	print x
	
for ship in self.ships[:]:
	ship.remove()