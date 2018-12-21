report a.
data a type i value 0. " dead store
a = 1.
write a.

data b type i value 0. " dead store
move 2 to b. " dead store

data c type i value 1.
if 1 = 1.
	b = 3.
	write c.
endif.

data d type i.
d = 2.
write d.

data e type i.
if a = 0.
	e = 2.
else.
	move 3 to e.
endif.
write e.