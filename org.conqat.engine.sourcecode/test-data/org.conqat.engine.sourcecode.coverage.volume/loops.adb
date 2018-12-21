with Ada.Text_IO;

procedure Hello is
	package IO renames Ada.Text_IO;
begin
	loop
		foo();
		exit when 2 > 3;
		bar();
		exit when 2 > 3 and 3 > 4;
	end loop;
end Hello;
