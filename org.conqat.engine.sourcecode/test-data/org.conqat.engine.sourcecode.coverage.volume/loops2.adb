with Ada.Text_IO;

procedure Hello is
	package IO renames Ada.Text_IO;
begin
	for I in Integer range 1 .. 10 loop
		bar();
	end loop;
end Hello;
