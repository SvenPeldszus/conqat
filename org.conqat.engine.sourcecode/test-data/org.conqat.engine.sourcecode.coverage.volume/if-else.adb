with Ada.Text_IO;

procedure Hello is
	package IO renames Ada.Text_IO;
	digit: Integer := 4;
begin
	if digit > 3 or else digit = 3 then
		if digit < 5 then
   			IO.Put_Line("Hello, world!");
		end if;
	elsif digit < 2 then
   		IO.Put_Line("Hello, world!");
	else 
		IO.Put_Line("Hello, world!");
	end if;
end Hello;
