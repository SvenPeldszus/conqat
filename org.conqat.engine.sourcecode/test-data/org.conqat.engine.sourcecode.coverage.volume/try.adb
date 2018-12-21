with Ada.Text_IO;

procedure Hello is
	package IO renames Ada.Text_IO;
begin
   	IO.Put_Line("Hello, world!");
exception 
	when End_Error =>
		IO.Put_Line("break");
	when others =>
		IO.Put_Line("others");
end Hello;
