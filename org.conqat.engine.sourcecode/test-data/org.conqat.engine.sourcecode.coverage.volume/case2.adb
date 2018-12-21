with Ada.Text_IO;

procedure Hello is
	package IO renames Ada.Text_IO;
	X: Integer := 4;
begin
	case X is
	   when 1 =>
	      Walk_The_Dog;
	   when 5 =>
	      Launch_Nuke;
	   when 8 | 10 =>
	      Sell_All_Stock;
	end case;
end Hello;
