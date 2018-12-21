package body Foo is

  
  procedure P is begin
    loop
      if cond1() then
        if cond2() then
          Label1 :
          while cond3() loop
            doIt();
          end loop Label1;
        end if;
      end if;
    end loop;
  end P;
  
end Foo;

