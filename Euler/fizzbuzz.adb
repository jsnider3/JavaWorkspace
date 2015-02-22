with Ada.Text_IO; use Ada.Text_IO;
procedure Fizzbuzz is
  tSum : Integer := 0;
begin
  for tIndex in Integer range 1 .. 999 loop
    --Put(Integer'Image(tIndex));
    if (tIndex mod 3 = 0) or (tIndex mod 5 = 0) then
      tSum := tSum + tIndex;
    end if;
  end loop;
  Put(Integer'Image(tSum));
  New_Line(1);
end Fizzbuzz;
