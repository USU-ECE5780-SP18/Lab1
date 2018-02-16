with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

with Ada.Text_IO;
use  Ada.Text_IO;

procedure Part6 is
	task type buffer is
		entry push(n : in Integer);
		entry pop(n : out Integer);
		entry close;
	end buffer;
	
	task type producer is
		entry close;
	end producer;
	
	task type consumer is
	end consumer;
	
	t_buff : buffer;
	t_prod : producer;
	t_cons : consumer;
	
	task body buffer is
		done: Boolean := false;
		full: Boolean := false;
		empty: Boolean := true;
		buff: array(0 .. 9) of Integer;
		front: Integer := 0;
		back: Integer := 0;
		SIZE : constant := 10;
	begin
		loop
			exit when done;
			select
				when not full =>
					accept push(n: Integer) do
						if front = back then
							empty := false;
						end if;
						
						buff(back) := n;
						back := (back + 1) mod SIZE;
						
						if front = back then
							full := true;
						end if;
					end push;
				or when not empty =>
					accept pop(n: out Integer) do
						if front = back then
							full := false;
						end if;
						
						n := buff(front);
						front := (front + 1) mod SIZE;
						
						if front = back then
							empty := true;
						end if;
					end pop;
				or
					accept close do
						if full then
							empty := true;
							full := false;
						end if;
						done := true;
					end close;
			end select;
		end loop;
	end buffer;
	
	task body producer is
		done: Boolean := false;
		rng: Generator;
		n: Integer;
	begin
		Reset(rng); -- Seeding or equivalent operation to ensure unique state for rng
		loop
			exit when done;
			
			n := Integer(25.0 * Random(rng)); -- generate a random number between 0 and 25
			Put("Push '"); Put(n); Put_Line("'");
			t_buff.push(n => n);
			select
				accept close do
					done := true;
				end close;
			end select;
		end loop;
	end producer;
	
	task body consumer is
		sum: Integer := 0;
		n: Integer;
	begin
		loop
			t_buff.pop(n => n);
			Put("Pop  '"); Put(n); Put_Line("'");
			sum := sum + n;
			if sum >= 100 then
				t_buff.close; -- Must close the buffer first so it can release a potentially waiting producer
				t_prod.close;
			end if;
		end loop;
	end consumer;
	
begin
	null;
end Part6;
