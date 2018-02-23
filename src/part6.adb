with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

with Ada.Text_IO;
use  Ada.Text_IO;

procedure part6 is
	-- The buffer resource in the sender/receiver or producer/consumer pattern
	-- Receives a close signal from the consumer
	task type buffer is
		entry push(n : in Integer);
		entry pop(n : out Integer);
		entry close;
	end buffer;
	
	-- Generates random integers between 0 and 25
	-- Receives a close signal from the consumer
	task type producer is
		entry close;
	end producer;
	
	-- Receives numbers from buffer and sums them together
	-- Upon reaching a sum of 100 or greater sends a close signal to buffer and producer
	task type consumer;
	
	t_buff : buffer;
	t_prod : producer;
	t_cons : consumer;
	
	-- Implements a circular buffer
	-- Includes locking mechanisms for push (if full) and pop (if empty) operations
	-- Task closes gracefully when it receives a close signal from the consumer
	task body buffer is
		done: Boolean := false; -- the flag that is updated when the close signal is received
		full: Boolean := false;
		empty: Boolean := true; -- a convenient shorthand for 'front == back && !full'
		
		SIZE : constant := 10;
		buff: array(0 .. 9) of Integer;
		
		front: Integer := 0; -- index of the circular buff array from which elements are popped
		back: Integer := 0; -- index of the circular buff array from which elements are pushed
	begin
		loop
			exit when done;
			select
				when not full =>
					accept push(n: Integer) do
						-- Thanks to our condition on accept the 'empty' in "front = back && empty" is implied
						if front = back then
							empty := false;
						end if;
						
						buff(back) := n;
						back := (back + 1) mod SIZE; -- mod SIZE => circular buffer
						
						-- Update our flag if adding this entry fills the circular buffer
						if front = back then
							full := true;
						end if;
					end push;
				or when not empty =>
					accept pop(n: out Integer) do
						-- Thanks to our condition on accept the 'full' in "front = back && full" is implied
						if front = back then
							full := false;
						end if;
						
						n := buff(front);
						front := (front + 1) mod SIZE; -- mod SIZE => circular buffer
						
						-- Update our flag if removing this entry empties the circular buffer
						if front = back then
							empty := true;
						end if;
					end pop;
				or
					accept close do
						done := true;
					end close;
			end select;
		end loop;
		
		select
			accept push(n: Integer) do
				null;
			end push;
		else
			null;
		end select;
		
		Put_Line("Exiting Buffer");
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
			--Put("Push '"); Put(n); Put_Line("'");
                        Put_Line("Push '" & Integer'Image(n) & "'";
			t_buff.push(n => n);
			select
				accept close do
					done := true;
				end close;
			else
				null;
			end select;
		end loop;
		Put_Line("Exiting Producer");
	end producer;
	
	task body consumer is
		done: Boolean := false;
		sum: Integer := 0;
		n: Integer;
	begin
		loop
			exit when done;
			t_buff.pop(n => n);
                        sum := sum + n;
                        Put_Line("Pop '" & Integer'Image(n) & "'";
                        Put_Line("Sum is '" & Integer'Image(sum) & "'";
			--Put("Pop  '"); Put(n); Put_Line("'");
			--Put("Sum is  '"); Put(sum); Put_Line("'");
			if sum >= 100 then
				t_buff.close; -- Must close the buffer first so it can release a potentially waiting producer
				t_prod.close;
				done := true;
			end if;
		end loop;
		Put_Line("Exiting Consumer");
	end consumer;
	
begin
	null;
end part6;
