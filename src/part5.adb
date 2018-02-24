with Ada.Calendar;
use  Ada.Calendar;

with Ada.Numerics.Float_Random;
use  Ada.Numerics.Float_Random;

with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

procedure Part5 is
	f3_deadline: Time;
	f3_finished: Boolean := false;

	--task in charge of starting and advising when f3 starts, 
	--and prints a message when f3 misses its deadline
	task type watch_dog is
		entry prime(deadline : in Time);--begin the watch_dog
	end watch_dog;
	task body watch_dog is
		active: Boolean := false;
		dl : Time;
	begin
		loop
			if active then
				Put_Line(" - F3 watchdog primed");
				delay until dl;
				
				-- if f3 is running print warning
				if active = true and then f3_finished = false then
					Put_Line(" - F3 missed its deadline");
				end if;
			end if;
			select --starts here, and primes for watch_dog delay. sets the deadline time
				accept prime(deadline : Time) do
					f3_finished := false;
					dl := deadline;
					active := true;
				end prime;
			end select;
		end loop;
	end watch_dog;

	rng: Generator;
	f3_time: Float;
	boot_time: Time;
	vTime, f1_prev, f1_next: Duration;
	f3_flag: Boolean;
	
	-- To print Duration variables you can instantiate the generic package Text_Io.Fixed_Io with a duration type: 
	-- "package DIO is new Text_Io.Fixed_Io(Duration);" The DIO package will then export, among other things, 
	-- the procedure DIO.Put(D:Duration, Fore:Field, Aft:Field) to print variable D of type Duration.
	-- See an example on how to use this below.
	package DIO is new Text_Io.Fixed_Io(Duration);
	
	-- Simulates a procedure executing for a variable period of time
	-- Prints out a message when it starts and finishes
	procedure F(name: String; run_time: Duration; boot_time: Time) is 
		vTime, start : Duration;
	begin
		vTime := Ada.Calendar.Clock - boot_time;
		start := vTime;
		
		Put(name); Put(" has  started executing. The time is now:"); DIO.Put(vTime); Put_Line("");
		
		loop -- a busy loop for simulating a simple "F* procedure" executing for the given duration run_time
			exit when vTime - start >= run_time;
			vTime := Ada.Calendar.Clock - boot_time;
		end loop;
		
		Put(name); Put(" has finished executing. The time is now:"); DIO.Put(vTime); Put_Line(""); 
		Put_Line("");
	end F;

	f3_watch: watch_dog;
begin
	Reset(rng); -- Seeding or equivalent operation to ensure unique state for rng
	vTime := 0.0;
	f1_prev := 0.0;
	f1_next := 1.0;
	f3_flag := False;
	boot_time := Ada.Calendar.Clock;

	loop
		vTime := Ada.Calendar.Clock - boot_time;

		-- Execute F1 every 1 second (with drift control)
		-- Execute F2 after F1 finishes
		if vTime - f1_next >= 0.0 then
			f1_prev := vTime;
			f3_flag := True;
			f1_next := f1_next + 1.000;
			
			F(name =>    "F1", run_time => 0.300, boot_time => boot_time);
			F(name => " - F2", run_time => 0.150, boot_time => boot_time);
		end if;
		
		-- Execute F3 0.5 seconds after F1 starts
		if f3_flag = True and then vTime - f1_prev >= 0.500 then
			f3_flag := False;
			f3_time := 0.2 + (0.4 * Random(rng)); -- give f3 a random execution time between 0.2 and 0.6
			
			f3_deadline := boot_time + f1_next;
			f3_watch.prime(f3_deadline);--start the watch_dog for f3
			F(name => " - F3", run_time => Duration(f3_time), boot_time => boot_time);
			f3_finished := true;
			
			-- Deal with the case where F3 missed its deadline
			loop
				exit when Ada.Calendar.Clock < f3_deadline;
				f1_next := f1_next + 1.000;
				f3_deadline := boot_time + f1_next;
			end loop;
		end if;
		
	end loop; --Main loop
end Part5;
