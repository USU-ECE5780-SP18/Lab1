with Ada.Calendar;
use  Ada.Calendar;

with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

procedure Part4 is
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
		
		Put(name); Put(" has started executing. The time is now:"); DIO.Put(vTime); Put_Line("");
		
		loop -- a busy loop for simulating a simple "F* procedure" executing for the given duration run_time
			exit when vTime - start >= run_time;
			vTime := Ada.Calendar.Clock - boot_time;
		end loop;
		
		Put(name); Put(" has finished executing. The time is now:"); DIO.Put(vTime); Put_Line(""); 
		Put_Line("");
	end F;

begin
	vTime := 0.0; --the running counter 
	f1_prev := 0.0; -- used to know when f1 starts
	f1_next := 1.0; --used to help with drift, and know when the next round should start
	f3_flag := False; --flag to know if f3 can start
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
			F(name => " - F3", run_time => 0.200, boot_time => boot_time);
		end if;
		
	end loop; --Main loop
end Part4;
