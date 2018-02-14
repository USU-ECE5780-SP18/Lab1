with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

with Ada.Calendar;
use  Ada.Calendar;

procedure Part4 is
	vTime, boot_time, f1_last: Duration;
	f3_flag: Boolean;
	
	-- To print Duration variables you can instantiate the generic package Text_Io.Fixed_Io with a duration type: 
	-- "package DIO is new Text_Io.Fixed_Io(Duration);" The DIO package will then export, among other things, 
	-- the procedure DIO.Put(D:Duration, Fore:Field, Aft:Field) to print variable D of type Duration.
	-- See an example on how to use this below.
	package DIO is new Text_Io.Fixed_Io(Duration);
	
	-- Simulates a procedure executing for a variable period of time
	-- Prints out a message when it starts and finishes
	procedure F(Name: String; Run_time: Duration; Boot_time: Duration) is 
		vTime, start : Duration;
	begin
		vTime := Ada.Calendar.Seconds(Ada.Calendar.Clock) - boot_time;
		start := vTime;
		
		Put(Name);
		Put(" has started executing. The time is now:");
		DIO.Put(vTime);
		Put_Line("");
		
		loop --a busy loop for a simple "F* procedure"
			exit when vTime - start >= run_time;
			vTime := Ada.Calendar.Seconds(Ada.Calendar.Clock) - boot_time;
		end loop;
		
		Put(Name);
		Put(" has finished executing. The time is now:");
		DIO.Put(vTime);
		Put_Line("");
		Put_Line("");
	end F;

begin
	vTime := 0.0;
	f1_last := 0.0;
	f3_flag := False;
	boot_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);

	loop
		vTime := Ada.Calendar.Seconds(Ada.Calendar.Clock) - boot_time;

		-- Execute F1 every 1 second
		-- Execute F2 after F1 finishes
		if vTime - f1_last >= 1.000 then
			f1_last := vTime;
			f3_flag := True;
			
			F(Name => "F1", Run_time => 0.300, Boot_time => boot_time);
			F(Name => "F2", Run_time => 0.150, Boot_time => boot_time);
		end if;
		
		-- Execute F3 0.5 seconds after F1 starts
		if f3_flag = True and then vTime - f1_last >= 0.500 then
			f3_flag := False;
			F(Name => "F3", Run_time => 0.200, Boot_time => boot_time);
		end if;
		
	end loop; --Main loop
end Part4;