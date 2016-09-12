------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2015-2016, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of STMicroelectronics nor the names of its       --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

--  A simple example that blinks all the LEDs simultaneously, w/o tasking.
--  It does not use the various convenience functions defined elsewhere, but
--  instead works directly with the GPIO driver to configure and control the
--  LEDs.

--  Note that this code is independent of the specific MCU device and board
--  in use because we use names and constants that are common across all of
--  them. For example, "All_LEDs" refers to different GPIO pins on different
--  boards, and indeed defines a different number of LEDs on different boards.
--  The gpr file determines which board is actually used.

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with HAL; use HAL;
with STM32.Device;  use STM32.Device;
with STM32.Board;   use STM32.Board;
with STM32.Button;
with LCD_Std_Out;

with STM32.DAC;     use STM32.DAC;
with STM32.GPIO;    use STM32.GPIO;
with Ada.Real_Time; use Ada.Real_Time;

procedure Blinky is

   use type Word;

   Period : constant Time_Span := Milliseconds (500);  -- arbitrary
   subtype counts is Integer range 0 .. 10;
   count : counts := 0;

   Next_Release : Time := Clock;

   Output_Channel : constant DAC_Channel := Channel_1;
   procedure ConfigureDAC_GPIO (Output_Channel : DAC_Channel);

   procedure Initialize_LEDs;
   --  Enables the clock and configures the GPIO pins and port connected to the
   --  LEDs on the target board so that we can drive them via GPIO commands.
   --  Note that the STM32.Board package provides a procedure (with the same
   --  name) to do this directly, for convenience, but we do not use it here
   --  for the sake of illustration.

   procedure Initialize_LEDs is
      Configuration : GPIO_Port_Configuration;
   begin
      Enable_Clock (All_LEDs);

      Configuration.Mode        := Mode_Out;
      Configuration.Output_Type := Push_Pull;
      Configuration.Speed       := Speed_100MHz;
      Configuration.Resistors   := Floating;
      Configure_IO (All_LEDs, Configuration);
   end Initialize_LEDs;

   procedure ConfigureDAC_GPIO (Output_Channel : DAC_Channel) is
      Output : constant GPIO_Point := (if Output_Channel = Channel_1
                                       then DAC_Channel_1_IO
                                       else DAC_Channel_2_IO);
      Config : GPIO_Port_Configuration;
   begin
      Enable_Clock (Output);
      Config.Mode := Mode_Analog;
      Config.Resistors := Floating;
      Configure_IO (Output, Config);
   end ConfigureDAC_GPIO;


      Value   : Word := 0;
      Percent : Word;
      K       : Word := 0;

      Resolution : constant DAC_Resolution := DAC_Resolution_12_Bits;
      --  Arbitrary, change as desired.  Counts will automatically adjust.

      Max_Counts : constant Word := (if Resolution = DAC_Resolution_12_Bits
                                     then Max_12bit_Resolution
                                     else Max_8bit_Resolution);
begin
   Initialize_LEDs;
   STM32.Button.Initialize;
   ConfigureDAC_GPIO (Output_Channel);
   LCD_Std_Out.Put ("Hello, World!  ");
   LCD_Std_Out.Put ("Press to count");

   Enable_Clock (DAC_1);
   Reset (DAC_1);
   Select_Trigger (DAC_1, Output_Channel, Software_Trigger);
   Enable_Trigger (DAC_1, Output_Channel);
   Enable (DAC_1, Output_Channel);

   Toggle (Red);
   Next_Release := Next_Release + Period;
   delay until Next_Release;
   loop
      Toggle (Green);
      if STM32.Button.Has_Been_Pressed then
         Toggle (Red);
         count := count + 1;
         LCD_Std_Out.Clear_Screen;
         LCD_Std_Out.Put ("Presses:");
         LCD_Std_Out.Put (count'Image);

         Percent := K * 10;
         K := K + 1;

         Value := (Percent * Max_Counts) / 100;

         LCD_Std_Out.New_Line;
         LCD_Std_Out.Put (Value'Image);
         LCD_Std_Out.New_Line;
         LCD_Std_Out.Put (Percent'Image);

         Set_Output
          (DAC_1,
           Output_Channel,
           Value,
           Resolution,
           Right_Aligned);

         Trigger_Conversion_By_Software (DAC_1, Output_Channel);
      end if;


      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
exception
      when Constraint_Error =>
      LCD_Std_Out.Clear_Screen;
      LCD_Std_Out.Put ("An error occured!");
      loop
         Toggle (Red);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
end Blinky;
