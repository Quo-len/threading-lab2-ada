with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Main is

   task type SeekingForMinElement is
      pragma Storage_Size (1_000_000_000);
   end SeekingForMinElement;

   task body SeekingForMinElement is

      NumOfThreads  : constant Integer := 5;
      NumOfElements : constant Integer := 1_000_000;

      Arr : array (1 .. NumOfElements) of Integer;
      type RandRange is range 1 .. NumOfElements;

      procedure Init_Arr is
      begin
         for I in 1 .. NumOfElements loop
            Arr (I) := I;
         end loop;
      end Init_Arr;

      procedure Set_Random_Minimum is
         package Rand_Int is new Ada.Numerics.Discrete_Random (RandRange);
         use Rand_Int;
         Rand_Idx : Integer;
         Rand     : Generator;
      begin
         Reset (Rand);
         Rand_Idx       := Integer (Random (Rand));
         Arr (Rand_Idx) := -1;
         Put_Line
           ("New min set - idx:" & Rand_Idx'Img & " num:" &
            Arr (Rand_Idx)'Img);
      end Set_Random_Minimum;

      function Part_Sum (Start_Index, Finish_Index : in Integer) return Integer
      is
         CurrMinIdx : Integer := Start_Index;
      begin
         for I in Start_Index .. Finish_Index loop
            if Arr (I) < Arr (CurrMinIdx) then
               CurrMinIdx := I;
            end if;
         end loop;
         return CurrMinIdx;
      end Part_Sum;

      protected UpdateHandler is
         procedure SetMinIdx (FoundMinIdx : in Integer);
         entry GetMinIdx (Idx : out Integer);
      private
         MinIdx     : Integer := Arr'First;
         TasksCount : Integer := 0;
      end UpdateHandler;

      protected body UpdateHandler is
         procedure SetMinIdx (FoundMinIdx : in Integer) is
         begin
            if Arr (FoundMinIdx) < Arr (MinIdx) then
               MinIdx := FoundMinIdx;
            end if;
            TasksCount := TasksCount + 1;
         end SetMinIdx;

         entry GetMinIdx (Idx : out Integer) when TasksCount = NumOfThreads is
         begin
            Idx := MinIdx;
         end GetMinIdx;

      end UpdateHandler;

      task type Seeker is
         entry Start (Start_Index, Finish_Index : in Integer);
      end Seeker;

      task body Seeker is
         Start_Index, Finish_Index : Integer;
         FoundMinIdx               : Integer := 0;
      begin
         accept Start (Start_Index, Finish_Index : in Integer) do
            Seeker.Start_Index  := Start_Index;
            Seeker.Finish_Index := Finish_Index;
         end Start;
         FoundMinIdx :=
           Part_Sum (Start_Index => Start_Index, Finish_Index => Finish_Index);
         UpdateHandler.SetMinIdx (FoundMinIdx);
      end Seeker;

      type SeekerArr is array (Integer range <>) of Seeker;

      procedure Parallel_Sum is
         Step         : Integer := Arr'Length / NumOfThreads;
         Threads      : SeekerArr (1 .. NumOfThreads);
         Boundary     : Integer := Arr'First;
         ResultMinIdx : Integer := 0;
      begin
         for I in 1 .. (NumOfThreads - 1) loop
            Put_Line (I'Img);
            Put_Line
              ("L:" & Integer'Image (Boundary) & " R:" &
               Integer'Image (Boundary + Step));
            Threads (I).Start (Boundary, Boundary + Step);
            Boundary := Boundary + Step;
         end loop;
         Put_Line
           ("LL:" & Integer'Image (Boundary) & " RR:" &
            Integer'Image (NumOfElements));
         Threads (Threads'Last).Start (Boundary, NumOfElements);

         UpdateHandler.GetMinIdx (ResultMinIdx);

         Put_Line ("Minimal element in array - " & Arr (ResultMinIdx)'Img);
         Put_Line ("Index of minimal element in array - " & ResultMinIdx'Img);
      end Parallel_Sum;

   begin
      Init_Arr;
      Set_Random_Minimum;
      Parallel_Sum;

   end SeekingForMinElement;

   FindIt : SeekingForMinElement;

begin
   null;
end Main;
