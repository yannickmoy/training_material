package body Employee_Data is

   function Create
     (Name : String;
      Rate : Hourly_Rate_T)
      return Employee_T is
   begin
      -- Return an employee object with appropriate data
      return Employee_T'(others => <>);
   end Create;

   function Id
     (Employee : Employee_T)
      return Id_T is
   begin
      -- return ID member of record
      return Id_T'first;
   end Id;

   function Name
     (Employee : Employee_T)
      return String is
   begin
      -- return Name member of record
      return "";
   end Name;

   function Rate
     (Employee : Employee_T)
      return Hourly_Rate_T is
   begin
      -- return Rate member of record;
      return Hourly_Rate_T'first;
   end Rate;

end Employee_Data;
