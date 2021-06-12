--------------------------------------------------------------------------------
--                              Compressor_23_3
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_23_3 is
   port ( X0 : in  std_logic_vector(2 downto 0);
          X1 : in  std_logic_vector(1 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_23_3 is
signal X :  std_logic_vector(4 downto 0);
begin
   X <=X1 & X0 ;
   with X select R <= 
      "000" when "00000", 
      "001" when "00001", 
      "001" when "00010", 
      "010" when "00011", 
      "001" when "00100", 
      "010" when "00101", 
      "010" when "00110", 
      "011" when "00111", 
      "010" when "01000", 
      "011" when "01001", 
      "011" when "01010", 
      "100" when "01011", 
      "011" when "01100", 
      "100" when "01101", 
      "100" when "01110", 
      "101" when "01111", 
      "010" when "10000", 
      "011" when "10001", 
      "011" when "10010", 
      "100" when "10011", 
      "011" when "10100", 
      "100" when "10101", 
      "100" when "10110", 
      "101" when "10111", 
      "100" when "11000", 
      "101" when "11001", 
      "101" when "11010", 
      "110" when "11011", 
      "101" when "11100", 
      "110" when "11101", 
      "110" when "11110", 
      "111" when "11111", 
      "---" when others;

end architecture;

--------------------------------------------------------------------------------
--                               Compressor_3_2
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_3_2 is
   port ( X0 : in  std_logic_vector(2 downto 0);
          R : out  std_logic_vector(1 downto 0)   );
end entity;

architecture arch of Compressor_3_2 is
signal X :  std_logic_vector(2 downto 0);
begin
   X <=X0 ;
   with X select R <= 
      "00" when "000", 
      "01" when "001", 
      "01" when "010", 
      "10" when "011", 
      "01" when "100", 
      "10" when "101", 
      "10" when "110", 
      "11" when "111", 
      "--" when others;

end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid50
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid50 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid50 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "010000" when "100100",
   "010100" when "100101",
   "011000" when "100110",
   "011100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "010100" when "101100",
   "011001" when "101101",
   "011110" when "101110",
   "100011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "011000" when "110100",
   "011110" when "110101",
   "100100" when "110110",
   "101010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "011100" when "111100",
   "100011" when "111101",
   "101010" when "111110",
   "110001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYu_F400_uid52
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid52 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid52 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "110000" when "100100",
   "110100" when "100101",
   "111000" when "100110",
   "111100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "101100" when "101100",
   "110001" when "101101",
   "110110" when "101110",
   "111011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "101000" when "110100",
   "101110" when "110101",
   "110100" when "110110",
   "111010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "100100" when "111100",
   "101011" when "111101",
   "110010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYs_F400_uid54
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid54 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid54 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "110000" when "100100",
   "101100" when "100101",
   "101000" when "100110",
   "100100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "110100" when "101100",
   "110001" when "101101",
   "101110" when "101110",
   "101011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "111000" when "110100",
   "110110" when "110101",
   "110100" when "110110",
   "110010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "111100" when "111100",
   "111011" when "111101",
   "111010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYs_F400_uid56
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid56 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid56 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "010000" when "100100",
   "001100" when "100101",
   "001000" when "100110",
   "000100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "001100" when "101100",
   "001001" when "101101",
   "000110" when "101110",
   "000011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "001000" when "110100",
   "000110" when "110101",
   "000100" when "110110",
   "000010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "000100" when "111100",
   "000011" when "111101",
   "000010" when "111110",
   "000001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid65
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid65 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid65 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "010000" when "100100",
   "010100" when "100101",
   "011000" when "100110",
   "011100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "010100" when "101100",
   "011001" when "101101",
   "011110" when "101110",
   "100011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "011000" when "110100",
   "011110" when "110101",
   "100100" when "110110",
   "101010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "011100" when "111100",
   "100011" when "111101",
   "101010" when "111110",
   "110001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYu_F400_uid67
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid67 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid67 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "110000" when "100100",
   "110100" when "100101",
   "111000" when "100110",
   "111100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "101100" when "101100",
   "110001" when "101101",
   "110110" when "101110",
   "111011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "101000" when "110100",
   "101110" when "110101",
   "110100" when "110110",
   "111010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "100100" when "111100",
   "101011" when "111101",
   "110010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYs_F400_uid69
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid69 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid69 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "110000" when "100100",
   "101100" when "100101",
   "101000" when "100110",
   "100100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "110100" when "101100",
   "110001" when "101101",
   "101110" when "101110",
   "101011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "111000" when "110100",
   "110110" when "110101",
   "110100" when "110110",
   "110010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "111100" when "111100",
   "111011" when "111101",
   "111010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYs_F400_uid71
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid71 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid71 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "010000" when "100100",
   "001100" when "100101",
   "001000" when "100110",
   "000100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "001100" when "101100",
   "001001" when "101101",
   "000110" when "101110",
   "000011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "001000" when "110100",
   "000110" when "110101",
   "000100" when "110110",
   "000010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "000100" when "111100",
   "000011" when "111101",
   "000010" when "111110",
   "000001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                               Compressor_6_3
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_6_3 is
   port ( X0 : in  std_logic_vector(5 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_6_3 is
signal X :  std_logic_vector(5 downto 0);
begin
   X <=X0 ;
   with X select R <= 
      "000" when "000000", 
      "001" when "000001", 
      "001" when "000010", 
      "010" when "000011", 
      "001" when "000100", 
      "010" when "000101", 
      "010" when "000110", 
      "011" when "000111", 
      "001" when "001000", 
      "010" when "001001", 
      "010" when "001010", 
      "011" when "001011", 
      "010" when "001100", 
      "011" when "001101", 
      "011" when "001110", 
      "100" when "001111", 
      "001" when "010000", 
      "010" when "010001", 
      "010" when "010010", 
      "011" when "010011", 
      "010" when "010100", 
      "011" when "010101", 
      "011" when "010110", 
      "100" when "010111", 
      "010" when "011000", 
      "011" when "011001", 
      "011" when "011010", 
      "100" when "011011", 
      "011" when "011100", 
      "100" when "011101", 
      "100" when "011110", 
      "101" when "011111", 
      "001" when "100000", 
      "010" when "100001", 
      "010" when "100010", 
      "011" when "100011", 
      "010" when "100100", 
      "011" when "100101", 
      "011" when "100110", 
      "100" when "100111", 
      "010" when "101000", 
      "011" when "101001", 
      "011" when "101010", 
      "100" when "101011", 
      "011" when "101100", 
      "100" when "101101", 
      "100" when "101110", 
      "101" when "101111", 
      "010" when "110000", 
      "011" when "110001", 
      "011" when "110010", 
      "100" when "110011", 
      "011" when "110100", 
      "100" when "110101", 
      "100" when "110110", 
      "101" when "110111", 
      "011" when "111000", 
      "100" when "111001", 
      "100" when "111010", 
      "101" when "111011", 
      "100" when "111100", 
      "101" when "111101", 
      "101" when "111110", 
      "110" when "111111", 
      "---" when others;

end architecture;

--------------------------------------------------------------------------------
--                              Compressor_14_3
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_14_3 is
   port ( X0 : in  std_logic_vector(3 downto 0);
          X1 : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_14_3 is
signal X :  std_logic_vector(4 downto 0);
begin
   X <=X1 & X0 ;
   with X select R <= 
      "000" when "00000", 
      "001" when "00001", 
      "001" when "00010", 
      "010" when "00011", 
      "001" when "00100", 
      "010" when "00101", 
      "010" when "00110", 
      "011" when "00111", 
      "001" when "01000", 
      "010" when "01001", 
      "010" when "01010", 
      "011" when "01011", 
      "010" when "01100", 
      "011" when "01101", 
      "011" when "01110", 
      "100" when "01111", 
      "010" when "10000", 
      "011" when "10001", 
      "011" when "10010", 
      "100" when "10011", 
      "011" when "10100", 
      "100" when "10101", 
      "100" when "10110", 
      "101" when "10111", 
      "011" when "11000", 
      "100" when "11001", 
      "100" when "11010", 
      "101" when "11011", 
      "100" when "11100", 
      "101" when "11101", 
      "101" when "11110", 
      "110" when "11111", 
      "---" when others;

end architecture;

--------------------------------------------------------------------------------
--                              Compressor_13_3
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_13_3 is
   port ( X0 : in  std_logic_vector(2 downto 0);
          X1 : in  std_logic_vector(0 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_13_3 is
signal X :  std_logic_vector(3 downto 0);
begin
   X <=X1 & X0 ;
   with X select R <= 
      "000" when "0000", 
      "001" when "0001", 
      "001" when "0010", 
      "010" when "0011", 
      "001" when "0100", 
      "010" when "0101", 
      "010" when "0110", 
      "011" when "0111", 
      "010" when "1000", 
      "011" when "1001", 
      "011" when "1010", 
      "100" when "1011", 
      "011" when "1100", 
      "100" when "1101", 
      "100" when "1110", 
      "101" when "1111", 
      "---" when others;

end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid251
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid251 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid251 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "010000" when "100100",
   "010100" when "100101",
   "011000" when "100110",
   "011100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "010100" when "101100",
   "011001" when "101101",
   "011110" when "101110",
   "100011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "011000" when "110100",
   "011110" when "110101",
   "100100" when "110110",
   "101010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "011100" when "111100",
   "100011" when "111101",
   "101010" when "111110",
   "110001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYu_F400_uid253
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid253 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid253 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "110000" when "100100",
   "110100" when "100101",
   "111000" when "100110",
   "111100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "101100" when "101100",
   "110001" when "101101",
   "110110" when "101110",
   "111011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "101000" when "110100",
   "101110" when "110101",
   "110100" when "110110",
   "111010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "100100" when "111100",
   "101011" when "111101",
   "110010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYs_F400_uid255
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid255 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid255 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "110000" when "100100",
   "101100" when "100101",
   "101000" when "100110",
   "100100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "110100" when "101100",
   "110001" when "101101",
   "101110" when "101110",
   "101011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "111000" when "110100",
   "110110" when "110101",
   "110100" when "110110",
   "110010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "111100" when "111100",
   "111011" when "111101",
   "111010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYs_F400_uid257
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid257 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid257 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "010000" when "100100",
   "001100" when "100101",
   "001000" when "100110",
   "000100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "001100" when "101100",
   "001001" when "101101",
   "000110" when "101110",
   "000011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "001000" when "110100",
   "000110" when "110101",
   "000100" when "110110",
   "000010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "000100" when "111100",
   "000011" when "111101",
   "000010" when "111110",
   "000001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid262
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid262 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid262 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "010000" when "100100",
   "010100" when "100101",
   "011000" when "100110",
   "011100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "010100" when "101100",
   "011001" when "101101",
   "011110" when "101110",
   "100011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "011000" when "110100",
   "011110" when "110101",
   "100100" when "110110",
   "101010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "011100" when "111100",
   "100011" when "111101",
   "101010" when "111110",
   "110001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYu_F400_uid264
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid264 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid264 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "110000" when "100100",
   "110100" when "100101",
   "111000" when "100110",
   "111100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "101100" when "101100",
   "110001" when "101101",
   "110110" when "101110",
   "111011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "101000" when "110100",
   "101110" when "110101",
   "110100" when "110110",
   "111010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "100100" when "111100",
   "101011" when "111101",
   "110010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYs_F400_uid266
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid266 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid266 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "110000" when "100100",
   "101100" when "100101",
   "101000" when "100110",
   "100100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "110100" when "101100",
   "110001" when "101101",
   "101110" when "101110",
   "101011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "111000" when "110100",
   "110110" when "110101",
   "110100" when "110110",
   "110010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "111100" when "111100",
   "111011" when "111101",
   "111010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYs_F400_uid268
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid268 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid268 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "010000" when "100100",
   "001100" when "100101",
   "001000" when "100110",
   "000100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "001100" when "101100",
   "001001" when "101101",
   "000110" when "101110",
   "000011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "001000" when "110100",
   "000110" when "110101",
   "000100" when "110110",
   "000010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "000100" when "111100",
   "000011" when "111101",
   "000010" when "111110",
   "000001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid408
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid408 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid408 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "010000" when "100100",
   "010100" when "100101",
   "011000" when "100110",
   "011100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "010100" when "101100",
   "011001" when "101101",
   "011110" when "101110",
   "100011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "011000" when "110100",
   "011110" when "110101",
   "100100" when "110110",
   "101010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "011100" when "111100",
   "100011" when "111101",
   "101010" when "111110",
   "110001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYu_F400_uid410
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid410 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid410 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "000100" when "100001",
   "001000" when "100010",
   "001100" when "100011",
   "110000" when "100100",
   "110100" when "100101",
   "111000" when "100110",
   "111100" when "100111",
   "000000" when "101000",
   "000101" when "101001",
   "001010" when "101010",
   "001111" when "101011",
   "101100" when "101100",
   "110001" when "101101",
   "110110" when "101110",
   "111011" when "101111",
   "000000" when "110000",
   "000110" when "110001",
   "001100" when "110010",
   "010010" when "110011",
   "101000" when "110100",
   "101110" when "110101",
   "110100" when "110110",
   "111010" when "110111",
   "000000" when "111000",
   "000111" when "111001",
   "001110" when "111010",
   "010101" when "111011",
   "100100" when "111100",
   "101011" when "111101",
   "110010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYs_F400_uid412
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid412 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid412 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "000100" when "001100",
   "000101" when "001101",
   "000110" when "001110",
   "000111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "001000" when "010100",
   "001010" when "010101",
   "001100" when "010110",
   "001110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "001100" when "011100",
   "001111" when "011101",
   "010010" when "011110",
   "010101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "110000" when "100100",
   "101100" when "100101",
   "101000" when "100110",
   "100100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "110100" when "101100",
   "110001" when "101101",
   "101110" when "101110",
   "101011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "111000" when "110100",
   "110110" when "110101",
   "110100" when "110110",
   "110010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "111100" when "111100",
   "111011" when "111101",
   "111010" when "111110",
   "111001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XsYs_F400_uid414
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid414 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid414 is
signal TableOut :  std_logic_vector(5 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "000000" when "000000",
   "000000" when "000001",
   "000000" when "000010",
   "000000" when "000011",
   "000000" when "000100",
   "000000" when "000101",
   "000000" when "000110",
   "000000" when "000111",
   "000000" when "001000",
   "000001" when "001001",
   "000010" when "001010",
   "000011" when "001011",
   "111100" when "001100",
   "111101" when "001101",
   "111110" when "001110",
   "111111" when "001111",
   "000000" when "010000",
   "000010" when "010001",
   "000100" when "010010",
   "000110" when "010011",
   "111000" when "010100",
   "111010" when "010101",
   "111100" when "010110",
   "111110" when "010111",
   "000000" when "011000",
   "000011" when "011001",
   "000110" when "011010",
   "001001" when "011011",
   "110100" when "011100",
   "110111" when "011101",
   "111010" when "011110",
   "111101" when "011111",
   "000000" when "100000",
   "111100" when "100001",
   "111000" when "100010",
   "110100" when "100011",
   "010000" when "100100",
   "001100" when "100101",
   "001000" when "100110",
   "000100" when "100111",
   "000000" when "101000",
   "111101" when "101001",
   "111010" when "101010",
   "110111" when "101011",
   "001100" when "101100",
   "001001" when "101101",
   "000110" when "101110",
   "000011" when "101111",
   "000000" when "110000",
   "111110" when "110001",
   "111100" when "110010",
   "111010" when "110011",
   "001000" when "110100",
   "000110" when "110101",
   "000100" when "110110",
   "000010" when "110111",
   "000000" when "111000",
   "111111" when "111001",
   "111110" when "111010",
   "111101" when "111011",
   "000100" when "111100",
   "000011" when "111101",
   "000010" when "111110",
   "000001" when "111111",
   "------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                               Compressor_4_3
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Popa, Illyes Kinga, 2012
--------------------------------------------------------------------------------
-- combinatorial

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity Compressor_4_3 is
   port ( X0 : in  std_logic_vector(3 downto 0);
          R : out  std_logic_vector(2 downto 0)   );
end entity;

architecture arch of Compressor_4_3 is
signal X :  std_logic_vector(3 downto 0);
begin
   X <=X0 ;
   with X select R <= 
      "000" when "0000", 
      "001" when "0001", 
      "001" when "0010", 
      "010" when "0011", 
      "001" when "0100", 
      "010" when "0101", 
      "010" when "0110", 
      "011" when "0111", 
      "001" when "1000", 
      "010" when "1001", 
      "010" when "1010", 
      "011" when "1011", 
      "010" when "1100", 
      "011" when "1101", 
      "011" when "1110", 
      "100" when "1111", 
      "---" when others;

end architecture;

--------------------------------------------------------------------------------
--                        GenericTable_8_184_F400_uid4
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_8_184_F400_uid4 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(7 downto 0);
          Y : out  std_logic_vector(183 downto 0)   );
end entity;

architecture arch of GenericTable_8_184_F400_uid4 is
signal TableOut, TableOut_d1 :  std_logic_vector(183 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            TableOut_d1 <=  TableOut;
         end if;
      end process;
  with X select TableOut <= 
   "1000000000011111111111000000000011111111101100000010000000111111111100000000010111111101100000010000011000000000010111111100010000100010111011111111011000001001110100001100001000101110" when "00000000",
   "1000000001011111110111000001101011100110110010100111101001111111110100000011010110111100110110000001000000000001000111011110011110101010110101111110001001001111001000001100011001111010" when "00000001",
   "1000000010011111100111000111110000111110000000110101010000111111101100001001010011001010001001011001001100000001110110100011010011100111011101111100111011011000101000101100101010110100" when "00000010",
   "1000000011011111001111010101010000011000110010111010000111111111100100010010001010110000101000100001111000000010100101001011001000101111001011111011101110100101001001001100111011011000" when "00000011",
   "1000000100011110101111101101000100010110100101101000001110111111011100011101111011111101000110000100110110000011010011010110010110111011111111111010100010110011100001001101001011110000" when "00000100",
   "1000000101011110001000010010000101100100011001111101011111111111010100101100100100111101110111011101111110000100000001000101010110101101001001111001011000000010100110101101011011101111" when "00000101",
   "1000000110011101011001000111001010111110010110111111011101111111001100111110000100000010110011001110101000000100101110011000100000000110001101111000001110010001010100001101101011100100" when "00000110",
   "1000000111011100100010001111001001110001001010001011011011111111000101010010010111011101001111010011100110000101011011010000001010110001011001110111000101011110100011001101111011000001" when "00000111",
   "1000001000011011100011101100110101011011100101111101110001111110111101101001011101011111111111011100111100000110000111101100101101111110010101110101111101101001010000101110001010010010" when "00001000",
   "1000001001011010011101100010111111101111111110110010000011111110110110000011010100011111010011101000010000000110110011101110100000100100000101110100110110110000011010101110011001010000" when "00001001",
   "1000001010011001001111110100011000110101100110011110100011111110101110011111111010110000110110011100111010000111011111010101111001000000101101110011110000110010111110101110100111111110" when "00001010",
   "1000001011010111111010100011101111001010000110001100111111111110100110111111001110101011101011101010010100001000001010100011001101011010000101110010101011101111111100101110110110011111" when "00001011",
   "1000001100010110011101110011101111100010110110110010100010111110011111100001001110101000001110101000001110001000110101010110110011011110101101110001100111100110010111101111000100101101" when "00001100",
   "1000001101010100111001100111000101001110010111101001001110111110011000000101111001000000010000111001000010001001011111110001000000100101100011110000100100010101010010001111010010101101" when "00001101",
   "1000001110010011001110000000011001110101100100001100011110111110010000101101001100001110111000101110000010001010001001110010001001101111000001101111100001111011101111001111100000011101" when "00001110",
   "1000001111010001011011000010010101011101000111111010110001111110001001010111000110110000011111101101001110001010110011011010100011100101001101101110100000011000110101101111101110000000" when "00001111",
   "1000010000001111100000101111011110100110110000111110010100111110000010000011100111000010110001011001011110001011011100101010100010011100011011101101011111101011101010001111111011010110" when "00010000",
   "1000010001001101011111001010011010010010100001011110011000111101111010110010101011100100101001111011110110001100000101100010011010010011101011101100011111110011010101010000001000011011" when "00010001",
   "1000010010001011010110010101101011111111111111011011001011111101110011100100010010110110010100101111001010001100101110000010011110110100110011101011100000101110111111010000010101010110" when "00010010",
   "1000010011001001000110010011110101101111100011010101111011111101101100011000011011011001001011001100111110001101010110001011000011010101010111101010100010011101110001110000100010000011" when "00010011",
   "1000010100000110101111000111011000000011100101100110110101111101100101001111000011101111110011011100001110001101111101111100011010110110101111101001100100111110111000110000101110100001" when "00010100",
   "1000010101000100010000110010110010000001101010100010100010111101011110001000001010011101111111000001101110001110100101010110111000000110100011101000101000010001011111010000111010110000" when "00010101",
   "1000010110000001101011011000100001010011101101010000101110111101010111000011101110001000101001110001110110001111001100011010101101011110111001100111101100010100110001110001000110110110" when "00010110",
   "1000010110111110111110111011000010001001001001010101010011111101010000000001101101010101111000100100000010001111110011001000001101000111001111100110110001001000000000010001010010101110" when "00010111",
   "1000010111111100001011011100101111011000000011001101111000111101001001000010000110101100111000000111110110010000011001011111101000110100001101100101110110101010010111010001011110011101" when "00011000",
   "1000011000111001010001000000000010011110001111100100110011111101000010000100111000110101111011111011001010010000111111100001010010001000100101100100111100111011000111010001101001111010" when "00011001",
   "1000011001110110001111100111010011100010011001011011011011111100111011001010000010011010011101000010000100010001100101001101011010010100110001100100000011111001100010010001110101001111" when "00011010",
   "1000011010110011000111010100111001010101000111001100110100111100110100010001100010000100111001000000010000010010001010100100010010010111111011100011001011100100111000110010000000011010" when "00011011",
   "1000011011101111111000001011001001010001111110101010011011111100101101011011010110100000110000110011011000010010101111100110001011000000000111100010010011111100011110010010001011010111" when "00011100",
   "1000011100101100100010001100010111100000100111110100000001111100100110100111011110011010100111101111001010010011010100010011010100101010011101100001011100111111100110110010010110001001" when "00011101",
   "1000011101101001000101011010110110110101101110101100010010111100011111110101111000100000000010011010101010010011111000101011111111100011011001100000100110101101100100110010100000110010" when "00011110",
   "1000011110100101100001111000111000110100000100001011001100111100011001000110100011011111100101101110101010010100011100110000011011100111010111011111110001000101101110110010101011010001" when "00011111",
   "1000011111100001110111101000101101101101011101101111100111111100010010011001011110001000110101110101100100010101000000100000111000100010110111011110111100000111011001110010110101100101" when "00100000",
   "1000100000011110000110101100100100100011110100010001010011111100001011101110100111001100010101001100010110010101100011111101100101110010110011011110000111110001111101010010111111101101" when "00100001",
   "1000100001011010001111000110101011001010000001110100011110111100000101000101111101011011100011100100101110010110000111000110110010100100101011011101010100000100110001010011001001101101" when "00100010",
   "1000100010010110010000111001001110000100111110100000001010111011111110011111011111101000111101001000100100010110101001111100101101110111001001011100100000111111001101110011010011011111" when "00100011",
   "1000100011010010001100000110011000101100011100011000100000111011110111111011001100100111111001011110011000010111001100011111100110011001110011011011101110100000101100010011011101001101" when "00100100",
   "1000100100001110000000110000010101001100000010011110000110111011110001011001000011001100101010101110111110010111101110101111101010101110001011011010111100101000100101010011100110101110" when "00100101",
   "1000100101001001101110111001001100100100000110110011011110111011101010111001000010001100011100101100000100011000010000101101001001000111011011011010001011010110010011010011110000000110" when "00100110",
   "1000100110000101010110100011000110101010100111101001111011111011100100011011001000011101010011111000000000011000110010011000001111101010101101011001011010101001010011110011111001010110" when "00100111",
   "1000100111000000110111110000001010001100000011110110011111111011011101111111010100110110001100101110111010011001010011110001001100001111101101011000101010100000111111110100000010011100" when "00101000",
   "1000100111111100010010100010011100101100010010010000011010111011010111100101100110001110111010110000000110011001110100111000001100100000011011010111111010111100110101110100001011011100" when "00101001",
   "1000101000110111100110111100000010100111011000011001110010111011010001001101111011100000000111101001001100011010010101101101011101111010000011010111001011111100010010110100010100010000" when "00101010",
   "1000101001110010110100111110111111010010100000010011101100111011001010111000010011100011010010100001111010011010110110010001001101101100011101010110011101011110110011110100011101000000" when "00101011",
   "1000101010101101111100101101010100111100101101011110100000111011000100100100101101010010101111001000101010011011010110100011101000111011001111010101101111100011111000110100100101100011" when "00101100",
   "1000101011101000111110001001000100101111110001000111100000111010111110010011000111101001100101000000100100011011110110100100111100011101001011010101000010001010111110010100101101111111" when "00101101",
   "1000101100100011111001010100001110110000111101100100101001111010111000000011100001100011101110101111101100011100010110010101010100111100111011010100010101010011100110010100110110010101" when "00101110",
   "1000101101011110101110010000110010000001111000111111010110111010110001110101111001111101111001001110111000011100110101110100111110111001010101010011101000111101010000110100111110100001" when "00101111",
   "1000101110011001011101000000101100100001001111001111010111111010101011101010001111110101100010111010000100011101010101000100000110100101001011010010111101000111011101010101000110100100" when "00110000",
   "1000101111010100000101100101111011001011100011000110010100111010100101100000100010001000111011000001011010011101110100000010111000000111101111010010010001110001101110010101001110100001" when "00110001",
   "1000110000001110101000000010011001111011111110101101000010111010011111011000101111110111000000111011100000011110010010110001011111011100111111010001100110111011100110010101010110010110" when "00110010",
   "1000110001001001000100011000000011101101000011010011010110111010011001010010110111111111100011011000010100011110110001010000001000010101100011010000111100100100100101110101011110000111" when "00110011",
   "1000110010000011011010101000110010011001011000010011001000111010010011001110111001100010111111110100110000011111001111011110111110010111011111010000010010101100010001110101100101101101" when "00110100",
   "1000110010111101101010110110011110111100011001100111101000111010001101001100110011100010100001101111011000011111101101011110001100111101100011001111101001010010001011110101101101010000" when "00110101",
   "1000110011110111110101000011000001010011000101011001011111111010000111001100100101000000000001111101011010100000001011001101111111011000011001001111000000010101111010010101110100101000" when "00110110",
   "1000110100110001111001010000010000011100101001000000100101111010000001001110001100111110000110000001000110100000101000101110100000101101111101001110010111110110111111110101111011111000" when "00110111",
   "1000110101101011110111100000000010011011001101011100001010111001111011010001101010011111111111100000001010100001000101111111111011111001111001001101101111110101000010010110000011000110" when "00111000",
   "1000110110100101101111110100001100010100100011000010001000111001110101010110111100101001101011011011100010100001100011000010011011101110001111001101001000001111100110110110001010001001" when "00111001",
   "1000110111011111100010001110100010010010101100100110010001111001101111011110000010011111110001100111011100100001111111110110001010110010101101001100100001000110010010110110010001001000" when "00111010",
   "1000111000011001001110110000110111100100101001111001111001111001101001100110111011000111100100000100001110100010011100011011010011100101100011001011111010011000101110110110011000000010" when "00111011",
   "1000111001010010110101011100111110011111000001100100110100111001100011110001100101100110111110011000001110100010111000110010000000011011100011001011010100000110011110110110011110110101" when "00111100",
   "1000111010001100010110010100101000011100101010011000001101111001011101111110000001000100100101001001100110100011010100111010011011100000000011001010101110001111001100010110100101100001" when "00111101",
   "1000111011000101110001011001100101111111010011111100000011111001011000001100001100100111100101011001100110100011110000110100101110110101000111001010001000110010011101110110101100000111" when "00111110",
   "1000111011111111000110101101100110110000001110110111101110111001010010011100000111010111110011111111111000100100001100100001000100010011111111001001100011101111111011110110110010100110" when "00111111",
   "1000111100111000010110010010011001100000110100010110100110111001001100101101110000011101101101000110101110100100100111111111100101101100101011001000111111000111001110010110111001000011" when "01000000",
   "1000111101110001100000001001101100001011001101001000111001111001000111000001000111000010010011100111110110100101000011010000011100100110111101001000011010110111111110010110111111010110" when "01000001",
   "1000111110101010100100010101001011110010111000000001110110111001000001010110001010001111010000101001101100100101011110010011110010100001011111000111110111000001110110010111000101100110" when "01000010",
   "1000111111100011100010110110100100100101001111110011010010111000111011101100111001001110110010111101100000100101111001001001110000110011000001000111010011100100011111010111001011101110" when "01000011",
   "1001000000011100011011101111100001111010010000100111110001111000110110000101010011001011101110011101111000100110010011110010100000101001011011000110110000011111100001110111010001110100" when "01000100",
   "1001000001010101001111000001101110010100111100111011001110111000110000011111010111010001011011101101101000100110101110001110001011001010111111000110001101110010101011010111010111110011" when "01000101",
   "1001000010001101111100101110110011100100000001110011001111111000101010111011000100101011110111010111110100100111001000011100111001010101101111000101101011011101100100010111011101101001" when "01000110",
   "1001000011000110100100111000011010100010011010110111001011111000100101011000011010100111100001101111101100100111100010011110110011111111100001000101001001011111111000010111100011100001" when "01000111",
   "1001000011111111000111100000001011010111110101101000101100111000011111110111011000010001011110010001100010100111111100010100000011110111001001000100100111111001010011010111101001001101" when "01001000",
   "1001000100110111100100100111101101011001010100011101011110111000011010010111111100110111010011000011111000101000010101111100110001100010111111000100000110101001100000110111101110111000" when "01001001",
   "1001000101101111111100010000100111001001110000111010001100111000010100111010000111100111001000011001010100101000101111011001000101100010100101000011100101110000001101010111110100011101" when "01001010",
   "1001000110101000001110011100011110011010011101101111110010111000001111011101110111101111101000010010110000101001001000101001001000001101101101000011000101001101000101010111111001111110" when "01001011",
   "1001000111100000011011001100111000001011101000011011000010111000001010000011001100011111111110000010001110101001100001101101000001110101000111000010100100111111110101010111111111011001" when "01001100",
   "1001001000011000100010100011011000101100111010000111010011111000000100101010000101000111110101101110000000101001111010100100111010100010001101000010000101001000001011111000000100110001" when "01001101",
   "1001001001010000100100100001100011011101111000010100101000110111111111010010100000110111011011110100100110101010010011010000111010010111100101000001100101100101110011111000001010000101" when "01001110",
   "1001001010001000100001001000111011001110100101000001111001110111111001111100011110111111011100110000100110101010101011110001001001010000111101000001000110011000011100111000001111010010" when "01001111",
   "1001001011000000011000011011000001111111111110011011010111110111110100100111111110110001000100011101100110101011000100000101101111000011000101000000100111011111110011111000010100011001" when "01010000",
   "1001001011111000001010011001011001000100011110001110000011110111101111010100111111011101111101111101000100101011011100001110110011011011111111000000001000111011101000011000011001100001" when "01010001",
   "1001001100101111110111000101100001000000011000100000011010110111101010000011100000011000010010111100000100101011110100001100011110000011100100111111101010101011101000011000011110011111" when "01010010",
   "1001001101100111011110100000111001101010011010010000110111110111100100110011100000110010101011011001000110101100001011111110110110011010101100111111001100101111100010111000100011011101" when "01010011",
   "1001001110011111000000101101000010001100000111011010010101110111011111100101000000000000001101001010011110101100100011100110000011111100010000111110101111000111000111011000101000010111" when "01010100",
   "1001001111010110011101101011011001000010011000011111011101110111011010010111111101010100011011100101001000101100111011000010001101111100111010111110010001110010000101111000101101001001" when "01010101",
   "1001010000001101110101011101011011111101110111111100110011110111010101001100011000000011010111000011111010101101010010010011011011101010111110111101110100110000001011111000110001111000" when "01010110",
   "1001010001000101001000000100101000000011011111000010011101110111010000000010001111100001011100101110111110101101101001011001110100001110101010111101011000000001001010111000110110100100" when "01010111",
   "1001010001111100010101100010011001101100110010010101010110110111001010111001100011000011100110000011110110101110000000010101011110101010010100111100111011100100110011111000111011010001" when "01011000",
   "1001010010110011011101111000001100101000011101111000110011110111000101110010010001111111001000011101111010101110010111000110100001111010110000111100011111011010110110011000111111110100" when "01011001",
   "1001010011101010100001000111011011111010110001000000101011110111000000101100011011101001110100111110111110101110101101101101000100110110100010111100000011100011000011011001000100010100" when "01011010",
   "1001010100100001011111010001100001111101111001101100010110110110111011100111111111011001110111111000100000101111000100001001001110001110101110111011100111111101001011011001001000110001" when "01011011",
   "1001010101011000011000010111111000100010011111101011000001110110110110100100111100100101111000010101000000101111011010011011000100101110111000111011001100101000111111011001001101001000" when "01011100",
   "1001010110001111001100011011111000101111111111001001101101110110110001100011010010100100111000000001110110101111110000100010101110111100110100111010110001100110010010011001010001011111" when "01011101",
   "1001010111000101111011011110111011000101000011001011001001110110101100100011000000101110010010111001100010110000000110100000010011011001010000111010010110110100110100111001010101110001" when "01011110",
   "1001010111111100100101100010010111010111111111101010001000110110100111100100000110011001111110101110001000110000011100010011111000011111011010111001111100010100011000111001011001111110" when "01011111",
   "1001011000110011001010100111100100110111001011000110011111110110100010100110100011000000001010110100001000110000110001111101100100100101001000111001100010000100110000011001011110001100" when "01100000",
   "1001011001101001101010101111111010001001010111111101000011110110011101101010010101111001011111101101101010110001000111011101011101111011011110111001001000000101101101011001100010010011" when "01100001",
   "1001011010100000000101111100101101001110001101101011000010110110011000101111011110011110111110110101111000110001011100110011101010101110000010111000101110010111000100011001100110011001" when "01100010",
   "1001011011010110011100001111010011011110100001011101000001110110010011110101111100001010000010001101000010110001110010000000010001000011101010111000010100111000100101111001101010011000" when "01100011",
   "1001011100001100101101101001000001101100101110101001111011110110001110111101101110010100011100000100010010110010000111000011010110111101111110110111111011101010000110011001101110011000" when "01100100",
   "1001011101000010111010001011001100000101001110111010001111110110001010000110110100011000010110101010011110110010011011111101000010011010001110110111100010101011010110111001110010010000" when "01100101",
   "1001011101111001000001110111000110001110110001111011111101110110000101010001001101110000010011111000011100110010110000101101011001010000010100110111001001111100001101111001110110000111" when "01100110",
   "1001011110101111000100101110000011001010110101000011001110110110000000011100111001110111001100111110100010110011000101010100100001010011101110110110110001011100011100011001111001111110" when "01100111",
   "1001011111100101000010110001010101010101111010011000010000110101111011101001111000001000010010010001100010110011011001110010100000010011100000110110011001001011110111111001111101101111" when "01101000",
   "1001100000011010111100000010001110100111111111110010110010110101110110111000000111111111001010111000100110110011101110000111011011111001110010110110000001001010010001111010000001011101" when "01101001",
   "1001100001010000110000100010000000010100110101100011001000110101110010000111101000110111110100011010101110110100000010010011011001101100010000110101101001010111100001111010000101001011" when "01101010",
   "1001100010000110100000010001111011001100010100101001010011110101101101011000011010001110100010101101011100110100010110010110011111001100100100110101010001110011011000111010001000110001" when "01101011",
   "1001100010111100001011010011001111011010110100111010100011110101101000101010011011011111111111100010101110110100101010010000110001110111011100110100111010011101101100111010001100010111" when "01101100",
   "1001100011110001110001100111001100101001100010110101010010110101100011111101101100001001001010011000000010110100111110000010010111000101101010110100100011010110010010111010001111111010" when "01101101",
   "1001100100100111010011001111000001111110110101000011111011110101011111010010001011100111011000000101000110110101010001101011010100001011101010110100001100011100111111111010010011011100" when "01101110",
   "1001100101011100110000001011111101111110100001101110110010110101011010100111111001011000010010101011010010110101100101001011101110011001111100110011110101110001100111011010010110110111" when "01101111",
   "1001100110010010001000011111001110101010010011011101011001110101010101111110110100111001111001000100111000110101111000100011101010111100011100110011011111010011111110111010011010010011" when "01110000",
   "1001100111000111011100001010000001100001111110000111001111110101010001010110111101101010011110110100110010110110001011110011001110111011011100110011001001000011111101011010011101101011" when "01110001",
   "1001100111111100101011001101100011100011110011010100011100110101001100110000010011001000101011110110110000110110011110111010011111011011000100110010110011000001010101111010100001000011" when "01110010",
   "1001101000110001110101101011000001001100110110101110100000110101001000001010110100110011011100001111001100110110110001111001100001011011101110110010011101001011111111111010100100010100" when "01110011",
   "1001101001100110111011100011100110011001010010000001010101110101000011100110100010001001111111111011110110110111000100110000011001111001100110110010000111100011110000011010100111100101" when "01110100",
   "1001101010011011111100111000011110100100101000101100101111110100111111000011011010101011111010100100010110110111010111011111001101101101100010110001110010001000011101011010101010101111" when "01110101",
   "1001101011010000111001101010110100101010001011100110111000110100111010100001011101111001000011001011001000110111101010000110000001101011111110110001011100111001111100111010101101111101" when "01110110",
   "1001101100000101110001111011110011000101001100001111011100110100110110000000101011010001100011111110011110110111111100100100111010100110010010110001000111111000000100111010110001000110" when "01110111",
   "1001101100111010100101101100100011110001001111110100001110110100110001100001000010010101111010001001110110111000001110111011111101001001111000110000110011000010101011111010110100001110" when "01111000",
   "1001101101101111010100111110010000001010100010000111000011110100101101000010100010100110110101100111011010111000100001001011001110000000101110110000011110011001101001011010110111010010" when "01111001",
   "1001101110100011111111110010000001001110001000000101010111110100101000100101001011100101011000110001110110111000110011010010110001110001001010110000001001111100110011011010111010010010" when "01111010",
   "1001101111011000100110001000111111011010010010010001101000110100100100001000111100110010111000010110010010111001000101010010101100111101111000101111110101101011111110111010111101010011" when "01111011",
   "1001110000001101001000000100010010101110101110111110110000110100011111101101110101110000111011000110001100111001010111001011000100000110100010101111100001100111000101111011000000001111" when "01111100",
   "1001110001000001100101100101000010101100111100001101111000110100011011010011110110000001011001101010001100111001101000111011111011100110111100101111001101101101111100111011000011001100" when "01111101",
   "1001110001110101111110101100010110011000011001011110011100110100010110111010111101000110011110010100001100111001111010100101010111111000000010101110111010000000011011111011000110000011" when "01111110",
   "1001110010101010010011011011010100010110111001010001000100110100010010100011001010100010100100110010010110111010001100000111011101001111000010101110100110011110011010011011001000111010" when "01111111",
   "1001110011011110100011110011000010110000110010011101001001110100001110001100011101111000011010000001111000111010011101100010001111111110000110101110010011000111101111111011001011110000" when "10000000",
   "1001110100010010101111110100100111010001010001011001011101110100001001110110110110101010111100000010010010111010101110110101110100010100010110101101111111111100010011111011001110100010" when "10000001",
   "1001110101000110110111100001000111000110101000111000001001110100000101100010010100011101011001101000100100111011000000000010001110011101011010101101101100111011111101111011010001010100" when "10000010",
   "1001110101111010111010111001100111000010100010110101110110110100000001001110110110110011010010010010101110111011010001000111100010100010000110101101011010000110100101111011010100000010" when "10000011",
   "1001110110101110111001111111001011011010010000111100100100110011111100111100011101010000010101111011011110111011100010000101110100101000000010101101000111011100000010111011010110101111" when "10000100",
   "1001110111100010110100110010111000000110111100111010010000110011111000101011000111011000100100101101111010111011110010111101001000110001110010101100110100111100001110011011011001011010" when "10000101",
   "1001111000010110101011010101110000100101111000101011001110110011110100011010110100110000001110111001110000111100000011101101100010111111001010101100100010100110111110111011011011111111" when "10000110",
   "1001111001001010011101101000110111111000101110011000110011110011110000001011100100111011110100100111010000111100010100010111000111001100100010101100010000011100001101111011011110101000" when "10000111",
   "1001111001111110001011101101010000100101110000001100000011110011101011111101010111100000000101101011110100111100100100111001111001010011111110101011111110011011110010011011100001001101" when "10001000",
   "1001111010110001110101100011111100111000000111110101000101110011100111110000001100000010000001011110001010111100110101010101111101001100011000101011101100100101100110011011100011101111" when "10001001",
   "1001111011100101011011001101111110100000000110000110110101110011100011100100000010000110110110101011010100111101000101101011010110101001111010101011011010111001100001111011100110001110" when "10001010",
   "1001111100011000111100101100010110110011010010000111101001110011011111011000111001010100000011001011010000111101010101111010001001011101110000101011001001010111011100111011101000101111" when "10001011",
   "1001111101001100011010000000000110101100111000010110101011110011011011001110110001001111010011110101111000111101100110000010011001010110110010101010110111111111010000011011101011001011" when "10001100",
   "1001111101111111110011001010001110101101111001100110011110110011010111000101101001011110100100011000001000111101110110000100001010000000101110101010100110110000110101111011101101100111" when "10001101",
   "1001111110110011001000001011101110111101011001101100100110110011010010111101100001100111111111001001011000111110000101111111011111000100111000101010010101101100000101111011110000000010" when "10001110",
   "1001111111100110011001000101100111001000101110000110101101110011001110110110011001010001111101000000110000111110010101110100011100001010000000101010000100110000111000111011110010011001" when "10001111",
   "1010000000011001100101111000110110100011101100010101001001110011001010110000010000000011000101001010101010111110100101100011000100110100000010101001110011111111001001011011110100101110" when "10010000",
   "1010000001001100101110100110011100001000111000001011000100110011000110101011000101100010001100111110101010111110110101001011011100100100011000101001100011010110101111111011110111000101" when "10010001",
   "1010000001111111110011001111010110011001110001110100011011110011000010100110111001010110010111110101001100111111000100101101100110111010010010101001010010110111100100111011111001010100" when "10010010",
   "1010000010110010110011110100100011011111000011110001111011110010111110100011101011000110110110111101100010111111010100001001100111010001111010101001000010100001100010111011111011100111" when "10010011",
   "1010000011100101110000010111000001001000110000101010111001110010111010100001011010011011001001010100000110111111100011011111100001000101011010101000110010010100100010111011111101111001" when "10010100",
   "1010000100011000101000110111101100101110100000110101011110110010110110100000000110111010111011011000011110111111110010101111010111101100011110101000100010010000011110111100000000000111" when "10010101",
   "1010000101001011011101010111100011001111101111110101000100110010110010011111110000001110000111000100000001000000000001111001001110011100011100101000010010010101010000111100000010010000" when "10010110",
   "1010000101111110001101110111100001010011111001101111010111110010101110100000010101111100110011100000010011000000010000111101001000100111110100101000000010100010110000111100000100011110" when "10010111",
   "1010000110110000111010011000100011001010101000010111101101110010101010100001110111101111010100111101101001000000011111111011001001011111100000100111110010111000111010111100000110100110" when "10011000",
   "1010000111100011100010111011100100101100000000010001011101110010100110100100010101001110001100101010000111000000101110110011010100010001101000100111100011010111100111011100001000101110" when "10011001",
   "1010001000010110000111100001100001011000101101101001000001110010100010100111101110000010001000101000001111000000111101100101101100001010000110100111010011111110110001011100001010110111" when "10011010",
   "1010001001001000101000001011010100011010010001000011110110110010011110101100000001110100000011100110001001000001001100010010010100010010111000100111000100101110010010011100001100111011" when "10011011",
   "1010001001111011000100111001111000100011001100000111100101110010011010110001010000001101000100110100101011000001011010111001001111110011100100100110110101100110000011111100001110111100" when "10011100",
   "1010001010101101011101101110001000001111001101111000011011110010010110110111011000110110011111111110011111000001101001011010100001110001001110100110100110100110000010011100010001000011" when "10011101",
   "1010001011011111110010101000111101100011011111001110101000110010010010111110011011011001110100111111101111000001110111110110001101001111100100100110010111101110000110011100010011000010" when "10011110",
   "1010001100010010000011101011010010001110101111000011101110110010001111000110010111100000101111111101010111000010000110001100010101001111011100100110001000111110001001111100010101000010" when "10011111",
   "1010001101000100010000110101111111101001011110010110111111110010001011001111001100110101001000111100100111000010010100011100111100110000001010100101111010010110001000111100010110111100" when "10100000",
   "1010001101110110011010001001111110110110001100001001110110110010000111011000111011000001000011111010111111000010100010101000000110101110011010100101101011110101111100111100011000111010" when "10100001",
   "1010001110101000011111101000001000100001100001010011110010110010000011100011100001101110110000100101111011000010110000101101110110000101010110100101011101011101100000011100011010110111" when "10100010",
   "1010001111011010100001010001010101000010011100001110010101110001111111101111000000101000101010010010101011000010111110101110001101101110000000100101001111001100101110111100011100101111" when "10100011",
   "1010010000001100011111000110011100011010011100011000111100110001111011111011010111011001010111110110011111000011001100101001010000011111010000100101000001000011100010111100011110101000" when "10100100",
   "1010010000111110011001001000010110010101101101110100111101110001111000001000100101101011101011011110100111000011011010011111000001001110001100100100110011000001110111011100100000011111" when "10100101",
   "1010010001110000001111010111111010001011010100011001110011110001110100010110101011001010100010101000101101000011101000001111100010101101110100100100100101000111100101011100100010011000" when "10100110",
   "1010010010100010000001110101111110111101010111000001011001110001110000100101100111100001000101111010110101000011110101111010110111101111100010100100010111010100101011011100100100001101" when "10100111",
   "1010010011010011110000100011011011011001001010101101000100110001101100110101011010011010101000111100011011000100000011100001000011000010100100100100001001101001000001011100100110000001" when "10101000",
   "1010010100000101011011100001000101110111011101100010110110110001101001000110000011100010101010001110010101000100010001000010000111010100100010100011111100000100100011011100100111110000" when "10101001",
   "1010010100110111000010101111110100011100100001100011011100110001100101010111100010100100110011000011101101000100011110011110000111010000110110100011101110100111001101011100101001100011" when "10101010",
   "1010010101101000100110010000011100111000010111011000110011110001100001101001110111001100110111011010101111000100101011110101000101100001100100100011100001010000111001111100101011010000" when "10101011",
   "1010010110011010000110000011110100100110111000111101100010110001011101111101000001000110110101110101010111000100111001000111000100101110100000100011010100000001100100011100101101000010" when "10101100",
   "1010010111001011100010001010110000110000000011111101001010110001011010010000111111111110110111010010010101000101000110010100000111011110001010100011000110111001000110111100101110110000" when "10101101",
   "1010010111111100111010100110000110001000000100001101010011110001010110100101110011100001001111000101111101000101010011011100010000010101000110100010111001110111011110111100110000011101" when "10101110",
   "1010011000101110001111010110101001001111011110000000000000110001010010111011011011011010011010110011100011000101100000011111100001110110001010100010101100111100100110011100110010001000" when "10101111",
   "1010011001011111100000011101001110010011011000001111001000110001001111010001110111010111000010000110010001000101101101011101111110100010011100100010100000001000011010111100110011110010" when "10110000",
   "1010011010010000101101111010101001001101100110100000111101110001001011101001000111000011110110101010110001000101111010010111101000111001011100100010010011011010110100011100110101011011" when "10110001",
   "1010011011000001110111101111101101100100110011000110001001110001001000000001001010001101110100001000000111000110000111001100100011011001000010100010000110110011110001011100110111000011" when "10110010",
   "1010011011110010111101111101001110101100101000110001000110110001000100011010000000100001111111111001100001000110010011111100110000011101011010100001111010010011001100111100111000101010" when "10110011",
   "1010011100100100000000100011111111100101111100100110110010110001000000110011101001101101101001000111110011000110100000101000010010100001000110100001101101111001000010111100111010010001" when "10110100",
   "1010011101010100111111100100110010111110110111101001000101110000111101001110000101011110001000100011000001000110101101001111001011111101001010100001100001100101001101111100111011110111" when "10110101",
   "1010011110000101111011000000011011010011000000011010110110110000111001101001010011100001000000011011111111000110111001110001011111001001000100100001010101010111101011011100111101011011" when "10110110",
   "1010011110110110110010110111101010101011100100011101101010110000110110000101010011100011111100011110001111000111000110001111001110011010101010100001001001010000010110111100111110111110" when "10110111",
   "1010011111100111100111001011010010111111100001101001010111110000110010100010000101010100110001101001110011000111010010101000011100000110011000100000111101001111001011011101000000011101" when "10111000",
   "1010100000011000010111111100000101110011101111011101100000110000101110111111101000100001011110001101000011000111011110111101001010011110111100100000110001010100000101111101000001111110" when "10111001",
   "1010100001001001000101001010110100011011001000001100101111110000101011011101111100111000001001011110110011000111101011001101011011110101101010100000100101011111000001011101000011100000" when "10111010",
   "1010100001111001101110111000001111110110110010000010001011110000100111111101000010000111000011111000010001000111110111011001010010011010100100100000011001101111111010111101000101000000" when "10111011",
   "1010100010101010010101000101001000110110001000000000111111110000100100011100110111111100100110101111010011001000000011100000110000011100001010100000001110000110101101111101000110011010" when "10111100",
   "1010100011011010110111110010001111110111000010111110000101110000100000111101011110000111010100010000100001001000001111100011111000000111000110100000000010100011010111011101000111111001" when "10111101",
   "1010100100001011010111000000010101000110000010010011111111110000011101011110110100010101110111011001101101001000011011100010101011100111001100011111110111000101110010011101001001010101" when "10111110",
   "1010100100111011110010110000001000011110010100110001001011110000011010000000111010010111000011110011111111001000100111011101001101000110101000011111101011101101111100111101001010101111" when "10111111",
   "1010100101101100001011000010011001101010000001000000101111110000010110100011101111111001110101101110011101001000110011010011011110101110000100011111100000011011110010011101001100001000" when "11000000",
   "1010100110011100011111110111111000000010001110001101011100110000010011000111010100101101010001111000100011001000111111000101100010100100110010011111010101001111001110111101001101100110" when "11000001",
   "1010100111001100110001010001010010101111001100011111010111110000001111101011101000100000100101011100100101001001001010110011011010110001001100011111001010001000001111011101001110111100" when "11000010",
   "1010100111111100111111001111011000101000011101010100001011110000001100010000101011000011000101111010100001001001010110011101001001010111110000011110111111000110101110111101010000010101" when "11000011",
   "1010101000101101001001110010111000010100111011110010000000110000001000110110011100000100010001000010011001001001100010000010110000011100000010011110110100001010101100011101010001101001" when "11000100",
   "1010101001011101010000111100100000001011000100110101000011110000000101011100111011010011101100101111011011001001101101100100010001111111111000011110101001010100000010011101010011000010" when "11000101",
   "1010101010001101010100101100111110010000111111010111111101110000000010000100001000100001000111000010011111001001111001000001110000000100011000011110011110100010101110111101010100010101" when "11000110",
   "1010101010111101010101000101000000011100100100010111000110101111111110101100000011011100010101111101010011001010000100011011001100101000111100011110010011110110101100111101010101101001" when "11000111",
   "1010101011101101010010000101010100010011100110101110101111101111111011010100101011110101010111011101000101001010001111110000101001101011111000011110001001001111111010011101010110111011" when "11001000",
   "1010101100011101001011101110100111001011111011010100001010101111110111111110000001011100010001010101110011001010011011000010001001001010001000011101111110101110010011011101011000001111" when "11001001",
   "1010101101001101000010000001100110001011100000101001110011101111110100101000000100000001010001001101000011001010100110001111101100111111101010011101110100010001110101011101011001100001" when "11001010",
   "1010101101111100110100111110111110001000100110101110100110101111110001010010110011010100101100010101010011001010110001011001010111000110111110011101101001111010011100111101011010110010" when "11001011",
   "1010101110101100100100100111011011101001110110101000010100101111101101111110001111000110111111101001000011001010111100011111001001011001100010011101011111101000000110011101011100000010" when "11001100",
   "1010101111011100010000111011101011000110011010001001001111101111101010101010010111001000101111100110000001001011000111100001000101101111100100011101010101011010101110111101011101010000" when "11001101",
   "1010110000001011111001111100011000100110000011010001000010101111100111010111001011001010101000001000011101001011010010011111001110000000000010011101001011010010010011011101011110011111" when "11001110",
   "1010110000111011011111101010010000000001010011101000111110101111100100000100101010111101011100100110100011001011011101011001100100000000110110011101000001001110110000111101011111110000" when "11001111",
   "1010110001101011000010000101111101000001100011111011100001101111100000110010110110010010000111101011110101001011101000010000001001100111000000011100110111010000000010111101100000111001" when "11010000",
   "1010110010011010100001010000001011000001001011000111011010101111011101100001101100111001101011010100011011001011110011000011000000100101110000011100101101010110001001011101100010000111" when "11010001",
   "1010110011001001111101001001100101001011100101101110000101101111011010010001001110100101010000101000111011001011111101110010001010101111110100011100100011100000111111011101100011010101" when "11010010",
   "1010110011111001010101110010110110011101011100111101110100101111010111000001011011000110000111111001101101001100001000011101101001110110101010011100011001110000100001111101100100011111" when "11010011",
   "1010110100101000101011001100101001100100101101110111010110101111010011110010010010001101101000011010100101001100010011000101011111101010100000011100010000000100101110011101100101101010" when "11010100",
   "1010110101010111111101010111101001000000110000001111001011101111010000100011110011101101010000011110011111001100011101101001101101111010101000011100000110011101100010111101100110110011" when "11010101",
   "1010110110000111001100010100011111000010011101101010100010101111001101010101111111010110100101010011010011001100101000001010010110010101010000011011111100111010111011011101100111111011" when "11010110",
   "1010110110110110011000000011110101101100011000011000001011101111001010001000110100111011010010111101011101001100110010100111011010100111011110011011110011011100110101011101101001000010" when "11010111",
   "1010110111100101100000100110010110110010110010000100110111101111000110111100010100001101001100010011110111001100111101000000111100011101010110011011101010000011001110011101101010001001" when "11011000",
   "1010111000010100100101111100101011111011110010101011110001101111000011110000011100111110001010111011101111001101000111010110111101100001111000011011100000101110000011011101101011010011" when "11011001",
   "1010111001000011101000000111011110011111011111000010101110101111000000100101001111000000001111000100100001001101010001101001011111011111011000011011010111011101010010011101101100010111" when "11011010",
   "1010111001110010100111000111010111100111111111100010011111101110111101011010101010000101011111100011111011001101011011111000100011111110101100011011001110010000110111011101101101011011" when "11011011",
   "1010111010100001100010111101000000010001100110101010111000101110111010010000101110000000001001110001101111001101100110000100001100100111111000011011000101001000110001011101101110100001" when "11011100",
   "1010111011010000011011101001000001001010110111100011000100101110110111000111011010100010100001100100001101001101110000001100011011000010000110011010111100000100111011011101101111100100" when "11011101",
   "1010111011111111010001001100000010110100101100010101110110101110110011111110101111011111000001001011101011001101111010010001010000110011010110011010110011000101010110011101110000101000" when "11011110",
   "1010111100101110000011100110101101100010011100101010000101101110110000110110101100101000001001001111010011001110000100010010101111100000111010011010101010001001111100011101110001101010" when "11011111",
   "1010111101011100110010111001101001011010000011110111010110101110101101101111010001110000100000101000101101001110001110010000111000101110110100011010100001010010101101011101110010101111" when "11100000",
   "1010111110001011011111000101011110010100000111010110101011101110101010101000011110101010110100100000100111001110011000001011101110000000101010011010011000011111100101011101110011101101" when "11100001",
   "1010111110111010001000001010110011111011111100101111110000101110100111100010010011001001111000001010111001001110100010000011010000111000010110011010001111110000100010011101110100110001" when "11100010",
   "1010111111101000101110001010010001101111110000000010001001101110100100011100101111000000100101000010111101001110101011110111100010110111101110011010000111000101100001111101110101101110" when "11100011",
   "1011000000010111010001000100011111000000101001101011001001101110100001010111110010000001111010101000000111001110110101101000100101011110111010011001111110011110100001111101110110110001" when "11100100",
   "1011000001000101110000111010000010110010110100100111110001101110011110010011011100000000111110011001111101001110111111010110011010001101110100011001110101111011100000011101110111110001" when "11100101",
   "1011000001110100001101101011100011111101100100010011010101101110011011001111101100110000111011110100111011001111001001000001000010100011001100011001101101011100011001011101111000101110" when "11100110",
   "1011000010100010100111011001101001001011011010100010011100101110011000001100100100000101000100001110100101001111010010101000011111111100110010011001100101000001001011011101111001101100" when "11100111",
   "1011000011010000111110000100111000111010001101011010011100101110010101001010000001110000101110110010001111001111011100001100110011110111101010011001011100101001110100111101111010101000" when "11101000",
   "1011000011111111010001101101111001011011001101000101011110101110010010001000000101100111011000011101100101001111100101101101111111101111111000011001010100010110010010111101111011100111" when "11101001",
   "1011000100101101100010010101010000110011001001100011000100101110001111000110101111011100100011111101001011001111101111001100000101000000111010011001001100000110100011011101111100100010" when "11101010",
   "1011000101011011101111111011100100111010011000010101011001101110001100000101111111000011111001101001001011001111111000100111000101000101000100011001000011111010100011111101111101100000" when "11101011",
   "1011000110001001111010100001011011011100111010001011001001101110001001000101110100010001000111100001111101010000000001111111000001010110001100011000111011110010010001111101111110011001" when "11101100",
   "1011000110111000000010000111011001111010100000100110001000101110000110000110001110111000000001001100110001010000001011010011111011001100111100011000110011101101101011111101111111010010" when "11101101",
   "1011000111100110000110101110000101100110110011011110100100101110000011000111001110101100011111110000101001010000010100100101110100000001010010011000101011101100101111111110000000001111" when "11101110",
   "1011001000010100001000010110000011101001010110100011001011101110000000001000110011100010100001110011000101010000011101110100101101001010101000011000100011101111011010111110000001001000" when "11101111",
   "1011001001000010000110111111111000111101101110110110000110101101111101001010111101001110001011010100110101010000100111000000100111111111010000011000011011110101101011111110000010000010" when "11110000",
   "1011001001110000000010101100001010010011101000000110100011101101111010001101101011100011100101101110110111010000110000001001100101110101000000011000010011111111011111111110000010111001" when "11110001",
   "1011001010011101111011011011011100001110111010000111100011101101110111010000111110010110111111101111001011010000111001001111101000000000100010011000001100001100110100111110000011110001" when "11110010",
   "1011001011001011110001001110010011000111101110000011010010101101110100010100110101011100101101010101101011010001000010010010101111110110000110011000000100011101101010011110000100101000" when "11110011",
   "1011001011111001100100000101010011001010100011101011101100101101110001011001010000101001000111110001010111010001001011010010111110101000111010010111111100110001111100011110000101100000" when "11110100",
   "1011001100100111010100000001000000011000010110100111110001101101101110011110001111110000101101011101000011010001010100010000010101101011110000010111110101001001101001011110000110010111" when "11110101",
   "1011001101010101000001000001111110100110100011011110001001101101101011100011110010101000000001111100011111010001011101001010110110010000011000010111101101100100110000011110000111001101" when "11110110",
   "1011001110000010101011001000110001011111001100111100011100101101101000101001111001000011101101111001100001010001100110000010100001100111111110010111100110000011001110111110001000000011" when "11110111",
   "1011001110110000010010010101111100100001000000111011111010101101100101110000100010111000011111000001000101010001101110110111011001000010111110010111011110100101000010011110001000111000" when "11111000",
   "1011001111011101110110101010000010111111011101100011000100101101100010110111101111111011001000000000010011010001110111101001011101110001000100010111010111001010001001011110001001101100" when "11111001",
   "1011010000001011011000000101101000000010110110000100100000101101011111111111100000000000100000100001101101010010000000011000110001000001001110010111001111110010100001111110001010011111" when "11111010",
   "1011010000111000110110101001001110101000010111111010110110101101011101000111110010111101100101001010011001010010001001000101010100000001101000010111001000011110001001111110001011010110" when "11111011",
   "1011010001100110010010010101011001100010001111100001111010101101011010010000101000100111010111010111001011010010010001101111001000000000001010010111000001001101000000111110001100000111" when "11111100",
   "1011010010010011101011001010101011010111101101001101001010101101010111011010000000110010111101011001111111010010011010010110001110001001100000010110111001111111000011111110001100111001" when "11111101",
   "1011010011000001000001001001100110100101001001111011010100101101010100100011111011010101100010010111000001010010100010111010100111101001110110010110110010110100010001111110001101101110" when "11111110",
   "1011010011101110010100010010101101011100001100000111010010101101010001101110011000000100010110000010000111010010101011011100010101101101000010010110101011101100100111011110001110011110" when "11111111",
   "----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" when others;
    Y <= TableOut_d1;
end architecture;

--------------------------------------------------------------------------------
--                           IntAdder_29_f400_uid33
--                     (IntAdderClassical_29_F400_uid35)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2010)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_29_f400_uid33 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(28 downto 0);
          Y : in  std_logic_vector(28 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(28 downto 0)   );
end entity;

architecture arch of IntAdder_29_f400_uid33 is
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   --Classical
    R <= X + Y + Cin;
end architecture;

--------------------------------------------------------------------------------
--                  FixMultAdd_18x18p27r27signed_F400_uid10
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan, 2012-2014
--------------------------------------------------------------------------------
-- Pipeline depth: 3 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixMultAdd_18x18p27r27signed_F400_uid10 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(17 downto 0);
          Y : in  std_logic_vector(17 downto 0);
          A : in  std_logic_vector(26 downto 0);
          R : out  std_logic_vector(26 downto 0)   );
end entity;

architecture arch of FixMultAdd_18x18p27r27signed_F400_uid10 is
   component IntAdder_29_f400_uid33 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(28 downto 0);
             Y : in  std_logic_vector(28 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(28 downto 0)   );
   end component;

   component Compressor_23_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_3_2 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

signal XX_m13, XX_m13_d1 :  std_logic_vector(17 downto 0);
signal YY_m13, YY_m13_d1 :  std_logic_vector(17 downto 0);
signal DSP_Res_12 :  std_logic_vector(42 downto 0);
signal heap_bh11_w0_0 :  std_logic;
signal heap_bh11_w1_0 :  std_logic;
signal heap_bh11_w2_0 :  std_logic;
signal heap_bh11_w3_0 :  std_logic;
signal heap_bh11_w4_0, heap_bh11_w4_0_d1, heap_bh11_w4_0_d2 :  std_logic;
signal heap_bh11_w5_0, heap_bh11_w5_0_d1, heap_bh11_w5_0_d2 :  std_logic;
signal heap_bh11_w6_0, heap_bh11_w6_0_d1, heap_bh11_w6_0_d2 :  std_logic;
signal heap_bh11_w7_0, heap_bh11_w7_0_d1, heap_bh11_w7_0_d2 :  std_logic;
signal heap_bh11_w8_0, heap_bh11_w8_0_d1, heap_bh11_w8_0_d2 :  std_logic;
signal heap_bh11_w9_0, heap_bh11_w9_0_d1, heap_bh11_w9_0_d2 :  std_logic;
signal heap_bh11_w10_0, heap_bh11_w10_0_d1, heap_bh11_w10_0_d2 :  std_logic;
signal heap_bh11_w11_0, heap_bh11_w11_0_d1, heap_bh11_w11_0_d2 :  std_logic;
signal heap_bh11_w12_0, heap_bh11_w12_0_d1, heap_bh11_w12_0_d2 :  std_logic;
signal heap_bh11_w13_0, heap_bh11_w13_0_d1, heap_bh11_w13_0_d2 :  std_logic;
signal heap_bh11_w14_0, heap_bh11_w14_0_d1, heap_bh11_w14_0_d2 :  std_logic;
signal heap_bh11_w15_0, heap_bh11_w15_0_d1, heap_bh11_w15_0_d2 :  std_logic;
signal heap_bh11_w16_0, heap_bh11_w16_0_d1, heap_bh11_w16_0_d2 :  std_logic;
signal heap_bh11_w17_0, heap_bh11_w17_0_d1, heap_bh11_w17_0_d2 :  std_logic;
signal heap_bh11_w18_0, heap_bh11_w18_0_d1, heap_bh11_w18_0_d2 :  std_logic;
signal heap_bh11_w19_0, heap_bh11_w19_0_d1, heap_bh11_w19_0_d2 :  std_logic;
signal heap_bh11_w20_0, heap_bh11_w20_0_d1, heap_bh11_w20_0_d2 :  std_logic;
signal heap_bh11_w21_0, heap_bh11_w21_0_d1, heap_bh11_w21_0_d2 :  std_logic;
signal heap_bh11_w22_0, heap_bh11_w22_0_d1, heap_bh11_w22_0_d2 :  std_logic;
signal heap_bh11_w23_0 :  std_logic;
signal heap_bh11_w5_1, heap_bh11_w5_1_d1, heap_bh11_w5_1_d2 :  std_logic;
signal heap_bh11_w6_1, heap_bh11_w6_1_d1, heap_bh11_w6_1_d2 :  std_logic;
signal heap_bh11_w7_1, heap_bh11_w7_1_d1, heap_bh11_w7_1_d2 :  std_logic;
signal heap_bh11_w8_1, heap_bh11_w8_1_d1, heap_bh11_w8_1_d2 :  std_logic;
signal heap_bh11_w9_1, heap_bh11_w9_1_d1, heap_bh11_w9_1_d2 :  std_logic;
signal heap_bh11_w10_1, heap_bh11_w10_1_d1, heap_bh11_w10_1_d2 :  std_logic;
signal heap_bh11_w11_1, heap_bh11_w11_1_d1, heap_bh11_w11_1_d2 :  std_logic;
signal heap_bh11_w12_1, heap_bh11_w12_1_d1, heap_bh11_w12_1_d2 :  std_logic;
signal heap_bh11_w13_1, heap_bh11_w13_1_d1, heap_bh11_w13_1_d2 :  std_logic;
signal heap_bh11_w14_1, heap_bh11_w14_1_d1, heap_bh11_w14_1_d2 :  std_logic;
signal heap_bh11_w15_1, heap_bh11_w15_1_d1, heap_bh11_w15_1_d2 :  std_logic;
signal heap_bh11_w16_1, heap_bh11_w16_1_d1, heap_bh11_w16_1_d2 :  std_logic;
signal heap_bh11_w17_1, heap_bh11_w17_1_d1, heap_bh11_w17_1_d2 :  std_logic;
signal heap_bh11_w18_1, heap_bh11_w18_1_d1, heap_bh11_w18_1_d2 :  std_logic;
signal heap_bh11_w19_1, heap_bh11_w19_1_d1, heap_bh11_w19_1_d2 :  std_logic;
signal heap_bh11_w20_1, heap_bh11_w20_1_d1, heap_bh11_w20_1_d2 :  std_logic;
signal heap_bh11_w21_1, heap_bh11_w21_1_d1, heap_bh11_w21_1_d2 :  std_logic;
signal heap_bh11_w22_1, heap_bh11_w22_1_d1, heap_bh11_w22_1_d2 :  std_logic;
signal heap_bh11_w23_1 :  std_logic;
signal heap_bh11_w24_0 :  std_logic;
signal heap_bh11_w25_0 :  std_logic;
signal heap_bh11_w26_0 :  std_logic;
signal heap_bh11_w27_0 :  std_logic;
signal heap_bh11_w28_0 :  std_logic;
signal heap_bh11_w29_0, heap_bh11_w29_0_d1 :  std_logic;
signal heap_bh11_w30_0, heap_bh11_w30_0_d1 :  std_logic;
signal heap_bh11_w31_0, heap_bh11_w31_0_d1 :  std_logic;
signal heap_bh11_w4_1, heap_bh11_w4_1_d1, heap_bh11_w4_1_d2, heap_bh11_w4_1_d3 :  std_logic;
signal heap_bh11_w23_2, heap_bh11_w23_2_d1 :  std_logic;
signal heap_bh11_w24_1, heap_bh11_w24_1_d1 :  std_logic;
signal heap_bh11_w25_1, heap_bh11_w25_1_d1 :  std_logic;
signal heap_bh11_w26_1, heap_bh11_w26_1_d1 :  std_logic;
signal heap_bh11_w27_1, heap_bh11_w27_1_d1 :  std_logic;
signal heap_bh11_w28_1, heap_bh11_w28_1_d1 :  std_logic;
signal heap_bh11_w29_1, heap_bh11_w29_1_d1, heap_bh11_w29_1_d2 :  std_logic;
signal heap_bh11_w30_1, heap_bh11_w30_1_d1, heap_bh11_w30_1_d2 :  std_logic;
signal heap_bh11_w31_1, heap_bh11_w31_1_d1, heap_bh11_w31_1_d2 :  std_logic;
signal tempR_bh11_0, tempR_bh11_0_d1, tempR_bh11_0_d2 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_0_0 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_0_1 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh11_w23_3, heap_bh11_w23_3_d1, heap_bh11_w23_3_d2 :  std_logic;
signal heap_bh11_w24_2, heap_bh11_w24_2_d1, heap_bh11_w24_2_d2 :  std_logic;
signal heap_bh11_w25_2 :  std_logic;
signal CompressorIn_bh11_1_2 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_1_3 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh11_w25_3, heap_bh11_w25_3_d1, heap_bh11_w25_3_d2 :  std_logic;
signal heap_bh11_w26_2, heap_bh11_w26_2_d1, heap_bh11_w26_2_d2 :  std_logic;
signal heap_bh11_w27_2 :  std_logic;
signal CompressorIn_bh11_2_4 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_2_5 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh11_w27_3, heap_bh11_w27_3_d1, heap_bh11_w27_3_d2 :  std_logic;
signal heap_bh11_w28_2, heap_bh11_w28_2_d1, heap_bh11_w28_2_d2 :  std_logic;
signal heap_bh11_w29_2, heap_bh11_w29_2_d1 :  std_logic;
signal CompressorIn_bh11_3_6 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_3_7 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh11_w29_3, heap_bh11_w29_3_d1 :  std_logic;
signal heap_bh11_w30_2, heap_bh11_w30_2_d1 :  std_logic;
signal heap_bh11_w31_2 :  std_logic;
signal CompressorIn_bh11_4_8 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh11_4_4 :  std_logic_vector(1 downto 0);
signal heap_bh11_w31_3, heap_bh11_w31_3_d1 :  std_logic;
signal finalAdderIn0_bh11 :  std_logic_vector(28 downto 0);
signal finalAdderIn1_bh11 :  std_logic_vector(28 downto 0);
signal finalAdderCin_bh11 :  std_logic;
signal finalAdderOut_bh11 :  std_logic_vector(28 downto 0);
signal CompressionResult11 :  std_logic_vector(32 downto 0);
signal A_d1 :  std_logic_vector(26 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            XX_m13_d1 <=  XX_m13;
            YY_m13_d1 <=  YY_m13;
            heap_bh11_w4_0_d1 <=  heap_bh11_w4_0;
            heap_bh11_w4_0_d2 <=  heap_bh11_w4_0_d1;
            heap_bh11_w5_0_d1 <=  heap_bh11_w5_0;
            heap_bh11_w5_0_d2 <=  heap_bh11_w5_0_d1;
            heap_bh11_w6_0_d1 <=  heap_bh11_w6_0;
            heap_bh11_w6_0_d2 <=  heap_bh11_w6_0_d1;
            heap_bh11_w7_0_d1 <=  heap_bh11_w7_0;
            heap_bh11_w7_0_d2 <=  heap_bh11_w7_0_d1;
            heap_bh11_w8_0_d1 <=  heap_bh11_w8_0;
            heap_bh11_w8_0_d2 <=  heap_bh11_w8_0_d1;
            heap_bh11_w9_0_d1 <=  heap_bh11_w9_0;
            heap_bh11_w9_0_d2 <=  heap_bh11_w9_0_d1;
            heap_bh11_w10_0_d1 <=  heap_bh11_w10_0;
            heap_bh11_w10_0_d2 <=  heap_bh11_w10_0_d1;
            heap_bh11_w11_0_d1 <=  heap_bh11_w11_0;
            heap_bh11_w11_0_d2 <=  heap_bh11_w11_0_d1;
            heap_bh11_w12_0_d1 <=  heap_bh11_w12_0;
            heap_bh11_w12_0_d2 <=  heap_bh11_w12_0_d1;
            heap_bh11_w13_0_d1 <=  heap_bh11_w13_0;
            heap_bh11_w13_0_d2 <=  heap_bh11_w13_0_d1;
            heap_bh11_w14_0_d1 <=  heap_bh11_w14_0;
            heap_bh11_w14_0_d2 <=  heap_bh11_w14_0_d1;
            heap_bh11_w15_0_d1 <=  heap_bh11_w15_0;
            heap_bh11_w15_0_d2 <=  heap_bh11_w15_0_d1;
            heap_bh11_w16_0_d1 <=  heap_bh11_w16_0;
            heap_bh11_w16_0_d2 <=  heap_bh11_w16_0_d1;
            heap_bh11_w17_0_d1 <=  heap_bh11_w17_0;
            heap_bh11_w17_0_d2 <=  heap_bh11_w17_0_d1;
            heap_bh11_w18_0_d1 <=  heap_bh11_w18_0;
            heap_bh11_w18_0_d2 <=  heap_bh11_w18_0_d1;
            heap_bh11_w19_0_d1 <=  heap_bh11_w19_0;
            heap_bh11_w19_0_d2 <=  heap_bh11_w19_0_d1;
            heap_bh11_w20_0_d1 <=  heap_bh11_w20_0;
            heap_bh11_w20_0_d2 <=  heap_bh11_w20_0_d1;
            heap_bh11_w21_0_d1 <=  heap_bh11_w21_0;
            heap_bh11_w21_0_d2 <=  heap_bh11_w21_0_d1;
            heap_bh11_w22_0_d1 <=  heap_bh11_w22_0;
            heap_bh11_w22_0_d2 <=  heap_bh11_w22_0_d1;
            heap_bh11_w5_1_d1 <=  heap_bh11_w5_1;
            heap_bh11_w5_1_d2 <=  heap_bh11_w5_1_d1;
            heap_bh11_w6_1_d1 <=  heap_bh11_w6_1;
            heap_bh11_w6_1_d2 <=  heap_bh11_w6_1_d1;
            heap_bh11_w7_1_d1 <=  heap_bh11_w7_1;
            heap_bh11_w7_1_d2 <=  heap_bh11_w7_1_d1;
            heap_bh11_w8_1_d1 <=  heap_bh11_w8_1;
            heap_bh11_w8_1_d2 <=  heap_bh11_w8_1_d1;
            heap_bh11_w9_1_d1 <=  heap_bh11_w9_1;
            heap_bh11_w9_1_d2 <=  heap_bh11_w9_1_d1;
            heap_bh11_w10_1_d1 <=  heap_bh11_w10_1;
            heap_bh11_w10_1_d2 <=  heap_bh11_w10_1_d1;
            heap_bh11_w11_1_d1 <=  heap_bh11_w11_1;
            heap_bh11_w11_1_d2 <=  heap_bh11_w11_1_d1;
            heap_bh11_w12_1_d1 <=  heap_bh11_w12_1;
            heap_bh11_w12_1_d2 <=  heap_bh11_w12_1_d1;
            heap_bh11_w13_1_d1 <=  heap_bh11_w13_1;
            heap_bh11_w13_1_d2 <=  heap_bh11_w13_1_d1;
            heap_bh11_w14_1_d1 <=  heap_bh11_w14_1;
            heap_bh11_w14_1_d2 <=  heap_bh11_w14_1_d1;
            heap_bh11_w15_1_d1 <=  heap_bh11_w15_1;
            heap_bh11_w15_1_d2 <=  heap_bh11_w15_1_d1;
            heap_bh11_w16_1_d1 <=  heap_bh11_w16_1;
            heap_bh11_w16_1_d2 <=  heap_bh11_w16_1_d1;
            heap_bh11_w17_1_d1 <=  heap_bh11_w17_1;
            heap_bh11_w17_1_d2 <=  heap_bh11_w17_1_d1;
            heap_bh11_w18_1_d1 <=  heap_bh11_w18_1;
            heap_bh11_w18_1_d2 <=  heap_bh11_w18_1_d1;
            heap_bh11_w19_1_d1 <=  heap_bh11_w19_1;
            heap_bh11_w19_1_d2 <=  heap_bh11_w19_1_d1;
            heap_bh11_w20_1_d1 <=  heap_bh11_w20_1;
            heap_bh11_w20_1_d2 <=  heap_bh11_w20_1_d1;
            heap_bh11_w21_1_d1 <=  heap_bh11_w21_1;
            heap_bh11_w21_1_d2 <=  heap_bh11_w21_1_d1;
            heap_bh11_w22_1_d1 <=  heap_bh11_w22_1;
            heap_bh11_w22_1_d2 <=  heap_bh11_w22_1_d1;
            heap_bh11_w29_0_d1 <=  heap_bh11_w29_0;
            heap_bh11_w30_0_d1 <=  heap_bh11_w30_0;
            heap_bh11_w31_0_d1 <=  heap_bh11_w31_0;
            heap_bh11_w4_1_d1 <=  heap_bh11_w4_1;
            heap_bh11_w4_1_d2 <=  heap_bh11_w4_1_d1;
            heap_bh11_w4_1_d3 <=  heap_bh11_w4_1_d2;
            heap_bh11_w23_2_d1 <=  heap_bh11_w23_2;
            heap_bh11_w24_1_d1 <=  heap_bh11_w24_1;
            heap_bh11_w25_1_d1 <=  heap_bh11_w25_1;
            heap_bh11_w26_1_d1 <=  heap_bh11_w26_1;
            heap_bh11_w27_1_d1 <=  heap_bh11_w27_1;
            heap_bh11_w28_1_d1 <=  heap_bh11_w28_1;
            heap_bh11_w29_1_d1 <=  heap_bh11_w29_1;
            heap_bh11_w29_1_d2 <=  heap_bh11_w29_1_d1;
            heap_bh11_w30_1_d1 <=  heap_bh11_w30_1;
            heap_bh11_w30_1_d2 <=  heap_bh11_w30_1_d1;
            heap_bh11_w31_1_d1 <=  heap_bh11_w31_1;
            heap_bh11_w31_1_d2 <=  heap_bh11_w31_1_d1;
            tempR_bh11_0_d1 <=  tempR_bh11_0;
            tempR_bh11_0_d2 <=  tempR_bh11_0_d1;
            heap_bh11_w23_3_d1 <=  heap_bh11_w23_3;
            heap_bh11_w23_3_d2 <=  heap_bh11_w23_3_d1;
            heap_bh11_w24_2_d1 <=  heap_bh11_w24_2;
            heap_bh11_w24_2_d2 <=  heap_bh11_w24_2_d1;
            heap_bh11_w25_3_d1 <=  heap_bh11_w25_3;
            heap_bh11_w25_3_d2 <=  heap_bh11_w25_3_d1;
            heap_bh11_w26_2_d1 <=  heap_bh11_w26_2;
            heap_bh11_w26_2_d2 <=  heap_bh11_w26_2_d1;
            heap_bh11_w27_3_d1 <=  heap_bh11_w27_3;
            heap_bh11_w27_3_d2 <=  heap_bh11_w27_3_d1;
            heap_bh11_w28_2_d1 <=  heap_bh11_w28_2;
            heap_bh11_w28_2_d2 <=  heap_bh11_w28_2_d1;
            heap_bh11_w29_2_d1 <=  heap_bh11_w29_2;
            heap_bh11_w29_3_d1 <=  heap_bh11_w29_3;
            heap_bh11_w30_2_d1 <=  heap_bh11_w30_2;
            heap_bh11_w31_3_d1 <=  heap_bh11_w31_3;
            A_d1 <=  A;
         end if;
      end process;
   XX_m13 <= X ;
   YY_m13 <= Y ;
   ----------------Synchro barrier, entering cycle 1----------------
   DSP_Res_12 <=  std_logic_vector(signed'(signed((XX_m13_d1(17) & XX_m13_d1(17) & XX_m13_d1(17) & XX_m13_d1(17) & XX_m13_d1(17) & XX_m13_d1(17) & XX_m13_d1(17)) & XX_m13_d1) * signed(YY_m13_d1)));
   heap_bh11_w0_0 <= DSP_Res_12(12); -- cycle= 1 cp= 0
   heap_bh11_w1_0 <= DSP_Res_12(13); -- cycle= 1 cp= 0
   heap_bh11_w2_0 <= DSP_Res_12(14); -- cycle= 1 cp= 0
   heap_bh11_w3_0 <= DSP_Res_12(15); -- cycle= 1 cp= 0
   heap_bh11_w4_0 <= DSP_Res_12(16); -- cycle= 1 cp= 0
   heap_bh11_w5_0 <= DSP_Res_12(17); -- cycle= 1 cp= 0
   heap_bh11_w6_0 <= DSP_Res_12(18); -- cycle= 1 cp= 0
   heap_bh11_w7_0 <= DSP_Res_12(19); -- cycle= 1 cp= 0
   heap_bh11_w8_0 <= DSP_Res_12(20); -- cycle= 1 cp= 0
   heap_bh11_w9_0 <= DSP_Res_12(21); -- cycle= 1 cp= 0
   heap_bh11_w10_0 <= DSP_Res_12(22); -- cycle= 1 cp= 0
   heap_bh11_w11_0 <= DSP_Res_12(23); -- cycle= 1 cp= 0
   heap_bh11_w12_0 <= DSP_Res_12(24); -- cycle= 1 cp= 0
   heap_bh11_w13_0 <= DSP_Res_12(25); -- cycle= 1 cp= 0
   heap_bh11_w14_0 <= DSP_Res_12(26); -- cycle= 1 cp= 0
   heap_bh11_w15_0 <= DSP_Res_12(27); -- cycle= 1 cp= 0
   heap_bh11_w16_0 <= DSP_Res_12(28); -- cycle= 1 cp= 0
   heap_bh11_w17_0 <= DSP_Res_12(29); -- cycle= 1 cp= 0
   heap_bh11_w18_0 <= DSP_Res_12(30); -- cycle= 1 cp= 0
   heap_bh11_w19_0 <= DSP_Res_12(31); -- cycle= 1 cp= 0
   heap_bh11_w20_0 <= DSP_Res_12(32); -- cycle= 1 cp= 0
   heap_bh11_w21_0 <= DSP_Res_12(33); -- cycle= 1 cp= 0
   heap_bh11_w22_0 <= DSP_Res_12(34); -- cycle= 1 cp= 0
   heap_bh11_w23_0 <= not(DSP_Res_12(35)); -- cycle= 1 cp= 0
   heap_bh11_w5_1 <= A_d1(0); -- cycle= 1 cp= 0
   heap_bh11_w6_1 <= A_d1(1); -- cycle= 1 cp= 0
   heap_bh11_w7_1 <= A_d1(2); -- cycle= 1 cp= 0
   heap_bh11_w8_1 <= A_d1(3); -- cycle= 1 cp= 0
   heap_bh11_w9_1 <= A_d1(4); -- cycle= 1 cp= 0
   heap_bh11_w10_1 <= A_d1(5); -- cycle= 1 cp= 0
   heap_bh11_w11_1 <= A_d1(6); -- cycle= 1 cp= 0
   heap_bh11_w12_1 <= A_d1(7); -- cycle= 1 cp= 0
   heap_bh11_w13_1 <= A_d1(8); -- cycle= 1 cp= 0
   heap_bh11_w14_1 <= A_d1(9); -- cycle= 1 cp= 0
   heap_bh11_w15_1 <= A_d1(10); -- cycle= 1 cp= 0
   heap_bh11_w16_1 <= A_d1(11); -- cycle= 1 cp= 0
   heap_bh11_w17_1 <= A_d1(12); -- cycle= 1 cp= 0
   heap_bh11_w18_1 <= A_d1(13); -- cycle= 1 cp= 0
   heap_bh11_w19_1 <= A_d1(14); -- cycle= 1 cp= 0
   heap_bh11_w20_1 <= A_d1(15); -- cycle= 1 cp= 0
   heap_bh11_w21_1 <= A_d1(16); -- cycle= 1 cp= 0
   heap_bh11_w22_1 <= A_d1(17); -- cycle= 1 cp= 0
   heap_bh11_w23_1 <= A_d1(18); -- cycle= 1 cp= 0
   heap_bh11_w24_0 <= A_d1(19); -- cycle= 1 cp= 0
   heap_bh11_w25_0 <= A_d1(20); -- cycle= 1 cp= 0
   heap_bh11_w26_0 <= A_d1(21); -- cycle= 1 cp= 0
   heap_bh11_w27_0 <= A_d1(22); -- cycle= 1 cp= 0
   heap_bh11_w28_0 <= A_d1(23); -- cycle= 1 cp= 0
   heap_bh11_w29_0 <= A_d1(24); -- cycle= 1 cp= 0
   heap_bh11_w30_0 <= A_d1(25); -- cycle= 1 cp= 0
   heap_bh11_w31_0 <= A_d1(26); -- cycle= 1 cp= 0
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh11_w4_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w23_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w24_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w25_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w26_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w27_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w28_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w29_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w30_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w31_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   tempR_bh11_0 <= heap_bh11_w3_0 & heap_bh11_w2_0 & heap_bh11_w1_0 & heap_bh11_w0_0; -- already compressed

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_0_0 <= heap_bh11_w23_2_d1 & heap_bh11_w23_1 & heap_bh11_w23_0;
   CompressorIn_bh11_0_1 <= heap_bh11_w24_1_d1 & heap_bh11_w24_0;
      Compressor_bh11_0: Compressor_23_3
      port map ( R => CompressorOut_bh11_0_0,
                 X0 => CompressorIn_bh11_0_0,
                 X1 => CompressorIn_bh11_0_1);
   heap_bh11_w23_3 <= CompressorOut_bh11_0_0(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w24_2 <= CompressorOut_bh11_0_0(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w25_2 <= CompressorOut_bh11_0_0(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_1_2 <= heap_bh11_w25_1_d1 & heap_bh11_w25_0 & heap_bh11_w25_2;
   CompressorIn_bh11_1_3 <= heap_bh11_w26_1_d1 & heap_bh11_w26_0;
      Compressor_bh11_1: Compressor_23_3
      port map ( R => CompressorOut_bh11_1_1,
                 X0 => CompressorIn_bh11_1_2,
                 X1 => CompressorIn_bh11_1_3);
   heap_bh11_w25_3 <= CompressorOut_bh11_1_1(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh11_w26_2 <= CompressorOut_bh11_1_1(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh11_w27_2 <= CompressorOut_bh11_1_1(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_2_4 <= heap_bh11_w27_1_d1 & heap_bh11_w27_0 & heap_bh11_w27_2;
   CompressorIn_bh11_2_5 <= heap_bh11_w28_1_d1 & heap_bh11_w28_0;
      Compressor_bh11_2: Compressor_23_3
      port map ( R => CompressorOut_bh11_2_2,
                 X0 => CompressorIn_bh11_2_4,
                 X1 => CompressorIn_bh11_2_5);
   heap_bh11_w27_3 <= CompressorOut_bh11_2_2(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh11_w28_2 <= CompressorOut_bh11_2_2(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh11_w29_2 <= CompressorOut_bh11_2_2(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh11_3_6 <= heap_bh11_w29_1_d2 & heap_bh11_w29_0_d1 & heap_bh11_w29_2_d1;
   CompressorIn_bh11_3_7 <= heap_bh11_w30_1_d2 & heap_bh11_w30_0_d1;
      Compressor_bh11_3: Compressor_23_3
      port map ( R => CompressorOut_bh11_3_3,
                 X0 => CompressorIn_bh11_3_6,
                 X1 => CompressorIn_bh11_3_7);
   heap_bh11_w29_3 <= CompressorOut_bh11_3_3(0); -- cycle= 2 cp= 0
   heap_bh11_w30_2 <= CompressorOut_bh11_3_3(1); -- cycle= 2 cp= 0
   heap_bh11_w31_2 <= CompressorOut_bh11_3_3(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh11_4_8 <= heap_bh11_w31_1_d2 & heap_bh11_w31_0_d1 & heap_bh11_w31_2;
      Compressor_bh11_4: Compressor_3_2
      port map ( R => CompressorOut_bh11_4_4,
                 X0 => CompressorIn_bh11_4_8);
   heap_bh11_w31_3 <= CompressorOut_bh11_4_4(0); -- cycle= 2 cp= 5.3072e-10
   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   finalAdderIn0_bh11 <= "0" & heap_bh11_w31_3_d1 & heap_bh11_w30_2_d1 & heap_bh11_w29_3_d1 & heap_bh11_w28_2_d2 & heap_bh11_w27_3_d2 & heap_bh11_w26_2_d2 & heap_bh11_w25_3_d2 & heap_bh11_w24_2_d2 & heap_bh11_w23_3_d2 & heap_bh11_w22_1_d2 & heap_bh11_w21_1_d2 & heap_bh11_w20_1_d2 & heap_bh11_w19_1_d2 & heap_bh11_w18_1_d2 & heap_bh11_w17_1_d2 & heap_bh11_w16_1_d2 & heap_bh11_w15_1_d2 & heap_bh11_w14_1_d2 & heap_bh11_w13_1_d2 & heap_bh11_w12_1_d2 & heap_bh11_w11_1_d2 & heap_bh11_w10_1_d2 & heap_bh11_w9_1_d2 & heap_bh11_w8_1_d2 & heap_bh11_w7_1_d2 & heap_bh11_w6_1_d2 & heap_bh11_w5_1_d2 & heap_bh11_w4_1_d3;
   finalAdderIn1_bh11 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh11_w22_0_d2 & heap_bh11_w21_0_d2 & heap_bh11_w20_0_d2 & heap_bh11_w19_0_d2 & heap_bh11_w18_0_d2 & heap_bh11_w17_0_d2 & heap_bh11_w16_0_d2 & heap_bh11_w15_0_d2 & heap_bh11_w14_0_d2 & heap_bh11_w13_0_d2 & heap_bh11_w12_0_d2 & heap_bh11_w11_0_d2 & heap_bh11_w10_0_d2 & heap_bh11_w9_0_d2 & heap_bh11_w8_0_d2 & heap_bh11_w7_0_d2 & heap_bh11_w6_0_d2 & heap_bh11_w5_0_d2 & heap_bh11_w4_0_d2;
   finalAdderCin_bh11 <= '0';
      Adder_final11_0: IntAdder_29_f400_uid33  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh11,
                 R => finalAdderOut_bh11,
                 X => finalAdderIn0_bh11,
                 Y => finalAdderIn1_bh11);
   -- concatenate all the compressed chunks
   CompressionResult11 <= finalAdderOut_bh11 & tempR_bh11_0_d2;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult11(31 downto 5);
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_42_f400_uid234
--                     (IntAdderClassical_42_F400_uid236)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2010)
--------------------------------------------------------------------------------
-- Pipeline depth: 0 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_42_f400_uid234 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(41 downto 0);
          Y : in  std_logic_vector(41 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(41 downto 0)   );
end entity;

architecture arch of IntAdder_42_f400_uid234 is
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   --Classical
    R <= X + Y + Cin;
end architecture;

--------------------------------------------------------------------------------
--                  FixMultAdd_27x27p37r37signed_F400_uid43
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan, 2012-2014
--------------------------------------------------------------------------------
-- Pipeline depth: 3 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixMultAdd_27x27p37r37signed_F400_uid43 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(26 downto 0);
          Y : in  std_logic_vector(26 downto 0);
          A : in  std_logic_vector(36 downto 0);
          R : out  std_logic_vector(36 downto 0)   );
end entity;

architecture arch of FixMultAdd_27x27p37r37signed_F400_uid43 is
   component IntAdder_42_f400_uid234 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(41 downto 0);
             Y : in  std_logic_vector(41 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(41 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid50 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid52 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid54 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid56 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid65 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid67 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid69 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid71 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component Compressor_6_3 is
      port ( X0 : in  std_logic_vector(5 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_14_3 is
      port ( X0 : in  std_logic_vector(3 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_23_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_13_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_3_2 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

signal XX_m46 :  std_logic_vector(26 downto 0);
signal YY_m46 :  std_logic_vector(26 downto 0);
signal Xp_m46b48 :  std_logic_vector(2 downto 0);
signal Yp_m46b48 :  std_logic_vector(8 downto 0);
signal x_m46b48_0 :  std_logic_vector(2 downto 0);
signal y_m46b48_0 :  std_logic_vector(2 downto 0);
signal y_m46b48_1 :  std_logic_vector(2 downto 0);
signal y_m46b48_2 :  std_logic_vector(2 downto 0);
signal Y0X0_48_m46 :  std_logic_vector(5 downto 0);
signal PP48X0Y0_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_0 :  std_logic;
signal heap_bh44_w1_0 :  std_logic;
signal heap_bh44_w2_0 :  std_logic;
signal Y1X0_48_m46 :  std_logic_vector(5 downto 0);
signal PP48X0Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w1_1 :  std_logic;
signal heap_bh44_w2_1 :  std_logic;
signal heap_bh44_w3_0 :  std_logic;
signal heap_bh44_w4_0 :  std_logic;
signal heap_bh44_w5_0 :  std_logic;
signal Y2X0_48_m46 :  std_logic_vector(5 downto 0);
signal PP48X0Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w4_1 :  std_logic;
signal heap_bh44_w5_1 :  std_logic;
signal heap_bh44_w6_0 :  std_logic;
signal heap_bh44_w7_0 :  std_logic;
signal heap_bh44_w8_0 :  std_logic;
signal Xp_m46b63 :  std_logic_vector(17 downto 0);
signal Yp_m46b63 :  std_logic_vector(8 downto 0);
signal x_m46b63_0 :  std_logic_vector(2 downto 0);
signal x_m46b63_1 :  std_logic_vector(2 downto 0);
signal x_m46b63_2 :  std_logic_vector(2 downto 0);
signal x_m46b63_3 :  std_logic_vector(2 downto 0);
signal x_m46b63_4 :  std_logic_vector(2 downto 0);
signal x_m46b63_5 :  std_logic_vector(2 downto 0);
signal y_m46b63_0 :  std_logic_vector(2 downto 0);
signal y_m46b63_1 :  std_logic_vector(2 downto 0);
signal y_m46b63_2 :  std_logic_vector(2 downto 0);
signal Y0X2_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X2Y0_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_1, heap_bh44_w0_1_d1 :  std_logic;
signal Y0X3_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X3Y0_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_2 :  std_logic;
signal heap_bh44_w1_2 :  std_logic;
signal heap_bh44_w2_2 :  std_logic;
signal heap_bh44_w3_1 :  std_logic;
signal Y0X4_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X4Y0_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w1_3 :  std_logic;
signal heap_bh44_w2_3 :  std_logic;
signal heap_bh44_w3_2 :  std_logic;
signal heap_bh44_w4_2 :  std_logic;
signal heap_bh44_w5_2 :  std_logic;
signal heap_bh44_w6_1 :  std_logic;
signal Y0X5_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X5Y0_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w4_3 :  std_logic;
signal heap_bh44_w5_3 :  std_logic;
signal heap_bh44_w6_2 :  std_logic;
signal heap_bh44_w7_1 :  std_logic;
signal heap_bh44_w8_1 :  std_logic;
signal heap_bh44_w9_0 :  std_logic;
signal Y1X1_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X1Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_3 :  std_logic;
signal Y1X2_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X2Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_4 :  std_logic;
signal heap_bh44_w1_4 :  std_logic;
signal heap_bh44_w2_4 :  std_logic;
signal heap_bh44_w3_3 :  std_logic;
signal Y1X3_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X3Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w1_5 :  std_logic;
signal heap_bh44_w2_5 :  std_logic;
signal heap_bh44_w3_4 :  std_logic;
signal heap_bh44_w4_4 :  std_logic;
signal heap_bh44_w5_4 :  std_logic;
signal heap_bh44_w6_3 :  std_logic;
signal Y1X4_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X4Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w4_5 :  std_logic;
signal heap_bh44_w5_5 :  std_logic;
signal heap_bh44_w6_4 :  std_logic;
signal heap_bh44_w7_2 :  std_logic;
signal heap_bh44_w8_2 :  std_logic;
signal heap_bh44_w9_1 :  std_logic;
signal Y1X5_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X5Y1_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w7_3 :  std_logic;
signal heap_bh44_w8_3 :  std_logic;
signal heap_bh44_w9_2 :  std_logic;
signal heap_bh44_w10_0 :  std_logic;
signal heap_bh44_w11_0 :  std_logic;
signal heap_bh44_w12_0 :  std_logic;
signal Y2X0_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X0Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_5 :  std_logic;
signal Y2X1_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X1Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w0_6 :  std_logic;
signal heap_bh44_w1_6 :  std_logic;
signal heap_bh44_w2_6 :  std_logic;
signal heap_bh44_w3_5 :  std_logic;
signal Y2X2_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X2Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w1_7 :  std_logic;
signal heap_bh44_w2_7 :  std_logic;
signal heap_bh44_w3_6 :  std_logic;
signal heap_bh44_w4_6 :  std_logic;
signal heap_bh44_w5_6 :  std_logic;
signal heap_bh44_w6_5 :  std_logic;
signal Y2X3_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X3Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w4_7 :  std_logic;
signal heap_bh44_w5_7 :  std_logic;
signal heap_bh44_w6_6 :  std_logic;
signal heap_bh44_w7_4 :  std_logic;
signal heap_bh44_w8_4 :  std_logic;
signal heap_bh44_w9_3 :  std_logic;
signal Y2X4_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X4Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w7_5 :  std_logic;
signal heap_bh44_w8_5 :  std_logic;
signal heap_bh44_w9_4 :  std_logic;
signal heap_bh44_w10_1 :  std_logic;
signal heap_bh44_w11_1 :  std_logic;
signal heap_bh44_w12_1 :  std_logic;
signal Y2X5_63_m46 :  std_logic_vector(5 downto 0);
signal PP63X5Y2_m46 :  std_logic_vector(5 downto 0);
signal heap_bh44_w10_2 :  std_logic;
signal heap_bh44_w11_2 :  std_logic;
signal heap_bh44_w12_2 :  std_logic;
signal heap_bh44_w13_0 :  std_logic;
signal heap_bh44_w14_0 :  std_logic;
signal heap_bh44_w15_0 :  std_logic;
signal heap_bh44_w6_7 :  std_logic;
signal heap_bh44_w7_6 :  std_logic;
signal heap_bh44_w8_6 :  std_logic;
signal heap_bh44_w9_5 :  std_logic;
signal heap_bh44_w10_3 :  std_logic;
signal heap_bh44_w11_3 :  std_logic;
signal heap_bh44_w12_3 :  std_logic;
signal heap_bh44_w13_1 :  std_logic;
signal heap_bh44_w14_1 :  std_logic;
signal heap_bh44_w15_1 :  std_logic;
signal heap_bh44_w16_0 :  std_logic;
signal heap_bh44_w17_0 :  std_logic;
signal heap_bh44_w18_0, heap_bh44_w18_0_d1 :  std_logic;
signal heap_bh44_w19_0, heap_bh44_w19_0_d1 :  std_logic;
signal heap_bh44_w20_0, heap_bh44_w20_0_d1 :  std_logic;
signal heap_bh44_w21_0, heap_bh44_w21_0_d1 :  std_logic;
signal heap_bh44_w22_0, heap_bh44_w22_0_d1 :  std_logic;
signal heap_bh44_w23_0, heap_bh44_w23_0_d1 :  std_logic;
signal heap_bh44_w24_0, heap_bh44_w24_0_d1 :  std_logic;
signal heap_bh44_w25_0, heap_bh44_w25_0_d1 :  std_logic;
signal heap_bh44_w26_0, heap_bh44_w26_0_d1 :  std_logic;
signal heap_bh44_w27_0, heap_bh44_w27_0_d1 :  std_logic;
signal heap_bh44_w28_0, heap_bh44_w28_0_d1 :  std_logic;
signal heap_bh44_w29_0, heap_bh44_w29_0_d1 :  std_logic;
signal heap_bh44_w30_0, heap_bh44_w30_0_d1 :  std_logic;
signal heap_bh44_w31_0, heap_bh44_w31_0_d1 :  std_logic;
signal heap_bh44_w32_0, heap_bh44_w32_0_d1 :  std_logic;
signal heap_bh44_w33_0, heap_bh44_w33_0_d1 :  std_logic;
signal heap_bh44_w34_0, heap_bh44_w34_0_d1 :  std_logic;
signal heap_bh44_w35_0, heap_bh44_w35_0_d1 :  std_logic;
signal heap_bh44_w36_0, heap_bh44_w36_0_d1 :  std_logic;
signal heap_bh44_w37_0, heap_bh44_w37_0_d1 :  std_logic;
signal heap_bh44_w38_0, heap_bh44_w38_0_d1 :  std_logic;
signal heap_bh44_w39_0, heap_bh44_w39_0_d1 :  std_logic;
signal heap_bh44_w40_0, heap_bh44_w40_0_d1, heap_bh44_w40_0_d2 :  std_logic;
signal heap_bh44_w41_0, heap_bh44_w41_0_d1, heap_bh44_w41_0_d2 :  std_logic;
signal heap_bh44_w42_0, heap_bh44_w42_0_d1, heap_bh44_w42_0_d2 :  std_logic;
signal DSP_bh44_ch0_0 :  std_logic_vector(42 downto 0);
signal heap_bh44_w33_1, heap_bh44_w33_1_d1 :  std_logic;
signal heap_bh44_w32_1, heap_bh44_w32_1_d1 :  std_logic;
signal heap_bh44_w31_1, heap_bh44_w31_1_d1, heap_bh44_w31_1_d2 :  std_logic;
signal heap_bh44_w30_1, heap_bh44_w30_1_d1 :  std_logic;
signal heap_bh44_w29_1, heap_bh44_w29_1_d1, heap_bh44_w29_1_d2 :  std_logic;
signal heap_bh44_w28_1, heap_bh44_w28_1_d1 :  std_logic;
signal heap_bh44_w27_1, heap_bh44_w27_1_d1, heap_bh44_w27_1_d2 :  std_logic;
signal heap_bh44_w26_1, heap_bh44_w26_1_d1 :  std_logic;
signal heap_bh44_w25_1, heap_bh44_w25_1_d1, heap_bh44_w25_1_d2 :  std_logic;
signal heap_bh44_w24_1, heap_bh44_w24_1_d1 :  std_logic;
signal heap_bh44_w23_1, heap_bh44_w23_1_d1 :  std_logic;
signal heap_bh44_w22_1, heap_bh44_w22_1_d1 :  std_logic;
signal heap_bh44_w21_1, heap_bh44_w21_1_d1 :  std_logic;
signal heap_bh44_w20_1, heap_bh44_w20_1_d1 :  std_logic;
signal heap_bh44_w19_1, heap_bh44_w19_1_d1 :  std_logic;
signal heap_bh44_w18_1, heap_bh44_w18_1_d1 :  std_logic;
signal heap_bh44_w17_1, heap_bh44_w17_1_d1 :  std_logic;
signal heap_bh44_w16_1, heap_bh44_w16_1_d1 :  std_logic;
signal heap_bh44_w15_2, heap_bh44_w15_2_d1 :  std_logic;
signal heap_bh44_w14_2, heap_bh44_w14_2_d1 :  std_logic;
signal heap_bh44_w13_2, heap_bh44_w13_2_d1 :  std_logic;
signal heap_bh44_w12_4, heap_bh44_w12_4_d1 :  std_logic;
signal heap_bh44_w11_4, heap_bh44_w11_4_d1 :  std_logic;
signal heap_bh44_w10_4, heap_bh44_w10_4_d1 :  std_logic;
signal heap_bh44_w9_6, heap_bh44_w9_6_d1 :  std_logic;
signal heap_bh44_w8_7, heap_bh44_w8_7_d1 :  std_logic;
signal heap_bh44_w7_7, heap_bh44_w7_7_d1 :  std_logic;
signal heap_bh44_w6_8, heap_bh44_w6_8_d1 :  std_logic;
signal heap_bh44_w5_8, heap_bh44_w5_8_d1 :  std_logic;
signal heap_bh44_w4_8, heap_bh44_w4_8_d1 :  std_logic;
signal heap_bh44_w3_7, heap_bh44_w3_7_d1 :  std_logic;
signal heap_bh44_w2_8, heap_bh44_w2_8_d1 :  std_logic;
signal heap_bh44_w1_8, heap_bh44_w1_8_d1 :  std_logic;
signal heap_bh44_w0_7, heap_bh44_w0_7_d1 :  std_logic;
signal heap_bh44_w5_9 :  std_logic;
signal heap_bh44_w8_8 :  std_logic;
signal heap_bh44_w10_5 :  std_logic;
signal heap_bh44_w11_5 :  std_logic;
signal heap_bh44_w13_3 :  std_logic;
signal heap_bh44_w14_3 :  std_logic;
signal heap_bh44_w16_2 :  std_logic;
signal heap_bh44_w17_2 :  std_logic;
signal heap_bh44_w18_2, heap_bh44_w18_2_d1 :  std_logic;
signal heap_bh44_w19_2, heap_bh44_w19_2_d1 :  std_logic;
signal heap_bh44_w20_2, heap_bh44_w20_2_d1 :  std_logic;
signal heap_bh44_w21_2, heap_bh44_w21_2_d1 :  std_logic;
signal heap_bh44_w22_2, heap_bh44_w22_2_d1 :  std_logic;
signal heap_bh44_w23_2, heap_bh44_w23_2_d1 :  std_logic;
signal heap_bh44_w24_2, heap_bh44_w24_2_d1 :  std_logic;
signal heap_bh44_w25_2, heap_bh44_w25_2_d1 :  std_logic;
signal heap_bh44_w26_2, heap_bh44_w26_2_d1 :  std_logic;
signal heap_bh44_w27_2, heap_bh44_w27_2_d1 :  std_logic;
signal heap_bh44_w28_2, heap_bh44_w28_2_d1 :  std_logic;
signal heap_bh44_w29_2, heap_bh44_w29_2_d1 :  std_logic;
signal heap_bh44_w30_2, heap_bh44_w30_2_d1 :  std_logic;
signal heap_bh44_w31_2, heap_bh44_w31_2_d1 :  std_logic;
signal heap_bh44_w32_2, heap_bh44_w32_2_d1 :  std_logic;
signal heap_bh44_w34_1, heap_bh44_w34_1_d1 :  std_logic;
signal heap_bh44_w35_1, heap_bh44_w35_1_d1 :  std_logic;
signal heap_bh44_w36_1, heap_bh44_w36_1_d1 :  std_logic;
signal heap_bh44_w37_1, heap_bh44_w37_1_d1 :  std_logic;
signal heap_bh44_w38_1, heap_bh44_w38_1_d1 :  std_logic;
signal heap_bh44_w39_1, heap_bh44_w39_1_d1 :  std_logic;
signal heap_bh44_w40_1, heap_bh44_w40_1_d1, heap_bh44_w40_1_d2 :  std_logic;
signal heap_bh44_w41_1, heap_bh44_w41_1_d1, heap_bh44_w41_1_d2 :  std_logic;
signal heap_bh44_w42_1, heap_bh44_w42_1_d1, heap_bh44_w42_1_d2 :  std_logic;
signal CompressorIn_bh44_0_0 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh44_w0_8, heap_bh44_w0_8_d1 :  std_logic;
signal heap_bh44_w1_9 :  std_logic;
signal heap_bh44_w2_9 :  std_logic;
signal CompressorIn_bh44_1_1 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh44_w1_10 :  std_logic;
signal heap_bh44_w2_10 :  std_logic;
signal heap_bh44_w3_8 :  std_logic;
signal CompressorIn_bh44_2_2 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh44_w2_11 :  std_logic;
signal heap_bh44_w3_9 :  std_logic;
signal heap_bh44_w4_9 :  std_logic;
signal CompressorIn_bh44_3_3 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh44_w3_10 :  std_logic;
signal heap_bh44_w4_10 :  std_logic;
signal heap_bh44_w5_10 :  std_logic;
signal CompressorIn_bh44_4_4 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_4_4 :  std_logic_vector(2 downto 0);
signal heap_bh44_w4_11 :  std_logic;
signal heap_bh44_w5_11 :  std_logic;
signal heap_bh44_w6_9 :  std_logic;
signal CompressorIn_bh44_5_5 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_5_5 :  std_logic_vector(2 downto 0);
signal heap_bh44_w5_12 :  std_logic;
signal heap_bh44_w6_10 :  std_logic;
signal heap_bh44_w7_8 :  std_logic;
signal CompressorIn_bh44_6_6 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_6_6 :  std_logic_vector(2 downto 0);
signal heap_bh44_w6_11 :  std_logic;
signal heap_bh44_w7_9 :  std_logic;
signal heap_bh44_w8_9 :  std_logic;
signal CompressorIn_bh44_7_7 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_7_7 :  std_logic_vector(2 downto 0);
signal heap_bh44_w7_10 :  std_logic;
signal heap_bh44_w8_10 :  std_logic;
signal heap_bh44_w9_7, heap_bh44_w9_7_d1 :  std_logic;
signal CompressorIn_bh44_8_8 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_8_8 :  std_logic_vector(2 downto 0);
signal heap_bh44_w8_11 :  std_logic;
signal heap_bh44_w9_8, heap_bh44_w9_8_d1 :  std_logic;
signal heap_bh44_w10_6 :  std_logic;
signal CompressorIn_bh44_9_9 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh44_9_9 :  std_logic_vector(2 downto 0);
signal heap_bh44_w9_9 :  std_logic;
signal heap_bh44_w10_7 :  std_logic;
signal heap_bh44_w11_6, heap_bh44_w11_6_d1 :  std_logic;
signal CompressorIn_bh44_10_10 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_10_11 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_10_10 :  std_logic_vector(2 downto 0);
signal heap_bh44_w10_8 :  std_logic;
signal heap_bh44_w11_7, heap_bh44_w11_7_d1 :  std_logic;
signal heap_bh44_w12_5 :  std_logic;
signal CompressorIn_bh44_11_12 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_11_13 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_11_11 :  std_logic_vector(2 downto 0);
signal heap_bh44_w11_8 :  std_logic;
signal heap_bh44_w12_6 :  std_logic;
signal heap_bh44_w13_4, heap_bh44_w13_4_d1 :  std_logic;
signal CompressorIn_bh44_12_14 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_12_15 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_12_12 :  std_logic_vector(2 downto 0);
signal heap_bh44_w5_13 :  std_logic;
signal heap_bh44_w6_12 :  std_logic;
signal heap_bh44_w7_11 :  std_logic;
signal CompressorIn_bh44_13_16 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_13_17 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_13_13 :  std_logic_vector(2 downto 0);
signal heap_bh44_w12_7 :  std_logic;
signal heap_bh44_w13_5 :  std_logic;
signal heap_bh44_w14_4, heap_bh44_w14_4_d1 :  std_logic;
signal CompressorIn_bh44_14_18 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_14_19 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_14_14 :  std_logic_vector(2 downto 0);
signal heap_bh44_w14_5, heap_bh44_w14_5_d1 :  std_logic;
signal heap_bh44_w15_3, heap_bh44_w15_3_d1 :  std_logic;
signal heap_bh44_w16_3 :  std_logic;
signal CompressorIn_bh44_15_20 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_15_21 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_15_15 :  std_logic_vector(2 downto 0);
signal heap_bh44_w1_11, heap_bh44_w1_11_d1 :  std_logic;
signal heap_bh44_w2_12, heap_bh44_w2_12_d1 :  std_logic;
signal heap_bh44_w3_11, heap_bh44_w3_11_d1 :  std_logic;
signal CompressorIn_bh44_16_22 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_16_23 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_16_16 :  std_logic_vector(2 downto 0);
signal heap_bh44_w2_13, heap_bh44_w2_13_d1 :  std_logic;
signal heap_bh44_w3_12, heap_bh44_w3_12_d1 :  std_logic;
signal heap_bh44_w4_12, heap_bh44_w4_12_d1 :  std_logic;
signal CompressorIn_bh44_17_24 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_17_25 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_17_17 :  std_logic_vector(2 downto 0);
signal heap_bh44_w4_13, heap_bh44_w4_13_d1 :  std_logic;
signal heap_bh44_w5_14, heap_bh44_w5_14_d1 :  std_logic;
signal heap_bh44_w6_13, heap_bh44_w6_13_d1 :  std_logic;
signal CompressorIn_bh44_18_26 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_18_27 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_18_18 :  std_logic_vector(2 downto 0);
signal heap_bh44_w6_14, heap_bh44_w6_14_d1 :  std_logic;
signal heap_bh44_w7_12, heap_bh44_w7_12_d1 :  std_logic;
signal heap_bh44_w8_12, heap_bh44_w8_12_d1 :  std_logic;
signal CompressorIn_bh44_19_28 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_19_29 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_19_19 :  std_logic_vector(2 downto 0);
signal heap_bh44_w7_13, heap_bh44_w7_13_d1 :  std_logic;
signal heap_bh44_w8_13, heap_bh44_w8_13_d1 :  std_logic;
signal heap_bh44_w9_10, heap_bh44_w9_10_d1 :  std_logic;
signal CompressorIn_bh44_20_30 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_20_31 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_20_20 :  std_logic_vector(2 downto 0);
signal heap_bh44_w8_14, heap_bh44_w8_14_d1 :  std_logic;
signal heap_bh44_w9_11, heap_bh44_w9_11_d1 :  std_logic;
signal heap_bh44_w10_9, heap_bh44_w10_9_d1 :  std_logic;
signal CompressorIn_bh44_21_32 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_21_33 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_21_21 :  std_logic_vector(2 downto 0);
signal heap_bh44_w10_10, heap_bh44_w10_10_d1 :  std_logic;
signal heap_bh44_w11_9, heap_bh44_w11_9_d1 :  std_logic;
signal heap_bh44_w12_8, heap_bh44_w12_8_d1 :  std_logic;
signal CompressorIn_bh44_22_34 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_22_35 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_22_22 :  std_logic_vector(2 downto 0);
signal heap_bh44_w12_9, heap_bh44_w12_9_d1 :  std_logic;
signal heap_bh44_w13_6, heap_bh44_w13_6_d1 :  std_logic;
signal heap_bh44_w14_6, heap_bh44_w14_6_d1 :  std_logic;
signal CompressorIn_bh44_23_36 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_23_37 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_23_23 :  std_logic_vector(2 downto 0);
signal heap_bh44_w16_4, heap_bh44_w16_4_d1 :  std_logic;
signal heap_bh44_w17_3, heap_bh44_w17_3_d1 :  std_logic;
signal heap_bh44_w18_3, heap_bh44_w18_3_d1 :  std_logic;
signal CompressorIn_bh44_24_38 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_24_39 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_24_24 :  std_logic_vector(2 downto 0);
signal heap_bh44_w3_13, heap_bh44_w3_13_d1 :  std_logic;
signal heap_bh44_w4_14, heap_bh44_w4_14_d1 :  std_logic;
signal heap_bh44_w5_15, heap_bh44_w5_15_d1 :  std_logic;
signal CompressorIn_bh44_25_40 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh44_25_25 :  std_logic_vector(1 downto 0);
signal heap_bh44_w5_16, heap_bh44_w5_16_d1 :  std_logic;
signal heap_bh44_w6_15, heap_bh44_w6_15_d1 :  std_logic;
signal CompressorIn_bh44_26_41 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_26_42 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_26_26 :  std_logic_vector(2 downto 0);
signal heap_bh44_w9_12 :  std_logic;
signal heap_bh44_w10_11, heap_bh44_w10_11_d1, heap_bh44_w10_11_d2 :  std_logic;
signal heap_bh44_w11_10 :  std_logic;
signal CompressorIn_bh44_27_43 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_27_44 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_27_27 :  std_logic_vector(2 downto 0);
signal heap_bh44_w3_14 :  std_logic;
signal heap_bh44_w4_15 :  std_logic;
signal heap_bh44_w5_17, heap_bh44_w5_17_d1, heap_bh44_w5_17_d2 :  std_logic;
signal CompressorIn_bh44_28_45 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_28_46 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_28_28 :  std_logic_vector(2 downto 0);
signal heap_bh44_w5_18 :  std_logic;
signal heap_bh44_w6_16 :  std_logic;
signal heap_bh44_w7_14 :  std_logic;
signal CompressorIn_bh44_29_47 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_29_48 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_29_29 :  std_logic_vector(2 downto 0);
signal heap_bh44_w11_11 :  std_logic;
signal heap_bh44_w12_10 :  std_logic;
signal heap_bh44_w13_7 :  std_logic;
signal CompressorIn_bh44_30_49 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_30_50 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_30_30 :  std_logic_vector(2 downto 0);
signal heap_bh44_w18_4 :  std_logic;
signal heap_bh44_w19_3 :  std_logic;
signal heap_bh44_w20_3 :  std_logic;
signal CompressorIn_bh44_31_51 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_31_52 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_31_31 :  std_logic_vector(2 downto 0);
signal heap_bh44_w14_7, heap_bh44_w14_7_d1, heap_bh44_w14_7_d2 :  std_logic;
signal heap_bh44_w15_4 :  std_logic;
signal heap_bh44_w16_5 :  std_logic;
signal CompressorIn_bh44_32_53 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh44_32_32 :  std_logic_vector(1 downto 0);
signal heap_bh44_w8_15, heap_bh44_w8_15_d1, heap_bh44_w8_15_d2 :  std_logic;
signal heap_bh44_w9_13 :  std_logic;
signal CompressorIn_bh44_33_54 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_33_55 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_33_33 :  std_logic_vector(2 downto 0);
signal heap_bh44_w7_15, heap_bh44_w7_15_d1, heap_bh44_w7_15_d2 :  std_logic;
signal heap_bh44_w8_16, heap_bh44_w8_16_d1, heap_bh44_w8_16_d2 :  std_logic;
signal heap_bh44_w9_14, heap_bh44_w9_14_d1, heap_bh44_w9_14_d2 :  std_logic;
signal CompressorIn_bh44_34_56 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_34_57 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_34_34 :  std_logic_vector(2 downto 0);
signal heap_bh44_w13_8, heap_bh44_w13_8_d1, heap_bh44_w13_8_d2 :  std_logic;
signal heap_bh44_w14_8, heap_bh44_w14_8_d1, heap_bh44_w14_8_d2 :  std_logic;
signal heap_bh44_w15_5 :  std_logic;
signal CompressorIn_bh44_35_58 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh44_35_59 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_35_35 :  std_logic_vector(2 downto 0);
signal heap_bh44_w20_4, heap_bh44_w20_4_d1, heap_bh44_w20_4_d2 :  std_logic;
signal heap_bh44_w21_3 :  std_logic;
signal heap_bh44_w22_3 :  std_logic;
signal CompressorIn_bh44_36_60 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_36_61 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_36_36 :  std_logic_vector(2 downto 0);
signal heap_bh44_w0_9 :  std_logic;
signal heap_bh44_w1_12 :  std_logic;
signal heap_bh44_w2_14, heap_bh44_w2_14_d1, heap_bh44_w2_14_d2 :  std_logic;
signal CompressorIn_bh44_37_62 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_37_63 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_37_37 :  std_logic_vector(2 downto 0);
signal heap_bh44_w2_15, heap_bh44_w2_15_d1, heap_bh44_w2_15_d2 :  std_logic;
signal heap_bh44_w3_15, heap_bh44_w3_15_d1, heap_bh44_w3_15_d2 :  std_logic;
signal heap_bh44_w4_16, heap_bh44_w4_16_d1, heap_bh44_w4_16_d2 :  std_logic;
signal CompressorIn_bh44_38_64 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_38_65 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_38_38 :  std_logic_vector(2 downto 0);
signal heap_bh44_w4_17, heap_bh44_w4_17_d1, heap_bh44_w4_17_d2 :  std_logic;
signal heap_bh44_w5_19, heap_bh44_w5_19_d1, heap_bh44_w5_19_d2 :  std_logic;
signal heap_bh44_w6_17, heap_bh44_w6_17_d1, heap_bh44_w6_17_d2 :  std_logic;
signal CompressorIn_bh44_39_66 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_39_67 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_39_39 :  std_logic_vector(2 downto 0);
signal heap_bh44_w9_15, heap_bh44_w9_15_d1, heap_bh44_w9_15_d2 :  std_logic;
signal heap_bh44_w10_12, heap_bh44_w10_12_d1, heap_bh44_w10_12_d2 :  std_logic;
signal heap_bh44_w11_12, heap_bh44_w11_12_d1, heap_bh44_w11_12_d2 :  std_logic;
signal CompressorIn_bh44_40_68 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_40_69 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_40_40 :  std_logic_vector(2 downto 0);
signal heap_bh44_w11_13, heap_bh44_w11_13_d1, heap_bh44_w11_13_d2 :  std_logic;
signal heap_bh44_w12_11, heap_bh44_w12_11_d1, heap_bh44_w12_11_d2 :  std_logic;
signal heap_bh44_w13_9, heap_bh44_w13_9_d1, heap_bh44_w13_9_d2 :  std_logic;
signal CompressorIn_bh44_41_70 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_41_71 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_41_41 :  std_logic_vector(2 downto 0);
signal heap_bh44_w16_6 :  std_logic;
signal heap_bh44_w17_4, heap_bh44_w17_4_d1, heap_bh44_w17_4_d2 :  std_logic;
signal heap_bh44_w18_5 :  std_logic;
signal CompressorIn_bh44_42_72 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_42_73 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_42_42 :  std_logic_vector(2 downto 0);
signal heap_bh44_w22_4 :  std_logic;
signal heap_bh44_w23_3 :  std_logic;
signal heap_bh44_w24_3 :  std_logic;
signal CompressorIn_bh44_43_74 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_43_75 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_43_43 :  std_logic_vector(2 downto 0);
signal heap_bh44_w24_4 :  std_logic;
signal heap_bh44_w25_3, heap_bh44_w25_3_d1 :  std_logic;
signal heap_bh44_w26_3, heap_bh44_w26_3_d1 :  std_logic;
signal CompressorIn_bh44_44_76 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_44_77 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_44_44 :  std_logic_vector(2 downto 0);
signal heap_bh44_w26_4, heap_bh44_w26_4_d1 :  std_logic;
signal heap_bh44_w27_3, heap_bh44_w27_3_d1 :  std_logic;
signal heap_bh44_w28_3, heap_bh44_w28_3_d1 :  std_logic;
signal CompressorIn_bh44_45_78 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_45_79 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_45_45 :  std_logic_vector(2 downto 0);
signal heap_bh44_w28_4, heap_bh44_w28_4_d1 :  std_logic;
signal heap_bh44_w29_3, heap_bh44_w29_3_d1 :  std_logic;
signal heap_bh44_w30_3, heap_bh44_w30_3_d1 :  std_logic;
signal CompressorIn_bh44_46_80 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_46_81 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_46_46 :  std_logic_vector(2 downto 0);
signal heap_bh44_w30_4, heap_bh44_w30_4_d1 :  std_logic;
signal heap_bh44_w31_3, heap_bh44_w31_3_d1 :  std_logic;
signal heap_bh44_w32_3, heap_bh44_w32_3_d1 :  std_logic;
signal CompressorIn_bh44_47_82 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_47_83 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_47_47 :  std_logic_vector(2 downto 0);
signal heap_bh44_w32_4, heap_bh44_w32_4_d1 :  std_logic;
signal heap_bh44_w33_2, heap_bh44_w33_2_d1, heap_bh44_w33_2_d2 :  std_logic;
signal heap_bh44_w34_2 :  std_logic;
signal CompressorIn_bh44_48_84 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh44_48_48 :  std_logic_vector(1 downto 0);
signal heap_bh44_w6_18, heap_bh44_w6_18_d1, heap_bh44_w6_18_d2 :  std_logic;
signal heap_bh44_w7_16, heap_bh44_w7_16_d1, heap_bh44_w7_16_d2 :  std_logic;
signal tempR_bh44_0, tempR_bh44_0_d1, tempR_bh44_0_d2 :  std_logic_vector(1 downto 0);
signal CompressorIn_bh44_49_85 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_49_86 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_49_49 :  std_logic_vector(2 downto 0);
signal heap_bh44_w18_6, heap_bh44_w18_6_d1, heap_bh44_w18_6_d2 :  std_logic;
signal heap_bh44_w19_4, heap_bh44_w19_4_d1, heap_bh44_w19_4_d2 :  std_logic;
signal heap_bh44_w20_5, heap_bh44_w20_5_d1, heap_bh44_w20_5_d2 :  std_logic;
signal CompressorIn_bh44_50_87 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_50_88 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_50_50 :  std_logic_vector(2 downto 0);
signal heap_bh44_w21_4, heap_bh44_w21_4_d1, heap_bh44_w21_4_d2 :  std_logic;
signal heap_bh44_w22_5, heap_bh44_w22_5_d1, heap_bh44_w22_5_d2 :  std_logic;
signal heap_bh44_w23_4 :  std_logic;
signal CompressorIn_bh44_51_89 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_51_90 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_51_51 :  std_logic_vector(2 downto 0);
signal heap_bh44_w34_3, heap_bh44_w34_3_d1, heap_bh44_w34_3_d2 :  std_logic;
signal heap_bh44_w35_2, heap_bh44_w35_2_d1, heap_bh44_w35_2_d2 :  std_logic;
signal heap_bh44_w36_2 :  std_logic;
signal CompressorIn_bh44_52_91 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_52_92 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh44_52_52 :  std_logic_vector(2 downto 0);
signal heap_bh44_w15_6, heap_bh44_w15_6_d1, heap_bh44_w15_6_d2 :  std_logic;
signal heap_bh44_w16_7, heap_bh44_w16_7_d1, heap_bh44_w16_7_d2 :  std_logic;
signal heap_bh44_w17_5, heap_bh44_w17_5_d1, heap_bh44_w17_5_d2 :  std_logic;
signal CompressorIn_bh44_53_93 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_53_94 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_53_53 :  std_logic_vector(2 downto 0);
signal heap_bh44_w23_5, heap_bh44_w23_5_d1, heap_bh44_w23_5_d2 :  std_logic;
signal heap_bh44_w24_5, heap_bh44_w24_5_d1, heap_bh44_w24_5_d2 :  std_logic;
signal heap_bh44_w25_4, heap_bh44_w25_4_d1 :  std_logic;
signal CompressorIn_bh44_54_95 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_54_96 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_54_54 :  std_logic_vector(2 downto 0);
signal heap_bh44_w36_3, heap_bh44_w36_3_d1, heap_bh44_w36_3_d2 :  std_logic;
signal heap_bh44_w37_2, heap_bh44_w37_2_d1, heap_bh44_w37_2_d2 :  std_logic;
signal heap_bh44_w38_2 :  std_logic;
signal CompressorIn_bh44_55_97 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_55_98 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_55_55 :  std_logic_vector(2 downto 0);
signal heap_bh44_w25_5, heap_bh44_w25_5_d1 :  std_logic;
signal heap_bh44_w26_5, heap_bh44_w26_5_d1 :  std_logic;
signal heap_bh44_w27_4 :  std_logic;
signal CompressorIn_bh44_56_99 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_56_100 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_56_56 :  std_logic_vector(2 downto 0);
signal heap_bh44_w38_3, heap_bh44_w38_3_d1, heap_bh44_w38_3_d2 :  std_logic;
signal heap_bh44_w39_2, heap_bh44_w39_2_d1, heap_bh44_w39_2_d2 :  std_logic;
signal heap_bh44_w40_2, heap_bh44_w40_2_d1 :  std_logic;
signal CompressorIn_bh44_57_101 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_57_102 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_57_57 :  std_logic_vector(2 downto 0);
signal heap_bh44_w27_5, heap_bh44_w27_5_d1 :  std_logic;
signal heap_bh44_w28_5, heap_bh44_w28_5_d1 :  std_logic;
signal heap_bh44_w29_4 :  std_logic;
signal CompressorIn_bh44_58_103 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_58_104 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_58_58 :  std_logic_vector(2 downto 0);
signal heap_bh44_w40_3, heap_bh44_w40_3_d1 :  std_logic;
signal heap_bh44_w41_2, heap_bh44_w41_2_d1 :  std_logic;
signal heap_bh44_w42_2 :  std_logic;
signal CompressorIn_bh44_59_105 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_59_106 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_59_59 :  std_logic_vector(2 downto 0);
signal heap_bh44_w29_5, heap_bh44_w29_5_d1 :  std_logic;
signal heap_bh44_w30_5, heap_bh44_w30_5_d1 :  std_logic;
signal heap_bh44_w31_4 :  std_logic;
signal CompressorIn_bh44_60_107 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh44_60_60 :  std_logic_vector(1 downto 0);
signal heap_bh44_w42_3, heap_bh44_w42_3_d1 :  std_logic;
signal CompressorIn_bh44_61_108 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh44_61_109 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh44_61_61 :  std_logic_vector(2 downto 0);
signal heap_bh44_w31_5, heap_bh44_w31_5_d1 :  std_logic;
signal heap_bh44_w32_5, heap_bh44_w32_5_d1 :  std_logic;
signal heap_bh44_w33_3, heap_bh44_w33_3_d1 :  std_logic;
signal finalAdderIn0_bh44 :  std_logic_vector(41 downto 0);
signal finalAdderIn1_bh44 :  std_logic_vector(41 downto 0);
signal finalAdderCin_bh44 :  std_logic;
signal finalAdderOut_bh44 :  std_logic_vector(41 downto 0);
signal CompressionResult44 :  std_logic_vector(43 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of SmallMultTableP3x3r6XsYu_F400_uid67: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYs_F400_uid54: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid50: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid65: component is "yes";
attribute rom_style of SmallMultTableP3x3r6XsYu_F400_uid67: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYs_F400_uid54: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid50: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid65: component is "distributed";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            heap_bh44_w0_1_d1 <=  heap_bh44_w0_1;
            heap_bh44_w18_0_d1 <=  heap_bh44_w18_0;
            heap_bh44_w19_0_d1 <=  heap_bh44_w19_0;
            heap_bh44_w20_0_d1 <=  heap_bh44_w20_0;
            heap_bh44_w21_0_d1 <=  heap_bh44_w21_0;
            heap_bh44_w22_0_d1 <=  heap_bh44_w22_0;
            heap_bh44_w23_0_d1 <=  heap_bh44_w23_0;
            heap_bh44_w24_0_d1 <=  heap_bh44_w24_0;
            heap_bh44_w25_0_d1 <=  heap_bh44_w25_0;
            heap_bh44_w26_0_d1 <=  heap_bh44_w26_0;
            heap_bh44_w27_0_d1 <=  heap_bh44_w27_0;
            heap_bh44_w28_0_d1 <=  heap_bh44_w28_0;
            heap_bh44_w29_0_d1 <=  heap_bh44_w29_0;
            heap_bh44_w30_0_d1 <=  heap_bh44_w30_0;
            heap_bh44_w31_0_d1 <=  heap_bh44_w31_0;
            heap_bh44_w32_0_d1 <=  heap_bh44_w32_0;
            heap_bh44_w33_0_d1 <=  heap_bh44_w33_0;
            heap_bh44_w34_0_d1 <=  heap_bh44_w34_0;
            heap_bh44_w35_0_d1 <=  heap_bh44_w35_0;
            heap_bh44_w36_0_d1 <=  heap_bh44_w36_0;
            heap_bh44_w37_0_d1 <=  heap_bh44_w37_0;
            heap_bh44_w38_0_d1 <=  heap_bh44_w38_0;
            heap_bh44_w39_0_d1 <=  heap_bh44_w39_0;
            heap_bh44_w40_0_d1 <=  heap_bh44_w40_0;
            heap_bh44_w40_0_d2 <=  heap_bh44_w40_0_d1;
            heap_bh44_w41_0_d1 <=  heap_bh44_w41_0;
            heap_bh44_w41_0_d2 <=  heap_bh44_w41_0_d1;
            heap_bh44_w42_0_d1 <=  heap_bh44_w42_0;
            heap_bh44_w42_0_d2 <=  heap_bh44_w42_0_d1;
            heap_bh44_w33_1_d1 <=  heap_bh44_w33_1;
            heap_bh44_w32_1_d1 <=  heap_bh44_w32_1;
            heap_bh44_w31_1_d1 <=  heap_bh44_w31_1;
            heap_bh44_w31_1_d2 <=  heap_bh44_w31_1_d1;
            heap_bh44_w30_1_d1 <=  heap_bh44_w30_1;
            heap_bh44_w29_1_d1 <=  heap_bh44_w29_1;
            heap_bh44_w29_1_d2 <=  heap_bh44_w29_1_d1;
            heap_bh44_w28_1_d1 <=  heap_bh44_w28_1;
            heap_bh44_w27_1_d1 <=  heap_bh44_w27_1;
            heap_bh44_w27_1_d2 <=  heap_bh44_w27_1_d1;
            heap_bh44_w26_1_d1 <=  heap_bh44_w26_1;
            heap_bh44_w25_1_d1 <=  heap_bh44_w25_1;
            heap_bh44_w25_1_d2 <=  heap_bh44_w25_1_d1;
            heap_bh44_w24_1_d1 <=  heap_bh44_w24_1;
            heap_bh44_w23_1_d1 <=  heap_bh44_w23_1;
            heap_bh44_w22_1_d1 <=  heap_bh44_w22_1;
            heap_bh44_w21_1_d1 <=  heap_bh44_w21_1;
            heap_bh44_w20_1_d1 <=  heap_bh44_w20_1;
            heap_bh44_w19_1_d1 <=  heap_bh44_w19_1;
            heap_bh44_w18_1_d1 <=  heap_bh44_w18_1;
            heap_bh44_w17_1_d1 <=  heap_bh44_w17_1;
            heap_bh44_w16_1_d1 <=  heap_bh44_w16_1;
            heap_bh44_w15_2_d1 <=  heap_bh44_w15_2;
            heap_bh44_w14_2_d1 <=  heap_bh44_w14_2;
            heap_bh44_w13_2_d1 <=  heap_bh44_w13_2;
            heap_bh44_w12_4_d1 <=  heap_bh44_w12_4;
            heap_bh44_w11_4_d1 <=  heap_bh44_w11_4;
            heap_bh44_w10_4_d1 <=  heap_bh44_w10_4;
            heap_bh44_w9_6_d1 <=  heap_bh44_w9_6;
            heap_bh44_w8_7_d1 <=  heap_bh44_w8_7;
            heap_bh44_w7_7_d1 <=  heap_bh44_w7_7;
            heap_bh44_w6_8_d1 <=  heap_bh44_w6_8;
            heap_bh44_w5_8_d1 <=  heap_bh44_w5_8;
            heap_bh44_w4_8_d1 <=  heap_bh44_w4_8;
            heap_bh44_w3_7_d1 <=  heap_bh44_w3_7;
            heap_bh44_w2_8_d1 <=  heap_bh44_w2_8;
            heap_bh44_w1_8_d1 <=  heap_bh44_w1_8;
            heap_bh44_w0_7_d1 <=  heap_bh44_w0_7;
            heap_bh44_w18_2_d1 <=  heap_bh44_w18_2;
            heap_bh44_w19_2_d1 <=  heap_bh44_w19_2;
            heap_bh44_w20_2_d1 <=  heap_bh44_w20_2;
            heap_bh44_w21_2_d1 <=  heap_bh44_w21_2;
            heap_bh44_w22_2_d1 <=  heap_bh44_w22_2;
            heap_bh44_w23_2_d1 <=  heap_bh44_w23_2;
            heap_bh44_w24_2_d1 <=  heap_bh44_w24_2;
            heap_bh44_w25_2_d1 <=  heap_bh44_w25_2;
            heap_bh44_w26_2_d1 <=  heap_bh44_w26_2;
            heap_bh44_w27_2_d1 <=  heap_bh44_w27_2;
            heap_bh44_w28_2_d1 <=  heap_bh44_w28_2;
            heap_bh44_w29_2_d1 <=  heap_bh44_w29_2;
            heap_bh44_w30_2_d1 <=  heap_bh44_w30_2;
            heap_bh44_w31_2_d1 <=  heap_bh44_w31_2;
            heap_bh44_w32_2_d1 <=  heap_bh44_w32_2;
            heap_bh44_w34_1_d1 <=  heap_bh44_w34_1;
            heap_bh44_w35_1_d1 <=  heap_bh44_w35_1;
            heap_bh44_w36_1_d1 <=  heap_bh44_w36_1;
            heap_bh44_w37_1_d1 <=  heap_bh44_w37_1;
            heap_bh44_w38_1_d1 <=  heap_bh44_w38_1;
            heap_bh44_w39_1_d1 <=  heap_bh44_w39_1;
            heap_bh44_w40_1_d1 <=  heap_bh44_w40_1;
            heap_bh44_w40_1_d2 <=  heap_bh44_w40_1_d1;
            heap_bh44_w41_1_d1 <=  heap_bh44_w41_1;
            heap_bh44_w41_1_d2 <=  heap_bh44_w41_1_d1;
            heap_bh44_w42_1_d1 <=  heap_bh44_w42_1;
            heap_bh44_w42_1_d2 <=  heap_bh44_w42_1_d1;
            heap_bh44_w0_8_d1 <=  heap_bh44_w0_8;
            heap_bh44_w9_7_d1 <=  heap_bh44_w9_7;
            heap_bh44_w9_8_d1 <=  heap_bh44_w9_8;
            heap_bh44_w11_6_d1 <=  heap_bh44_w11_6;
            heap_bh44_w11_7_d1 <=  heap_bh44_w11_7;
            heap_bh44_w13_4_d1 <=  heap_bh44_w13_4;
            heap_bh44_w14_4_d1 <=  heap_bh44_w14_4;
            heap_bh44_w14_5_d1 <=  heap_bh44_w14_5;
            heap_bh44_w15_3_d1 <=  heap_bh44_w15_3;
            heap_bh44_w1_11_d1 <=  heap_bh44_w1_11;
            heap_bh44_w2_12_d1 <=  heap_bh44_w2_12;
            heap_bh44_w3_11_d1 <=  heap_bh44_w3_11;
            heap_bh44_w2_13_d1 <=  heap_bh44_w2_13;
            heap_bh44_w3_12_d1 <=  heap_bh44_w3_12;
            heap_bh44_w4_12_d1 <=  heap_bh44_w4_12;
            heap_bh44_w4_13_d1 <=  heap_bh44_w4_13;
            heap_bh44_w5_14_d1 <=  heap_bh44_w5_14;
            heap_bh44_w6_13_d1 <=  heap_bh44_w6_13;
            heap_bh44_w6_14_d1 <=  heap_bh44_w6_14;
            heap_bh44_w7_12_d1 <=  heap_bh44_w7_12;
            heap_bh44_w8_12_d1 <=  heap_bh44_w8_12;
            heap_bh44_w7_13_d1 <=  heap_bh44_w7_13;
            heap_bh44_w8_13_d1 <=  heap_bh44_w8_13;
            heap_bh44_w9_10_d1 <=  heap_bh44_w9_10;
            heap_bh44_w8_14_d1 <=  heap_bh44_w8_14;
            heap_bh44_w9_11_d1 <=  heap_bh44_w9_11;
            heap_bh44_w10_9_d1 <=  heap_bh44_w10_9;
            heap_bh44_w10_10_d1 <=  heap_bh44_w10_10;
            heap_bh44_w11_9_d1 <=  heap_bh44_w11_9;
            heap_bh44_w12_8_d1 <=  heap_bh44_w12_8;
            heap_bh44_w12_9_d1 <=  heap_bh44_w12_9;
            heap_bh44_w13_6_d1 <=  heap_bh44_w13_6;
            heap_bh44_w14_6_d1 <=  heap_bh44_w14_6;
            heap_bh44_w16_4_d1 <=  heap_bh44_w16_4;
            heap_bh44_w17_3_d1 <=  heap_bh44_w17_3;
            heap_bh44_w18_3_d1 <=  heap_bh44_w18_3;
            heap_bh44_w3_13_d1 <=  heap_bh44_w3_13;
            heap_bh44_w4_14_d1 <=  heap_bh44_w4_14;
            heap_bh44_w5_15_d1 <=  heap_bh44_w5_15;
            heap_bh44_w5_16_d1 <=  heap_bh44_w5_16;
            heap_bh44_w6_15_d1 <=  heap_bh44_w6_15;
            heap_bh44_w10_11_d1 <=  heap_bh44_w10_11;
            heap_bh44_w10_11_d2 <=  heap_bh44_w10_11_d1;
            heap_bh44_w5_17_d1 <=  heap_bh44_w5_17;
            heap_bh44_w5_17_d2 <=  heap_bh44_w5_17_d1;
            heap_bh44_w14_7_d1 <=  heap_bh44_w14_7;
            heap_bh44_w14_7_d2 <=  heap_bh44_w14_7_d1;
            heap_bh44_w8_15_d1 <=  heap_bh44_w8_15;
            heap_bh44_w8_15_d2 <=  heap_bh44_w8_15_d1;
            heap_bh44_w7_15_d1 <=  heap_bh44_w7_15;
            heap_bh44_w7_15_d2 <=  heap_bh44_w7_15_d1;
            heap_bh44_w8_16_d1 <=  heap_bh44_w8_16;
            heap_bh44_w8_16_d2 <=  heap_bh44_w8_16_d1;
            heap_bh44_w9_14_d1 <=  heap_bh44_w9_14;
            heap_bh44_w9_14_d2 <=  heap_bh44_w9_14_d1;
            heap_bh44_w13_8_d1 <=  heap_bh44_w13_8;
            heap_bh44_w13_8_d2 <=  heap_bh44_w13_8_d1;
            heap_bh44_w14_8_d1 <=  heap_bh44_w14_8;
            heap_bh44_w14_8_d2 <=  heap_bh44_w14_8_d1;
            heap_bh44_w20_4_d1 <=  heap_bh44_w20_4;
            heap_bh44_w20_4_d2 <=  heap_bh44_w20_4_d1;
            heap_bh44_w2_14_d1 <=  heap_bh44_w2_14;
            heap_bh44_w2_14_d2 <=  heap_bh44_w2_14_d1;
            heap_bh44_w2_15_d1 <=  heap_bh44_w2_15;
            heap_bh44_w2_15_d2 <=  heap_bh44_w2_15_d1;
            heap_bh44_w3_15_d1 <=  heap_bh44_w3_15;
            heap_bh44_w3_15_d2 <=  heap_bh44_w3_15_d1;
            heap_bh44_w4_16_d1 <=  heap_bh44_w4_16;
            heap_bh44_w4_16_d2 <=  heap_bh44_w4_16_d1;
            heap_bh44_w4_17_d1 <=  heap_bh44_w4_17;
            heap_bh44_w4_17_d2 <=  heap_bh44_w4_17_d1;
            heap_bh44_w5_19_d1 <=  heap_bh44_w5_19;
            heap_bh44_w5_19_d2 <=  heap_bh44_w5_19_d1;
            heap_bh44_w6_17_d1 <=  heap_bh44_w6_17;
            heap_bh44_w6_17_d2 <=  heap_bh44_w6_17_d1;
            heap_bh44_w9_15_d1 <=  heap_bh44_w9_15;
            heap_bh44_w9_15_d2 <=  heap_bh44_w9_15_d1;
            heap_bh44_w10_12_d1 <=  heap_bh44_w10_12;
            heap_bh44_w10_12_d2 <=  heap_bh44_w10_12_d1;
            heap_bh44_w11_12_d1 <=  heap_bh44_w11_12;
            heap_bh44_w11_12_d2 <=  heap_bh44_w11_12_d1;
            heap_bh44_w11_13_d1 <=  heap_bh44_w11_13;
            heap_bh44_w11_13_d2 <=  heap_bh44_w11_13_d1;
            heap_bh44_w12_11_d1 <=  heap_bh44_w12_11;
            heap_bh44_w12_11_d2 <=  heap_bh44_w12_11_d1;
            heap_bh44_w13_9_d1 <=  heap_bh44_w13_9;
            heap_bh44_w13_9_d2 <=  heap_bh44_w13_9_d1;
            heap_bh44_w17_4_d1 <=  heap_bh44_w17_4;
            heap_bh44_w17_4_d2 <=  heap_bh44_w17_4_d1;
            heap_bh44_w25_3_d1 <=  heap_bh44_w25_3;
            heap_bh44_w26_3_d1 <=  heap_bh44_w26_3;
            heap_bh44_w26_4_d1 <=  heap_bh44_w26_4;
            heap_bh44_w27_3_d1 <=  heap_bh44_w27_3;
            heap_bh44_w28_3_d1 <=  heap_bh44_w28_3;
            heap_bh44_w28_4_d1 <=  heap_bh44_w28_4;
            heap_bh44_w29_3_d1 <=  heap_bh44_w29_3;
            heap_bh44_w30_3_d1 <=  heap_bh44_w30_3;
            heap_bh44_w30_4_d1 <=  heap_bh44_w30_4;
            heap_bh44_w31_3_d1 <=  heap_bh44_w31_3;
            heap_bh44_w32_3_d1 <=  heap_bh44_w32_3;
            heap_bh44_w32_4_d1 <=  heap_bh44_w32_4;
            heap_bh44_w33_2_d1 <=  heap_bh44_w33_2;
            heap_bh44_w33_2_d2 <=  heap_bh44_w33_2_d1;
            heap_bh44_w6_18_d1 <=  heap_bh44_w6_18;
            heap_bh44_w6_18_d2 <=  heap_bh44_w6_18_d1;
            heap_bh44_w7_16_d1 <=  heap_bh44_w7_16;
            heap_bh44_w7_16_d2 <=  heap_bh44_w7_16_d1;
            tempR_bh44_0_d1 <=  tempR_bh44_0;
            tempR_bh44_0_d2 <=  tempR_bh44_0_d1;
            heap_bh44_w18_6_d1 <=  heap_bh44_w18_6;
            heap_bh44_w18_6_d2 <=  heap_bh44_w18_6_d1;
            heap_bh44_w19_4_d1 <=  heap_bh44_w19_4;
            heap_bh44_w19_4_d2 <=  heap_bh44_w19_4_d1;
            heap_bh44_w20_5_d1 <=  heap_bh44_w20_5;
            heap_bh44_w20_5_d2 <=  heap_bh44_w20_5_d1;
            heap_bh44_w21_4_d1 <=  heap_bh44_w21_4;
            heap_bh44_w21_4_d2 <=  heap_bh44_w21_4_d1;
            heap_bh44_w22_5_d1 <=  heap_bh44_w22_5;
            heap_bh44_w22_5_d2 <=  heap_bh44_w22_5_d1;
            heap_bh44_w34_3_d1 <=  heap_bh44_w34_3;
            heap_bh44_w34_3_d2 <=  heap_bh44_w34_3_d1;
            heap_bh44_w35_2_d1 <=  heap_bh44_w35_2;
            heap_bh44_w35_2_d2 <=  heap_bh44_w35_2_d1;
            heap_bh44_w15_6_d1 <=  heap_bh44_w15_6;
            heap_bh44_w15_6_d2 <=  heap_bh44_w15_6_d1;
            heap_bh44_w16_7_d1 <=  heap_bh44_w16_7;
            heap_bh44_w16_7_d2 <=  heap_bh44_w16_7_d1;
            heap_bh44_w17_5_d1 <=  heap_bh44_w17_5;
            heap_bh44_w17_5_d2 <=  heap_bh44_w17_5_d1;
            heap_bh44_w23_5_d1 <=  heap_bh44_w23_5;
            heap_bh44_w23_5_d2 <=  heap_bh44_w23_5_d1;
            heap_bh44_w24_5_d1 <=  heap_bh44_w24_5;
            heap_bh44_w24_5_d2 <=  heap_bh44_w24_5_d1;
            heap_bh44_w25_4_d1 <=  heap_bh44_w25_4;
            heap_bh44_w36_3_d1 <=  heap_bh44_w36_3;
            heap_bh44_w36_3_d2 <=  heap_bh44_w36_3_d1;
            heap_bh44_w37_2_d1 <=  heap_bh44_w37_2;
            heap_bh44_w37_2_d2 <=  heap_bh44_w37_2_d1;
            heap_bh44_w25_5_d1 <=  heap_bh44_w25_5;
            heap_bh44_w26_5_d1 <=  heap_bh44_w26_5;
            heap_bh44_w38_3_d1 <=  heap_bh44_w38_3;
            heap_bh44_w38_3_d2 <=  heap_bh44_w38_3_d1;
            heap_bh44_w39_2_d1 <=  heap_bh44_w39_2;
            heap_bh44_w39_2_d2 <=  heap_bh44_w39_2_d1;
            heap_bh44_w40_2_d1 <=  heap_bh44_w40_2;
            heap_bh44_w27_5_d1 <=  heap_bh44_w27_5;
            heap_bh44_w28_5_d1 <=  heap_bh44_w28_5;
            heap_bh44_w40_3_d1 <=  heap_bh44_w40_3;
            heap_bh44_w41_2_d1 <=  heap_bh44_w41_2;
            heap_bh44_w29_5_d1 <=  heap_bh44_w29_5;
            heap_bh44_w30_5_d1 <=  heap_bh44_w30_5;
            heap_bh44_w42_3_d1 <=  heap_bh44_w42_3;
            heap_bh44_w31_5_d1 <=  heap_bh44_w31_5;
            heap_bh44_w32_5_d1 <=  heap_bh44_w32_5;
            heap_bh44_w33_3_d1 <=  heap_bh44_w33_3;
         end if;
      end process;
   XX_m46 <= X ;
   YY_m46 <= Y ;
   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=0 lsbY=18 msbX=2 msbY=27
   Xp_m46b48 <= XX_m46(1 downto 0) & "0";
   Yp_m46b48 <= YY_m46(26 downto 18) & "";
   x_m46b48_0 <= Xp_m46b48(2 downto 0);
   y_m46b48_0 <= Yp_m46b48(2 downto 0);
   y_m46b48_1 <= Yp_m46b48(5 downto 3);
   y_m46b48_2 <= Yp_m46b48(8 downto 6);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X0_48_m46 <= y_m46b48_0 & x_m46b48_0;
   PP_m46_48X0Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid50  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X0_48_m46,
                 Y => PP48X0Y0_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_0 <= PP48X0Y0_m46(3); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w1_0 <= PP48X0Y0_m46(4); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w2_0 <= PP48X0Y0_m46(5); -- cycle= 0 cp= 5.3072e-10

   -- Partial product row number 1
   Y1X0_48_m46 <= y_m46b48_1 & x_m46b48_0;
   PP_m46_48X0Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid50  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X0_48_m46,
                 Y => PP48X0Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w1_1 <= PP48X0Y1_m46(1); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w2_1 <= PP48X0Y1_m46(2); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w3_0 <= PP48X0Y1_m46(3); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w4_0 <= PP48X0Y1_m46(4); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w5_0 <= PP48X0Y1_m46(5); -- cycle= 0 cp= 5.3072e-10

   -- Partial product row number 2
   Y2X0_48_m46 <= y_m46b48_2 & x_m46b48_0;
   PP_m46_48X0Y2_Tbl: SmallMultTableP3x3r6XuYs_F400_uid54  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X0_48_m46,
                 Y => PP48X0Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w4_1 <= PP48X0Y2_m46(1); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w5_1 <= PP48X0Y2_m46(2); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w6_0 <= PP48X0Y2_m46(3); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w7_0 <= PP48X0Y2_m46(4); -- cycle= 0 cp= 5.3072e-10
   heap_bh44_w8_0 <= not PP48X0Y2_m46(5); -- cycle= 0 cp= 5.3072e-10

   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=11 lsbY=0 msbX=27 msbY=9
   Xp_m46b63 <= XX_m46(26 downto 11) & "00";
   Yp_m46b63 <= YY_m46(8 downto 0) & "";
   x_m46b63_0 <= Xp_m46b63(2 downto 0);
   x_m46b63_1 <= Xp_m46b63(5 downto 3);
   x_m46b63_2 <= Xp_m46b63(8 downto 6);
   x_m46b63_3 <= Xp_m46b63(11 downto 9);
   x_m46b63_4 <= Xp_m46b63(14 downto 12);
   x_m46b63_5 <= Xp_m46b63(17 downto 15);
   y_m46b63_0 <= Yp_m46b63(2 downto 0);
   y_m46b63_1 <= Yp_m46b63(5 downto 3);
   y_m46b63_2 <= Yp_m46b63(8 downto 6);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X2_63_m46 <= y_m46b63_0 & x_m46b63_2;
   PP_m46_63X2Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X2_63_m46,
                 Y => PP63X2Y0_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_1 <= PP63X2Y0_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y0X3_63_m46 <= y_m46b63_0 & x_m46b63_3;
   PP_m46_63X3Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X3_63_m46,
                 Y => PP63X3Y0_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_2 <= PP63X3Y0_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w1_2 <= PP63X3Y0_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_2 <= PP63X3Y0_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_1 <= PP63X3Y0_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y0X4_63_m46 <= y_m46b63_0 & x_m46b63_4;
   PP_m46_63X4Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X4_63_m46,
                 Y => PP63X4Y0_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w1_3 <= PP63X4Y0_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_3 <= PP63X4Y0_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_2 <= PP63X4Y0_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w4_2 <= PP63X4Y0_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_2 <= PP63X4Y0_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_1 <= PP63X4Y0_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y0X5_63_m46 <= y_m46b63_0 & x_m46b63_5;
   PP_m46_63X5Y0_Tbl: SmallMultTableP3x3r6XsYu_F400_uid67  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X5_63_m46,
                 Y => PP63X5Y0_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w4_3 <= PP63X5Y0_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_3 <= PP63X5Y0_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_2 <= PP63X5Y0_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w7_1 <= PP63X5Y0_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_1 <= PP63X5Y0_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_0 <= not PP63X5Y0_m46(5); -- cycle= 0 cp= 5.7432e-10

   -- Partial product row number 1
   Y1X1_63_m46 <= y_m46b63_1 & x_m46b63_1;
   PP_m46_63X1Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X1_63_m46,
                 Y => PP63X1Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_3 <= PP63X1Y1_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y1X2_63_m46 <= y_m46b63_1 & x_m46b63_2;
   PP_m46_63X2Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X2_63_m46,
                 Y => PP63X2Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_4 <= PP63X2Y1_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w1_4 <= PP63X2Y1_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_4 <= PP63X2Y1_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_3 <= PP63X2Y1_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y1X3_63_m46 <= y_m46b63_1 & x_m46b63_3;
   PP_m46_63X3Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X3_63_m46,
                 Y => PP63X3Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w1_5 <= PP63X3Y1_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_5 <= PP63X3Y1_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_4 <= PP63X3Y1_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w4_4 <= PP63X3Y1_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_4 <= PP63X3Y1_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_3 <= PP63X3Y1_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y1X4_63_m46 <= y_m46b63_1 & x_m46b63_4;
   PP_m46_63X4Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X4_63_m46,
                 Y => PP63X4Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w4_5 <= PP63X4Y1_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_5 <= PP63X4Y1_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_4 <= PP63X4Y1_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w7_2 <= PP63X4Y1_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_2 <= PP63X4Y1_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_1 <= PP63X4Y1_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y1X5_63_m46 <= y_m46b63_1 & x_m46b63_5;
   PP_m46_63X5Y1_Tbl: SmallMultTableP3x3r6XsYu_F400_uid67  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X5_63_m46,
                 Y => PP63X5Y1_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w7_3 <= PP63X5Y1_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_3 <= PP63X5Y1_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_2 <= PP63X5Y1_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w10_0 <= PP63X5Y1_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w11_0 <= PP63X5Y1_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w12_0 <= not PP63X5Y1_m46(5); -- cycle= 0 cp= 5.7432e-10

   -- Partial product row number 2
   Y2X0_63_m46 <= y_m46b63_2 & x_m46b63_0;
   PP_m46_63X0Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X0_63_m46,
                 Y => PP63X0Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_5 <= PP63X0Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y2X1_63_m46 <= y_m46b63_2 & x_m46b63_1;
   PP_m46_63X1Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X1_63_m46,
                 Y => PP63X1Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w0_6 <= PP63X1Y2_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w1_6 <= PP63X1Y2_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_6 <= PP63X1Y2_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_5 <= PP63X1Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y2X2_63_m46 <= y_m46b63_2 & x_m46b63_2;
   PP_m46_63X2Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X2_63_m46,
                 Y => PP63X2Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w1_7 <= PP63X2Y2_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w2_7 <= PP63X2Y2_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w3_6 <= PP63X2Y2_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w4_6 <= PP63X2Y2_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_6 <= PP63X2Y2_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_5 <= PP63X2Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y2X3_63_m46 <= y_m46b63_2 & x_m46b63_3;
   PP_m46_63X3Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X3_63_m46,
                 Y => PP63X3Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w4_7 <= PP63X3Y2_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w5_7 <= PP63X3Y2_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w6_6 <= PP63X3Y2_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w7_4 <= PP63X3Y2_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_4 <= PP63X3Y2_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_3 <= PP63X3Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y2X4_63_m46 <= y_m46b63_2 & x_m46b63_4;
   PP_m46_63X4Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid65  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X4_63_m46,
                 Y => PP63X4Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w7_5 <= PP63X4Y2_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_5 <= PP63X4Y2_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_4 <= PP63X4Y2_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w10_1 <= PP63X4Y2_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w11_1 <= PP63X4Y2_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w12_1 <= PP63X4Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   Y2X5_63_m46 <= y_m46b63_2 & x_m46b63_5;
   PP_m46_63X5Y2_Tbl: SmallMultTableP3x3r6XsYu_F400_uid67  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X5_63_m46,
                 Y => PP63X5Y2_m46);
   -- Adding the relevant bits to the heap of bits
   heap_bh44_w10_2 <= PP63X5Y2_m46(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w11_2 <= PP63X5Y2_m46(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w12_2 <= PP63X5Y2_m46(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w13_0 <= PP63X5Y2_m46(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w14_0 <= PP63X5Y2_m46(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w15_0 <= not PP63X5Y2_m46(5); -- cycle= 0 cp= 5.7432e-10

   heap_bh44_w6_7 <= A(0); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w7_6 <= A(1); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w8_6 <= A(2); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w9_5 <= A(3); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w10_3 <= A(4); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w11_3 <= A(5); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w12_3 <= A(6); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w13_1 <= A(7); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w14_1 <= A(8); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w15_1 <= A(9); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w16_0 <= A(10); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w17_0 <= A(11); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w18_0 <= A(12); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w19_0 <= A(13); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w20_0 <= A(14); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w21_0 <= A(15); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w22_0 <= A(16); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w23_0 <= A(17); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w24_0 <= A(18); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w25_0 <= A(19); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w26_0 <= A(20); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w27_0 <= A(21); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w28_0 <= A(22); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w29_0 <= A(23); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w30_0 <= A(24); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w31_0 <= A(25); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w32_0 <= A(26); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w33_0 <= A(27); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w34_0 <= A(28); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w35_0 <= A(29); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w36_0 <= A(30); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w37_0 <= A(31); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w38_0 <= A(32); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w39_0 <= A(33); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w40_0 <= A(34); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w41_0 <= A(35); -- cycle= 0 cp= 5.7432e-10
   heap_bh44_w42_0 <= A(36); -- cycle= 0 cp= 5.7432e-10
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------
   DSP_bh44_ch0_0 <= std_logic_vector(signed("" & XX_m46(26 downto 2) & "") * signed("" & YY_m46(26 downto 9) & ""));
   heap_bh44_w33_1 <= not( DSP_bh44_ch0_0(42) ); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w32_1 <= DSP_bh44_ch0_0(41); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w31_1 <= DSP_bh44_ch0_0(40); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w30_1 <= DSP_bh44_ch0_0(39); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w29_1 <= DSP_bh44_ch0_0(38); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w28_1 <= DSP_bh44_ch0_0(37); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w27_1 <= DSP_bh44_ch0_0(36); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w26_1 <= DSP_bh44_ch0_0(35); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w25_1 <= DSP_bh44_ch0_0(34); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w24_1 <= DSP_bh44_ch0_0(33); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w23_1 <= DSP_bh44_ch0_0(32); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w22_1 <= DSP_bh44_ch0_0(31); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w21_1 <= DSP_bh44_ch0_0(30); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w20_1 <= DSP_bh44_ch0_0(29); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w19_1 <= DSP_bh44_ch0_0(28); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w18_1 <= DSP_bh44_ch0_0(27); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w17_1 <= DSP_bh44_ch0_0(26); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w16_1 <= DSP_bh44_ch0_0(25); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w15_2 <= DSP_bh44_ch0_0(24); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w14_2 <= DSP_bh44_ch0_0(23); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w13_2 <= DSP_bh44_ch0_0(22); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w12_4 <= DSP_bh44_ch0_0(21); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w11_4 <= DSP_bh44_ch0_0(20); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w10_4 <= DSP_bh44_ch0_0(19); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w9_6 <= DSP_bh44_ch0_0(18); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w8_7 <= DSP_bh44_ch0_0(17); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w7_7 <= DSP_bh44_ch0_0(16); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w6_8 <= DSP_bh44_ch0_0(15); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w5_8 <= DSP_bh44_ch0_0(14); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w4_8 <= DSP_bh44_ch0_0(13); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w3_7 <= DSP_bh44_ch0_0(12); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w2_8 <= DSP_bh44_ch0_0(11); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w1_8 <= DSP_bh44_ch0_0(10); -- cycle= 0 cp= 2.387e-09
   heap_bh44_w0_7 <= DSP_bh44_ch0_0(9); -- cycle= 0 cp= 2.387e-09
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh44_w5_9 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w8_8 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w10_5 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w11_5 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w13_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w14_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w16_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w17_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w18_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w19_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w20_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w21_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w22_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w23_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w24_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w25_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w26_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w27_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w28_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w29_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w30_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w31_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w32_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w34_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w35_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w36_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w37_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w38_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w39_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w40_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w41_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh44_w42_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_0_0 <= heap_bh44_w0_0 & heap_bh44_w0_6 & heap_bh44_w0_5 & heap_bh44_w0_4 & heap_bh44_w0_3 & heap_bh44_w0_2;
      Compressor_bh44_0: Compressor_6_3
      port map ( R => CompressorOut_bh44_0_0,
                 X0 => CompressorIn_bh44_0_0);
   heap_bh44_w0_8 <= CompressorOut_bh44_0_0(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w1_9 <= CompressorOut_bh44_0_0(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w2_9 <= CompressorOut_bh44_0_0(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_1_1 <= heap_bh44_w1_1 & heap_bh44_w1_0 & heap_bh44_w1_7 & heap_bh44_w1_6 & heap_bh44_w1_5 & heap_bh44_w1_4;
      Compressor_bh44_1: Compressor_6_3
      port map ( R => CompressorOut_bh44_1_1,
                 X0 => CompressorIn_bh44_1_1);
   heap_bh44_w1_10 <= CompressorOut_bh44_1_1(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w2_10 <= CompressorOut_bh44_1_1(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w3_8 <= CompressorOut_bh44_1_1(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_2_2 <= heap_bh44_w2_1 & heap_bh44_w2_0 & heap_bh44_w2_7 & heap_bh44_w2_6 & heap_bh44_w2_5 & heap_bh44_w2_4;
      Compressor_bh44_2: Compressor_6_3
      port map ( R => CompressorOut_bh44_2_2,
                 X0 => CompressorIn_bh44_2_2);
   heap_bh44_w2_11 <= CompressorOut_bh44_2_2(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w3_9 <= CompressorOut_bh44_2_2(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w4_9 <= CompressorOut_bh44_2_2(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_3_3 <= heap_bh44_w3_0 & heap_bh44_w3_6 & heap_bh44_w3_5 & heap_bh44_w3_4 & heap_bh44_w3_3 & heap_bh44_w3_2;
      Compressor_bh44_3: Compressor_6_3
      port map ( R => CompressorOut_bh44_3_3,
                 X0 => CompressorIn_bh44_3_3);
   heap_bh44_w3_10 <= CompressorOut_bh44_3_3(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w4_10 <= CompressorOut_bh44_3_3(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w5_10 <= CompressorOut_bh44_3_3(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_4_4 <= heap_bh44_w4_1 & heap_bh44_w4_0 & heap_bh44_w4_7 & heap_bh44_w4_6 & heap_bh44_w4_5 & heap_bh44_w4_4;
      Compressor_bh44_4: Compressor_6_3
      port map ( R => CompressorOut_bh44_4_4,
                 X0 => CompressorIn_bh44_4_4);
   heap_bh44_w4_11 <= CompressorOut_bh44_4_4(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w5_11 <= CompressorOut_bh44_4_4(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w6_9 <= CompressorOut_bh44_4_4(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_5_5 <= heap_bh44_w5_9 & heap_bh44_w5_1 & heap_bh44_w5_0 & heap_bh44_w5_7 & heap_bh44_w5_6 & heap_bh44_w5_5;
      Compressor_bh44_5: Compressor_6_3
      port map ( R => CompressorOut_bh44_5_5,
                 X0 => CompressorIn_bh44_5_5);
   heap_bh44_w5_12 <= CompressorOut_bh44_5_5(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w6_10 <= CompressorOut_bh44_5_5(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w7_8 <= CompressorOut_bh44_5_5(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_6_6 <= heap_bh44_w6_0 & heap_bh44_w6_7 & heap_bh44_w6_6 & heap_bh44_w6_5 & heap_bh44_w6_4 & heap_bh44_w6_3;
      Compressor_bh44_6: Compressor_6_3
      port map ( R => CompressorOut_bh44_6_6,
                 X0 => CompressorIn_bh44_6_6);
   heap_bh44_w6_11 <= CompressorOut_bh44_6_6(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w7_9 <= CompressorOut_bh44_6_6(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w8_9 <= CompressorOut_bh44_6_6(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_7_7 <= heap_bh44_w7_0 & heap_bh44_w7_6 & heap_bh44_w7_5 & heap_bh44_w7_4 & heap_bh44_w7_3 & heap_bh44_w7_2;
      Compressor_bh44_7: Compressor_6_3
      port map ( R => CompressorOut_bh44_7_7,
                 X0 => CompressorIn_bh44_7_7);
   heap_bh44_w7_10 <= CompressorOut_bh44_7_7(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w8_10 <= CompressorOut_bh44_7_7(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w9_7 <= CompressorOut_bh44_7_7(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_8_8 <= heap_bh44_w8_8 & heap_bh44_w8_0 & heap_bh44_w8_6 & heap_bh44_w8_5 & heap_bh44_w8_4 & heap_bh44_w8_3;
      Compressor_bh44_8: Compressor_6_3
      port map ( R => CompressorOut_bh44_8_8,
                 X0 => CompressorIn_bh44_8_8);
   heap_bh44_w8_11 <= CompressorOut_bh44_8_8(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w9_8 <= CompressorOut_bh44_8_8(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w10_6 <= CompressorOut_bh44_8_8(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_9_9 <= heap_bh44_w9_5 & heap_bh44_w9_4 & heap_bh44_w9_3 & heap_bh44_w9_2 & heap_bh44_w9_1 & heap_bh44_w9_0;
      Compressor_bh44_9: Compressor_6_3
      port map ( R => CompressorOut_bh44_9_9,
                 X0 => CompressorIn_bh44_9_9);
   heap_bh44_w9_9 <= CompressorOut_bh44_9_9(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w10_7 <= CompressorOut_bh44_9_9(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w11_6 <= CompressorOut_bh44_9_9(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_10_10 <= heap_bh44_w10_5 & heap_bh44_w10_3 & heap_bh44_w10_2 & heap_bh44_w10_1;
   CompressorIn_bh44_10_11(0) <= heap_bh44_w11_5;
      Compressor_bh44_10: Compressor_14_3
      port map ( R => CompressorOut_bh44_10_10,
                 X0 => CompressorIn_bh44_10_10,
                 X1 => CompressorIn_bh44_10_11);
   heap_bh44_w10_8 <= CompressorOut_bh44_10_10(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w11_7 <= CompressorOut_bh44_10_10(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w12_5 <= CompressorOut_bh44_10_10(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_11_12 <= heap_bh44_w11_3 & heap_bh44_w11_2 & heap_bh44_w11_1 & heap_bh44_w11_0;
   CompressorIn_bh44_11_13(0) <= heap_bh44_w12_3;
      Compressor_bh44_11: Compressor_14_3
      port map ( R => CompressorOut_bh44_11_11,
                 X0 => CompressorIn_bh44_11_12,
                 X1 => CompressorIn_bh44_11_13);
   heap_bh44_w11_8 <= CompressorOut_bh44_11_11(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w12_6 <= CompressorOut_bh44_11_11(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w13_4 <= CompressorOut_bh44_11_11(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_12_14 <= heap_bh44_w5_4 & heap_bh44_w5_3 & heap_bh44_w5_2;
   CompressorIn_bh44_12_15 <= heap_bh44_w6_2 & heap_bh44_w6_1;
      Compressor_bh44_12: Compressor_23_3
      port map ( R => CompressorOut_bh44_12_12,
                 X0 => CompressorIn_bh44_12_14,
                 X1 => CompressorIn_bh44_12_15);
   heap_bh44_w5_13 <= CompressorOut_bh44_12_12(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w6_12 <= CompressorOut_bh44_12_12(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w7_11 <= CompressorOut_bh44_12_12(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_13_16 <= heap_bh44_w12_2 & heap_bh44_w12_1 & heap_bh44_w12_0;
   CompressorIn_bh44_13_17 <= heap_bh44_w13_3 & heap_bh44_w13_1;
      Compressor_bh44_13: Compressor_23_3
      port map ( R => CompressorOut_bh44_13_13,
                 X0 => CompressorIn_bh44_13_16,
                 X1 => CompressorIn_bh44_13_17);
   heap_bh44_w12_7 <= CompressorOut_bh44_13_13(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w13_5 <= CompressorOut_bh44_13_13(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w14_4 <= CompressorOut_bh44_13_13(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_14_18 <= heap_bh44_w14_3 & heap_bh44_w14_1 & heap_bh44_w14_0;
   CompressorIn_bh44_14_19 <= heap_bh44_w15_1 & heap_bh44_w15_0;
      Compressor_bh44_14: Compressor_23_3
      port map ( R => CompressorOut_bh44_14_14,
                 X0 => CompressorIn_bh44_14_18,
                 X1 => CompressorIn_bh44_14_19);
   heap_bh44_w14_5 <= CompressorOut_bh44_14_14(0); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w15_3 <= CompressorOut_bh44_14_14(1); -- cycle= 0 cp= 1.10504e-09
   heap_bh44_w16_3 <= CompressorOut_bh44_14_14(2); -- cycle= 0 cp= 1.10504e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_15_20 <= heap_bh44_w1_3 & heap_bh44_w1_2 & heap_bh44_w1_10 & heap_bh44_w1_9;
   CompressorIn_bh44_15_21(0) <= heap_bh44_w2_3;
      Compressor_bh44_15: Compressor_14_3
      port map ( R => CompressorOut_bh44_15_15,
                 X0 => CompressorIn_bh44_15_20,
                 X1 => CompressorIn_bh44_15_21);
   heap_bh44_w1_11 <= CompressorOut_bh44_15_15(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w2_12 <= CompressorOut_bh44_15_15(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w3_11 <= CompressorOut_bh44_15_15(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_16_22 <= heap_bh44_w2_2 & heap_bh44_w2_11 & heap_bh44_w2_10 & heap_bh44_w2_9;
   CompressorIn_bh44_16_23(0) <= heap_bh44_w3_1;
      Compressor_bh44_16: Compressor_14_3
      port map ( R => CompressorOut_bh44_16_16,
                 X0 => CompressorIn_bh44_16_22,
                 X1 => CompressorIn_bh44_16_23);
   heap_bh44_w2_13 <= CompressorOut_bh44_16_16(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w3_12 <= CompressorOut_bh44_16_16(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w4_12 <= CompressorOut_bh44_16_16(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_17_24 <= heap_bh44_w4_3 & heap_bh44_w4_2 & heap_bh44_w4_11 & heap_bh44_w4_10;
   CompressorIn_bh44_17_25(0) <= heap_bh44_w5_13;
      Compressor_bh44_17: Compressor_14_3
      port map ( R => CompressorOut_bh44_17_17,
                 X0 => CompressorIn_bh44_17_24,
                 X1 => CompressorIn_bh44_17_25);
   heap_bh44_w4_13 <= CompressorOut_bh44_17_17(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w5_14 <= CompressorOut_bh44_17_17(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w6_13 <= CompressorOut_bh44_17_17(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_18_26 <= heap_bh44_w6_12 & heap_bh44_w6_11 & heap_bh44_w6_10 & heap_bh44_w6_9;
   CompressorIn_bh44_18_27(0) <= heap_bh44_w7_1;
      Compressor_bh44_18: Compressor_14_3
      port map ( R => CompressorOut_bh44_18_18,
                 X0 => CompressorIn_bh44_18_26,
                 X1 => CompressorIn_bh44_18_27);
   heap_bh44_w6_14 <= CompressorOut_bh44_18_18(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w7_12 <= CompressorOut_bh44_18_18(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w8_12 <= CompressorOut_bh44_18_18(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_19_28 <= heap_bh44_w7_11 & heap_bh44_w7_10 & heap_bh44_w7_9 & heap_bh44_w7_8;
   CompressorIn_bh44_19_29(0) <= heap_bh44_w8_2;
      Compressor_bh44_19: Compressor_14_3
      port map ( R => CompressorOut_bh44_19_19,
                 X0 => CompressorIn_bh44_19_28,
                 X1 => CompressorIn_bh44_19_29);
   heap_bh44_w7_13 <= CompressorOut_bh44_19_19(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w8_13 <= CompressorOut_bh44_19_19(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w9_10 <= CompressorOut_bh44_19_19(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_20_30 <= heap_bh44_w8_1 & heap_bh44_w8_11 & heap_bh44_w8_10 & heap_bh44_w8_9;
   CompressorIn_bh44_20_31(0) <= heap_bh44_w9_9;
      Compressor_bh44_20: Compressor_14_3
      port map ( R => CompressorOut_bh44_20_20,
                 X0 => CompressorIn_bh44_20_30,
                 X1 => CompressorIn_bh44_20_31);
   heap_bh44_w8_14 <= CompressorOut_bh44_20_20(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w9_11 <= CompressorOut_bh44_20_20(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w10_9 <= CompressorOut_bh44_20_20(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_21_32 <= heap_bh44_w10_0 & heap_bh44_w10_8 & heap_bh44_w10_7 & heap_bh44_w10_6;
   CompressorIn_bh44_21_33(0) <= heap_bh44_w11_8;
      Compressor_bh44_21: Compressor_14_3
      port map ( R => CompressorOut_bh44_21_21,
                 X0 => CompressorIn_bh44_21_32,
                 X1 => CompressorIn_bh44_21_33);
   heap_bh44_w10_10 <= CompressorOut_bh44_21_21(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w11_9 <= CompressorOut_bh44_21_21(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w12_8 <= CompressorOut_bh44_21_21(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_22_34 <= heap_bh44_w12_7 & heap_bh44_w12_6 & heap_bh44_w12_5;
   CompressorIn_bh44_22_35 <= heap_bh44_w13_0 & heap_bh44_w13_5;
      Compressor_bh44_22: Compressor_23_3
      port map ( R => CompressorOut_bh44_22_22,
                 X0 => CompressorIn_bh44_22_34,
                 X1 => CompressorIn_bh44_22_35);
   heap_bh44_w12_9 <= CompressorOut_bh44_22_22(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w13_6 <= CompressorOut_bh44_22_22(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w14_6 <= CompressorOut_bh44_22_22(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_23_36 <= heap_bh44_w16_2 & heap_bh44_w16_0 & heap_bh44_w16_3;
   CompressorIn_bh44_23_37 <= heap_bh44_w17_2 & heap_bh44_w17_0;
      Compressor_bh44_23: Compressor_23_3
      port map ( R => CompressorOut_bh44_23_23,
                 X0 => CompressorIn_bh44_23_36,
                 X1 => CompressorIn_bh44_23_37);
   heap_bh44_w16_4 <= CompressorOut_bh44_23_23(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w17_3 <= CompressorOut_bh44_23_23(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w18_3 <= CompressorOut_bh44_23_23(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_24_38 <= heap_bh44_w3_10 & heap_bh44_w3_9 & heap_bh44_w3_8;
   CompressorIn_bh44_24_39(0) <= heap_bh44_w4_9;
      Compressor_bh44_24: Compressor_13_3
      port map ( R => CompressorOut_bh44_24_24,
                 X0 => CompressorIn_bh44_24_38,
                 X1 => CompressorIn_bh44_24_39);
   heap_bh44_w3_13 <= CompressorOut_bh44_24_24(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w4_14 <= CompressorOut_bh44_24_24(1); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w5_15 <= CompressorOut_bh44_24_24(2); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh44_25_40 <= heap_bh44_w5_12 & heap_bh44_w5_11 & heap_bh44_w5_10;
      Compressor_bh44_25: Compressor_3_2
      port map ( R => CompressorOut_bh44_25_25,
                 X0 => CompressorIn_bh44_25_40);
   heap_bh44_w5_16 <= CompressorOut_bh44_25_25(0); -- cycle= 0 cp= 1.63576e-09
   heap_bh44_w6_15 <= CompressorOut_bh44_25_25(1); -- cycle= 0 cp= 1.63576e-09

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_26_41 <= heap_bh44_w9_8_d1 & heap_bh44_w9_7_d1 & heap_bh44_w9_11_d1 & heap_bh44_w9_10_d1;
   CompressorIn_bh44_26_42(0) <= heap_bh44_w10_10_d1;
      Compressor_bh44_26: Compressor_14_3
      port map ( R => CompressorOut_bh44_26_26,
                 X0 => CompressorIn_bh44_26_41,
                 X1 => CompressorIn_bh44_26_42);
   heap_bh44_w9_12 <= CompressorOut_bh44_26_26(0); -- cycle= 1 cp= 0
   heap_bh44_w10_11 <= CompressorOut_bh44_26_26(1); -- cycle= 1 cp= 0
   heap_bh44_w11_10 <= CompressorOut_bh44_26_26(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_27_43 <= heap_bh44_w3_13_d1 & heap_bh44_w3_12_d1 & heap_bh44_w3_11_d1;
   CompressorIn_bh44_27_44 <= heap_bh44_w4_14_d1 & heap_bh44_w4_13_d1;
      Compressor_bh44_27: Compressor_23_3
      port map ( R => CompressorOut_bh44_27_27,
                 X0 => CompressorIn_bh44_27_43,
                 X1 => CompressorIn_bh44_27_44);
   heap_bh44_w3_14 <= CompressorOut_bh44_27_27(0); -- cycle= 1 cp= 0
   heap_bh44_w4_15 <= CompressorOut_bh44_27_27(1); -- cycle= 1 cp= 0
   heap_bh44_w5_17 <= CompressorOut_bh44_27_27(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_28_45 <= heap_bh44_w5_16_d1 & heap_bh44_w5_15_d1 & heap_bh44_w5_14_d1;
   CompressorIn_bh44_28_46 <= heap_bh44_w6_15_d1 & heap_bh44_w6_14_d1;
      Compressor_bh44_28: Compressor_23_3
      port map ( R => CompressorOut_bh44_28_28,
                 X0 => CompressorIn_bh44_28_45,
                 X1 => CompressorIn_bh44_28_46);
   heap_bh44_w5_18 <= CompressorOut_bh44_28_28(0); -- cycle= 1 cp= 0
   heap_bh44_w6_16 <= CompressorOut_bh44_28_28(1); -- cycle= 1 cp= 0
   heap_bh44_w7_14 <= CompressorOut_bh44_28_28(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_29_47 <= heap_bh44_w11_7_d1 & heap_bh44_w11_6_d1 & heap_bh44_w11_9_d1;
   CompressorIn_bh44_29_48 <= heap_bh44_w12_9_d1 & heap_bh44_w12_8_d1;
      Compressor_bh44_29: Compressor_23_3
      port map ( R => CompressorOut_bh44_29_29,
                 X0 => CompressorIn_bh44_29_47,
                 X1 => CompressorIn_bh44_29_48);
   heap_bh44_w11_11 <= CompressorOut_bh44_29_29(0); -- cycle= 1 cp= 0
   heap_bh44_w12_10 <= CompressorOut_bh44_29_29(1); -- cycle= 1 cp= 0
   heap_bh44_w13_7 <= CompressorOut_bh44_29_29(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_30_49 <= heap_bh44_w18_2_d1 & heap_bh44_w18_0_d1 & heap_bh44_w18_3_d1;
   CompressorIn_bh44_30_50 <= heap_bh44_w19_2_d1 & heap_bh44_w19_0_d1;
      Compressor_bh44_30: Compressor_23_3
      port map ( R => CompressorOut_bh44_30_30,
                 X0 => CompressorIn_bh44_30_49,
                 X1 => CompressorIn_bh44_30_50);
   heap_bh44_w18_4 <= CompressorOut_bh44_30_30(0); -- cycle= 1 cp= 0
   heap_bh44_w19_3 <= CompressorOut_bh44_30_30(1); -- cycle= 1 cp= 0
   heap_bh44_w20_3 <= CompressorOut_bh44_30_30(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_31_51 <= heap_bh44_w14_5_d1 & heap_bh44_w14_4_d1 & heap_bh44_w14_6_d1;
   CompressorIn_bh44_31_52(0) <= heap_bh44_w15_3_d1;
      Compressor_bh44_31: Compressor_13_3
      port map ( R => CompressorOut_bh44_31_31,
                 X0 => CompressorIn_bh44_31_51,
                 X1 => CompressorIn_bh44_31_52);
   heap_bh44_w14_7 <= CompressorOut_bh44_31_31(0); -- cycle= 1 cp= 0
   heap_bh44_w15_4 <= CompressorOut_bh44_31_31(1); -- cycle= 1 cp= 0
   heap_bh44_w16_5 <= CompressorOut_bh44_31_31(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_32_53 <= heap_bh44_w8_14_d1 & heap_bh44_w8_13_d1 & heap_bh44_w8_12_d1;
      Compressor_bh44_32: Compressor_3_2
      port map ( R => CompressorOut_bh44_32_32,
                 X0 => CompressorIn_bh44_32_53);
   heap_bh44_w8_15 <= CompressorOut_bh44_32_32(0); -- cycle= 1 cp= 0
   heap_bh44_w9_13 <= CompressorOut_bh44_32_32(1); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_33_54 <= heap_bh44_w7_13_d1 & heap_bh44_w7_12_d1 & heap_bh44_w7_7_d1 & heap_bh44_w7_14;
   CompressorIn_bh44_33_55(0) <= heap_bh44_w8_7_d1;
      Compressor_bh44_33: Compressor_14_3
      port map ( R => CompressorOut_bh44_33_33,
                 X0 => CompressorIn_bh44_33_54,
                 X1 => CompressorIn_bh44_33_55);
   heap_bh44_w7_15 <= CompressorOut_bh44_33_33(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w8_16 <= CompressorOut_bh44_33_33(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w9_14 <= CompressorOut_bh44_33_33(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_34_56 <= heap_bh44_w13_4_d1 & heap_bh44_w13_6_d1 & heap_bh44_w13_2_d1 & heap_bh44_w13_7;
   CompressorIn_bh44_34_57(0) <= heap_bh44_w14_2_d1;
      Compressor_bh44_34: Compressor_14_3
      port map ( R => CompressorOut_bh44_34_34,
                 X0 => CompressorIn_bh44_34_56,
                 X1 => CompressorIn_bh44_34_57);
   heap_bh44_w13_8 <= CompressorOut_bh44_34_34(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w14_8 <= CompressorOut_bh44_34_34(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w15_5 <= CompressorOut_bh44_34_34(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_35_58 <= heap_bh44_w20_2_d1 & heap_bh44_w20_0_d1 & heap_bh44_w20_1_d1 & heap_bh44_w20_3;
   CompressorIn_bh44_35_59(0) <= heap_bh44_w21_2_d1;
      Compressor_bh44_35: Compressor_14_3
      port map ( R => CompressorOut_bh44_35_35,
                 X0 => CompressorIn_bh44_35_58,
                 X1 => CompressorIn_bh44_35_59);
   heap_bh44_w20_4 <= CompressorOut_bh44_35_35(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w21_3 <= CompressorOut_bh44_35_35(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w22_3 <= CompressorOut_bh44_35_35(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_36_60 <= heap_bh44_w0_1_d1 & heap_bh44_w0_8_d1 & heap_bh44_w0_7_d1;
   CompressorIn_bh44_36_61 <= heap_bh44_w1_11_d1 & heap_bh44_w1_8_d1;
      Compressor_bh44_36: Compressor_23_3
      port map ( R => CompressorOut_bh44_36_36,
                 X0 => CompressorIn_bh44_36_60,
                 X1 => CompressorIn_bh44_36_61);
   heap_bh44_w0_9 <= CompressorOut_bh44_36_36(0); -- cycle= 1 cp= 0
   heap_bh44_w1_12 <= CompressorOut_bh44_36_36(1); -- cycle= 1 cp= 0
   heap_bh44_w2_14 <= CompressorOut_bh44_36_36(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_37_62 <= heap_bh44_w2_13_d1 & heap_bh44_w2_12_d1 & heap_bh44_w2_8_d1;
   CompressorIn_bh44_37_63 <= heap_bh44_w3_7_d1 & heap_bh44_w3_14;
      Compressor_bh44_37: Compressor_23_3
      port map ( R => CompressorOut_bh44_37_37,
                 X0 => CompressorIn_bh44_37_62,
                 X1 => CompressorIn_bh44_37_63);
   heap_bh44_w2_15 <= CompressorOut_bh44_37_37(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w3_15 <= CompressorOut_bh44_37_37(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w4_16 <= CompressorOut_bh44_37_37(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_38_64 <= heap_bh44_w4_12_d1 & heap_bh44_w4_8_d1 & heap_bh44_w4_15;
   CompressorIn_bh44_38_65 <= heap_bh44_w5_8_d1 & heap_bh44_w5_18;
      Compressor_bh44_38: Compressor_23_3
      port map ( R => CompressorOut_bh44_38_38,
                 X0 => CompressorIn_bh44_38_64,
                 X1 => CompressorIn_bh44_38_65);
   heap_bh44_w4_17 <= CompressorOut_bh44_38_38(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w5_19 <= CompressorOut_bh44_38_38(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w6_17 <= CompressorOut_bh44_38_38(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_39_66 <= heap_bh44_w9_6_d1 & heap_bh44_w9_13 & heap_bh44_w9_12;
   CompressorIn_bh44_39_67 <= heap_bh44_w10_9_d1 & heap_bh44_w10_4_d1;
      Compressor_bh44_39: Compressor_23_3
      port map ( R => CompressorOut_bh44_39_39,
                 X0 => CompressorIn_bh44_39_66,
                 X1 => CompressorIn_bh44_39_67);
   heap_bh44_w9_15 <= CompressorOut_bh44_39_39(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w10_12 <= CompressorOut_bh44_39_39(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w11_12 <= CompressorOut_bh44_39_39(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_40_68 <= heap_bh44_w11_4_d1 & heap_bh44_w11_11 & heap_bh44_w11_10;
   CompressorIn_bh44_40_69 <= heap_bh44_w12_4_d1 & heap_bh44_w12_10;
      Compressor_bh44_40: Compressor_23_3
      port map ( R => CompressorOut_bh44_40_40,
                 X0 => CompressorIn_bh44_40_68,
                 X1 => CompressorIn_bh44_40_69);
   heap_bh44_w11_13 <= CompressorOut_bh44_40_40(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w12_11 <= CompressorOut_bh44_40_40(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w13_9 <= CompressorOut_bh44_40_40(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_41_70 <= heap_bh44_w16_4_d1 & heap_bh44_w16_1_d1 & heap_bh44_w16_5;
   CompressorIn_bh44_41_71 <= heap_bh44_w17_3_d1 & heap_bh44_w17_1_d1;
      Compressor_bh44_41: Compressor_23_3
      port map ( R => CompressorOut_bh44_41_41,
                 X0 => CompressorIn_bh44_41_70,
                 X1 => CompressorIn_bh44_41_71);
   heap_bh44_w16_6 <= CompressorOut_bh44_41_41(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w17_4 <= CompressorOut_bh44_41_41(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w18_5 <= CompressorOut_bh44_41_41(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_42_72 <= heap_bh44_w22_2_d1 & heap_bh44_w22_0_d1 & heap_bh44_w22_1_d1;
   CompressorIn_bh44_42_73 <= heap_bh44_w23_2_d1 & heap_bh44_w23_0_d1;
      Compressor_bh44_42: Compressor_23_3
      port map ( R => CompressorOut_bh44_42_42,
                 X0 => CompressorIn_bh44_42_72,
                 X1 => CompressorIn_bh44_42_73);
   heap_bh44_w22_4 <= CompressorOut_bh44_42_42(0); -- cycle= 1 cp= 0
   heap_bh44_w23_3 <= CompressorOut_bh44_42_42(1); -- cycle= 1 cp= 0
   heap_bh44_w24_3 <= CompressorOut_bh44_42_42(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_43_74 <= heap_bh44_w24_2_d1 & heap_bh44_w24_0_d1 & heap_bh44_w24_1_d1;
   CompressorIn_bh44_43_75 <= heap_bh44_w25_2_d1 & heap_bh44_w25_0_d1;
      Compressor_bh44_43: Compressor_23_3
      port map ( R => CompressorOut_bh44_43_43,
                 X0 => CompressorIn_bh44_43_74,
                 X1 => CompressorIn_bh44_43_75);
   heap_bh44_w24_4 <= CompressorOut_bh44_43_43(0); -- cycle= 1 cp= 0
   heap_bh44_w25_3 <= CompressorOut_bh44_43_43(1); -- cycle= 1 cp= 0
   heap_bh44_w26_3 <= CompressorOut_bh44_43_43(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_44_76 <= heap_bh44_w26_2_d1 & heap_bh44_w26_0_d1 & heap_bh44_w26_1_d1;
   CompressorIn_bh44_44_77 <= heap_bh44_w27_2_d1 & heap_bh44_w27_0_d1;
      Compressor_bh44_44: Compressor_23_3
      port map ( R => CompressorOut_bh44_44_44,
                 X0 => CompressorIn_bh44_44_76,
                 X1 => CompressorIn_bh44_44_77);
   heap_bh44_w26_4 <= CompressorOut_bh44_44_44(0); -- cycle= 1 cp= 0
   heap_bh44_w27_3 <= CompressorOut_bh44_44_44(1); -- cycle= 1 cp= 0
   heap_bh44_w28_3 <= CompressorOut_bh44_44_44(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_45_78 <= heap_bh44_w28_2_d1 & heap_bh44_w28_0_d1 & heap_bh44_w28_1_d1;
   CompressorIn_bh44_45_79 <= heap_bh44_w29_2_d1 & heap_bh44_w29_0_d1;
      Compressor_bh44_45: Compressor_23_3
      port map ( R => CompressorOut_bh44_45_45,
                 X0 => CompressorIn_bh44_45_78,
                 X1 => CompressorIn_bh44_45_79);
   heap_bh44_w28_4 <= CompressorOut_bh44_45_45(0); -- cycle= 1 cp= 0
   heap_bh44_w29_3 <= CompressorOut_bh44_45_45(1); -- cycle= 1 cp= 0
   heap_bh44_w30_3 <= CompressorOut_bh44_45_45(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_46_80 <= heap_bh44_w30_2_d1 & heap_bh44_w30_0_d1 & heap_bh44_w30_1_d1;
   CompressorIn_bh44_46_81 <= heap_bh44_w31_2_d1 & heap_bh44_w31_0_d1;
      Compressor_bh44_46: Compressor_23_3
      port map ( R => CompressorOut_bh44_46_46,
                 X0 => CompressorIn_bh44_46_80,
                 X1 => CompressorIn_bh44_46_81);
   heap_bh44_w30_4 <= CompressorOut_bh44_46_46(0); -- cycle= 1 cp= 0
   heap_bh44_w31_3 <= CompressorOut_bh44_46_46(1); -- cycle= 1 cp= 0
   heap_bh44_w32_3 <= CompressorOut_bh44_46_46(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_47_82 <= heap_bh44_w32_2_d1 & heap_bh44_w32_0_d1 & heap_bh44_w32_1_d1;
   CompressorIn_bh44_47_83 <= heap_bh44_w33_0_d1 & heap_bh44_w33_1_d1;
      Compressor_bh44_47: Compressor_23_3
      port map ( R => CompressorOut_bh44_47_47,
                 X0 => CompressorIn_bh44_47_82,
                 X1 => CompressorIn_bh44_47_83);
   heap_bh44_w32_4 <= CompressorOut_bh44_47_47(0); -- cycle= 1 cp= 0
   heap_bh44_w33_2 <= CompressorOut_bh44_47_47(1); -- cycle= 1 cp= 0
   heap_bh44_w34_2 <= CompressorOut_bh44_47_47(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_48_84 <= heap_bh44_w6_13_d1 & heap_bh44_w6_8_d1 & heap_bh44_w6_16;
      Compressor_bh44_48: Compressor_3_2
      port map ( R => CompressorOut_bh44_48_48,
                 X0 => CompressorIn_bh44_48_84);
   heap_bh44_w6_18 <= CompressorOut_bh44_48_48(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w7_16 <= CompressorOut_bh44_48_48(1); -- cycle= 1 cp= 5.3072e-10
   ----------------Synchro barrier, entering cycle 1----------------
   tempR_bh44_0 <= heap_bh44_w1_12 & heap_bh44_w0_9; -- already compressed

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_49_85 <= heap_bh44_w18_1_d1 & heap_bh44_w18_4 & heap_bh44_w18_5;
   CompressorIn_bh44_49_86 <= heap_bh44_w19_1_d1 & heap_bh44_w19_3;
      Compressor_bh44_49: Compressor_23_3
      port map ( R => CompressorOut_bh44_49_49,
                 X0 => CompressorIn_bh44_49_85,
                 X1 => CompressorIn_bh44_49_86);
   heap_bh44_w18_6 <= CompressorOut_bh44_49_49(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w19_4 <= CompressorOut_bh44_49_49(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w20_5 <= CompressorOut_bh44_49_49(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_50_87 <= heap_bh44_w21_0_d1 & heap_bh44_w21_1_d1 & heap_bh44_w21_3;
   CompressorIn_bh44_50_88 <= heap_bh44_w22_4 & heap_bh44_w22_3;
      Compressor_bh44_50: Compressor_23_3
      port map ( R => CompressorOut_bh44_50_50,
                 X0 => CompressorIn_bh44_50_87,
                 X1 => CompressorIn_bh44_50_88);
   heap_bh44_w21_4 <= CompressorOut_bh44_50_50(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w22_5 <= CompressorOut_bh44_50_50(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w23_4 <= CompressorOut_bh44_50_50(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_51_89 <= heap_bh44_w34_1_d1 & heap_bh44_w34_0_d1 & heap_bh44_w34_2;
   CompressorIn_bh44_51_90 <= heap_bh44_w35_1_d1 & heap_bh44_w35_0_d1;
      Compressor_bh44_51: Compressor_23_3
      port map ( R => CompressorOut_bh44_51_51,
                 X0 => CompressorIn_bh44_51_89,
                 X1 => CompressorIn_bh44_51_90);
   heap_bh44_w34_3 <= CompressorOut_bh44_51_51(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w35_2 <= CompressorOut_bh44_51_51(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh44_w36_2 <= CompressorOut_bh44_51_51(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_52_91 <= heap_bh44_w15_2_d1 & heap_bh44_w15_4 & heap_bh44_w15_5;
   CompressorIn_bh44_52_92(0) <= heap_bh44_w16_6;
      Compressor_bh44_52: Compressor_13_3
      port map ( R => CompressorOut_bh44_52_52,
                 X0 => CompressorIn_bh44_52_91,
                 X1 => CompressorIn_bh44_52_92);
   heap_bh44_w15_6 <= CompressorOut_bh44_52_52(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w16_7 <= CompressorOut_bh44_52_52(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w17_5 <= CompressorOut_bh44_52_52(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_53_93 <= heap_bh44_w23_1_d1 & heap_bh44_w23_3 & heap_bh44_w23_4;
   CompressorIn_bh44_53_94 <= heap_bh44_w24_4 & heap_bh44_w24_3;
      Compressor_bh44_53: Compressor_23_3
      port map ( R => CompressorOut_bh44_53_53,
                 X0 => CompressorIn_bh44_53_93,
                 X1 => CompressorIn_bh44_53_94);
   heap_bh44_w23_5 <= CompressorOut_bh44_53_53(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh44_w24_5 <= CompressorOut_bh44_53_53(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh44_w25_4 <= CompressorOut_bh44_53_53(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_54_95 <= heap_bh44_w36_1_d1 & heap_bh44_w36_0_d1 & heap_bh44_w36_2;
   CompressorIn_bh44_54_96 <= heap_bh44_w37_1_d1 & heap_bh44_w37_0_d1;
      Compressor_bh44_54: Compressor_23_3
      port map ( R => CompressorOut_bh44_54_54,
                 X0 => CompressorIn_bh44_54_95,
                 X1 => CompressorIn_bh44_54_96);
   heap_bh44_w36_3 <= CompressorOut_bh44_54_54(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w37_2 <= CompressorOut_bh44_54_54(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh44_w38_2 <= CompressorOut_bh44_54_54(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_55_97 <= heap_bh44_w25_1_d2 & heap_bh44_w25_3_d1 & heap_bh44_w25_4_d1;
   CompressorIn_bh44_55_98 <= heap_bh44_w26_4_d1 & heap_bh44_w26_3_d1;
      Compressor_bh44_55: Compressor_23_3
      port map ( R => CompressorOut_bh44_55_55,
                 X0 => CompressorIn_bh44_55_97,
                 X1 => CompressorIn_bh44_55_98);
   heap_bh44_w25_5 <= CompressorOut_bh44_55_55(0); -- cycle= 2 cp= 0
   heap_bh44_w26_5 <= CompressorOut_bh44_55_55(1); -- cycle= 2 cp= 0
   heap_bh44_w27_4 <= CompressorOut_bh44_55_55(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh44_56_99 <= heap_bh44_w38_1_d1 & heap_bh44_w38_0_d1 & heap_bh44_w38_2;
   CompressorIn_bh44_56_100 <= heap_bh44_w39_1_d1 & heap_bh44_w39_0_d1;
      Compressor_bh44_56: Compressor_23_3
      port map ( R => CompressorOut_bh44_56_56,
                 X0 => CompressorIn_bh44_56_99,
                 X1 => CompressorIn_bh44_56_100);
   heap_bh44_w38_3 <= CompressorOut_bh44_56_56(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh44_w39_2 <= CompressorOut_bh44_56_56(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh44_w40_2 <= CompressorOut_bh44_56_56(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_57_101 <= heap_bh44_w27_1_d2 & heap_bh44_w27_3_d1 & heap_bh44_w27_4;
   CompressorIn_bh44_57_102 <= heap_bh44_w28_4_d1 & heap_bh44_w28_3_d1;
      Compressor_bh44_57: Compressor_23_3
      port map ( R => CompressorOut_bh44_57_57,
                 X0 => CompressorIn_bh44_57_101,
                 X1 => CompressorIn_bh44_57_102);
   heap_bh44_w27_5 <= CompressorOut_bh44_57_57(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh44_w28_5 <= CompressorOut_bh44_57_57(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh44_w29_4 <= CompressorOut_bh44_57_57(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_58_103 <= heap_bh44_w40_1_d2 & heap_bh44_w40_0_d2 & heap_bh44_w40_2_d1;
   CompressorIn_bh44_58_104 <= heap_bh44_w41_1_d2 & heap_bh44_w41_0_d2;
      Compressor_bh44_58: Compressor_23_3
      port map ( R => CompressorOut_bh44_58_58,
                 X0 => CompressorIn_bh44_58_103,
                 X1 => CompressorIn_bh44_58_104);
   heap_bh44_w40_3 <= CompressorOut_bh44_58_58(0); -- cycle= 2 cp= 0
   heap_bh44_w41_2 <= CompressorOut_bh44_58_58(1); -- cycle= 2 cp= 0
   heap_bh44_w42_2 <= CompressorOut_bh44_58_58(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_59_105 <= heap_bh44_w29_1_d2 & heap_bh44_w29_3_d1 & heap_bh44_w29_4;
   CompressorIn_bh44_59_106 <= heap_bh44_w30_4_d1 & heap_bh44_w30_3_d1;
      Compressor_bh44_59: Compressor_23_3
      port map ( R => CompressorOut_bh44_59_59,
                 X0 => CompressorIn_bh44_59_105,
                 X1 => CompressorIn_bh44_59_106);
   heap_bh44_w29_5 <= CompressorOut_bh44_59_59(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh44_w30_5 <= CompressorOut_bh44_59_59(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh44_w31_4 <= CompressorOut_bh44_59_59(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_60_107 <= heap_bh44_w42_1_d2 & heap_bh44_w42_0_d2 & heap_bh44_w42_2;
      Compressor_bh44_60: Compressor_3_2
      port map ( R => CompressorOut_bh44_60_60,
                 X0 => CompressorIn_bh44_60_107);
   heap_bh44_w42_3 <= CompressorOut_bh44_60_60(0); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh44_61_108 <= heap_bh44_w31_1_d2 & heap_bh44_w31_3_d1 & heap_bh44_w31_4;
   CompressorIn_bh44_61_109 <= heap_bh44_w32_4_d1 & heap_bh44_w32_3_d1;
      Compressor_bh44_61: Compressor_23_3
      port map ( R => CompressorOut_bh44_61_61,
                 X0 => CompressorIn_bh44_61_108,
                 X1 => CompressorIn_bh44_61_109);
   heap_bh44_w31_5 <= CompressorOut_bh44_61_61(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh44_w32_5 <= CompressorOut_bh44_61_61(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh44_w33_3 <= CompressorOut_bh44_61_61(2); -- cycle= 2 cp= 1.59216e-09
   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   finalAdderIn0_bh44 <= "0" & heap_bh44_w42_3_d1 & heap_bh44_w41_2_d1 & heap_bh44_w40_3_d1 & heap_bh44_w39_2_d2 & heap_bh44_w38_3_d2 & heap_bh44_w37_2_d2 & heap_bh44_w36_3_d2 & heap_bh44_w35_2_d2 & heap_bh44_w34_3_d2 & heap_bh44_w33_2_d2 & heap_bh44_w32_5_d1 & heap_bh44_w31_5_d1 & heap_bh44_w30_5_d1 & heap_bh44_w29_5_d1 & heap_bh44_w28_5_d1 & heap_bh44_w27_5_d1 & heap_bh44_w26_5_d1 & heap_bh44_w25_5_d1 & heap_bh44_w24_5_d2 & heap_bh44_w23_5_d2 & heap_bh44_w22_5_d2 & heap_bh44_w21_4_d2 & heap_bh44_w20_4_d2 & heap_bh44_w19_4_d2 & heap_bh44_w18_6_d2 & heap_bh44_w17_4_d2 & heap_bh44_w16_7_d2 & heap_bh44_w15_6_d2 & heap_bh44_w14_7_d2 & heap_bh44_w13_9_d2 & heap_bh44_w12_11_d2 & heap_bh44_w11_13_d2 & heap_bh44_w10_11_d2 & heap_bh44_w9_15_d2 & heap_bh44_w8_15_d2 & heap_bh44_w7_16_d2 & heap_bh44_w6_18_d2 & heap_bh44_w5_17_d2 & heap_bh44_w4_17_d2 & heap_bh44_w3_15_d2 & heap_bh44_w2_14_d2;
   finalAdderIn1_bh44 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh44_w33_3_d1 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh44_w20_5_d2 & '0' & '0' & heap_bh44_w17_5_d2 & '0' & '0' & heap_bh44_w14_8_d2 & heap_bh44_w13_8_d2 & '0' & heap_bh44_w11_12_d2 & heap_bh44_w10_12_d2 & heap_bh44_w9_14_d2 & heap_bh44_w8_16_d2 & heap_bh44_w7_15_d2 & heap_bh44_w6_17_d2 & heap_bh44_w5_19_d2 & heap_bh44_w4_16_d2 & '0' & heap_bh44_w2_15_d2;
   finalAdderCin_bh44 <= '0';
      Adder_final44_0: IntAdder_42_f400_uid234  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh44,
                 R => finalAdderOut_bh44,
                 X => finalAdderIn0_bh44,
                 Y => finalAdderIn1_bh44);
   -- concatenate all the compressed chunks
   CompressionResult44 <= finalAdderOut_bh44 & tempR_bh44_0_d2;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult44(42 downto 6);
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_55_f400_uid391
--                    (IntAdderAlternative_55_F400_uid395)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2010)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_55_f400_uid391 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(54 downto 0);
          Y : in  std_logic_vector(54 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(54 downto 0)   );
end entity;

architecture arch of IntAdder_55_f400_uid391 is
signal s_sum_l0_idx0 :  std_logic_vector(42 downto 0);
signal s_sum_l0_idx1, s_sum_l0_idx1_d1 :  std_logic_vector(13 downto 0);
signal sum_l0_idx0, sum_l0_idx0_d1 :  std_logic_vector(41 downto 0);
signal c_l0_idx0, c_l0_idx0_d1 :  std_logic_vector(0 downto 0);
signal sum_l0_idx1 :  std_logic_vector(12 downto 0);
signal c_l0_idx1 :  std_logic_vector(0 downto 0);
signal s_sum_l1_idx1 :  std_logic_vector(13 downto 0);
signal sum_l1_idx1 :  std_logic_vector(12 downto 0);
signal c_l1_idx1 :  std_logic_vector(0 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            s_sum_l0_idx1_d1 <=  s_sum_l0_idx1;
            sum_l0_idx0_d1 <=  sum_l0_idx0;
            c_l0_idx0_d1 <=  c_l0_idx0;
         end if;
      end process;
   --Alternative
   s_sum_l0_idx0 <= ( "0" & X(41 downto 0)) + ( "0" & Y(41 downto 0)) + Cin;
   s_sum_l0_idx1 <= ( "0" & X(54 downto 42)) + ( "0" & Y(54 downto 42));
   sum_l0_idx0 <= s_sum_l0_idx0(41 downto 0);
   c_l0_idx0 <= s_sum_l0_idx0(42 downto 42);
   sum_l0_idx1 <= s_sum_l0_idx1(12 downto 0);
   c_l0_idx1 <= s_sum_l0_idx1(13 downto 13);
   ----------------Synchro barrier, entering cycle 1----------------
   s_sum_l1_idx1 <=  s_sum_l0_idx1_d1 + c_l0_idx0_d1(0 downto 0);
   sum_l1_idx1 <= s_sum_l1_idx1(12 downto 0);
   c_l1_idx1 <= s_sum_l1_idx1(13 downto 13);
   R <= sum_l1_idx1(12 downto 0) & sum_l0_idx0_d1(41 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                  FixMultAdd_37x37p48r48signed_F400_uid244
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan, 2012-2014
--------------------------------------------------------------------------------
-- Pipeline depth: 6 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixMultAdd_37x37p48r48signed_F400_uid244 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(36 downto 0);
          Y : in  std_logic_vector(36 downto 0);
          A : in  std_logic_vector(47 downto 0);
          R : out  std_logic_vector(47 downto 0)   );
end entity;

architecture arch of FixMultAdd_37x37p48r48signed_F400_uid244 is
   component IntAdder_55_f400_uid391 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(54 downto 0);
             Y : in  std_logic_vector(54 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(54 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid251 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid253 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid255 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid257 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid262 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid264 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid266 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid268 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component Compressor_14_3 is
      port ( X0 : in  std_logic_vector(3 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_23_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_13_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

signal XX_m247 :  std_logic_vector(36 downto 0);
signal YY_m247 :  std_logic_vector(36 downto 0);
signal Xp_m247b249 :  std_logic_vector(2 downto 0);
signal Yp_m247b249 :  std_logic_vector(2 downto 0);
signal x_m247b249_0 :  std_logic_vector(2 downto 0);
signal y_m247b249_0 :  std_logic_vector(2 downto 0);
signal Y0X0_249_m247 :  std_logic_vector(5 downto 0);
signal PP249X0Y0_m247 :  std_logic_vector(5 downto 0);
signal heap_bh245_w0_0 :  std_logic;
signal Xp_m247b260 :  std_logic_vector(8 downto 0);
signal Yp_m247b260 :  std_logic_vector(2 downto 0);
signal x_m247b260_0 :  std_logic_vector(2 downto 0);
signal x_m247b260_1 :  std_logic_vector(2 downto 0);
signal x_m247b260_2 :  std_logic_vector(2 downto 0);
signal y_m247b260_0 :  std_logic_vector(2 downto 0);
signal Y0X0_260_m247 :  std_logic_vector(5 downto 0);
signal PP260X0Y0_m247 :  std_logic_vector(5 downto 0);
signal heap_bh245_w0_1 :  std_logic;
signal heap_bh245_w1_0 :  std_logic;
signal heap_bh245_w2_0 :  std_logic;
signal Y0X1_260_m247 :  std_logic_vector(5 downto 0);
signal PP260X1Y0_m247 :  std_logic_vector(5 downto 0);
signal heap_bh245_w1_1 :  std_logic;
signal heap_bh245_w2_1 :  std_logic;
signal heap_bh245_w3_0, heap_bh245_w3_0_d1 :  std_logic;
signal heap_bh245_w4_0, heap_bh245_w4_0_d1 :  std_logic;
signal heap_bh245_w5_0 :  std_logic;
signal Y0X2_260_m247 :  std_logic_vector(5 downto 0);
signal PP260X2Y0_m247 :  std_logic_vector(5 downto 0);
signal heap_bh245_w4_1, heap_bh245_w4_1_d1 :  std_logic;
signal heap_bh245_w5_1 :  std_logic;
signal heap_bh245_w6_0 :  std_logic;
signal heap_bh245_w7_0 :  std_logic;
signal heap_bh245_w8_0 :  std_logic;
signal heap_bh245_w6_1 :  std_logic;
signal heap_bh245_w7_1 :  std_logic;
signal heap_bh245_w8_1 :  std_logic;
signal heap_bh245_w9_0 :  std_logic;
signal heap_bh245_w10_0 :  std_logic;
signal heap_bh245_w11_0 :  std_logic;
signal heap_bh245_w12_0, heap_bh245_w12_0_d1 :  std_logic;
signal heap_bh245_w13_0, heap_bh245_w13_0_d1 :  std_logic;
signal heap_bh245_w14_0, heap_bh245_w14_0_d1 :  std_logic;
signal heap_bh245_w15_0, heap_bh245_w15_0_d1 :  std_logic;
signal heap_bh245_w16_0, heap_bh245_w16_0_d1 :  std_logic;
signal heap_bh245_w17_0, heap_bh245_w17_0_d1 :  std_logic;
signal heap_bh245_w18_0, heap_bh245_w18_0_d1 :  std_logic;
signal heap_bh245_w19_0, heap_bh245_w19_0_d1 :  std_logic;
signal heap_bh245_w20_0, heap_bh245_w20_0_d1 :  std_logic;
signal heap_bh245_w21_0, heap_bh245_w21_0_d1 :  std_logic;
signal heap_bh245_w22_0, heap_bh245_w22_0_d1 :  std_logic;
signal heap_bh245_w23_0, heap_bh245_w23_0_d1 :  std_logic;
signal heap_bh245_w24_0, heap_bh245_w24_0_d1 :  std_logic;
signal heap_bh245_w25_0, heap_bh245_w25_0_d1 :  std_logic;
signal heap_bh245_w26_0, heap_bh245_w26_0_d1, heap_bh245_w26_0_d2 :  std_logic;
signal heap_bh245_w27_0, heap_bh245_w27_0_d1, heap_bh245_w27_0_d2 :  std_logic;
signal heap_bh245_w28_0, heap_bh245_w28_0_d1, heap_bh245_w28_0_d2 :  std_logic;
signal heap_bh245_w29_0, heap_bh245_w29_0_d1, heap_bh245_w29_0_d2 :  std_logic;
signal heap_bh245_w30_0, heap_bh245_w30_0_d1, heap_bh245_w30_0_d2 :  std_logic;
signal heap_bh245_w31_0, heap_bh245_w31_0_d1, heap_bh245_w31_0_d2 :  std_logic;
signal heap_bh245_w32_0, heap_bh245_w32_0_d1, heap_bh245_w32_0_d2 :  std_logic;
signal heap_bh245_w33_0, heap_bh245_w33_0_d1, heap_bh245_w33_0_d2 :  std_logic;
signal heap_bh245_w34_0, heap_bh245_w34_0_d1, heap_bh245_w34_0_d2 :  std_logic;
signal heap_bh245_w35_0, heap_bh245_w35_0_d1, heap_bh245_w35_0_d2 :  std_logic;
signal heap_bh245_w36_0, heap_bh245_w36_0_d1, heap_bh245_w36_0_d2 :  std_logic;
signal heap_bh245_w37_0, heap_bh245_w37_0_d1, heap_bh245_w37_0_d2 :  std_logic;
signal heap_bh245_w38_0, heap_bh245_w38_0_d1, heap_bh245_w38_0_d2 :  std_logic;
signal heap_bh245_w39_0, heap_bh245_w39_0_d1, heap_bh245_w39_0_d2 :  std_logic;
signal heap_bh245_w40_0, heap_bh245_w40_0_d1, heap_bh245_w40_0_d2 :  std_logic;
signal heap_bh245_w41_0, heap_bh245_w41_0_d1, heap_bh245_w41_0_d2 :  std_logic;
signal heap_bh245_w42_0, heap_bh245_w42_0_d1, heap_bh245_w42_0_d2 :  std_logic;
signal heap_bh245_w43_0, heap_bh245_w43_0_d1, heap_bh245_w43_0_d2 :  std_logic;
signal heap_bh245_w44_0, heap_bh245_w44_0_d1, heap_bh245_w44_0_d2 :  std_logic;
signal heap_bh245_w45_0, heap_bh245_w45_0_d1, heap_bh245_w45_0_d2 :  std_logic;
signal heap_bh245_w46_0, heap_bh245_w46_0_d1, heap_bh245_w46_0_d2 :  std_logic;
signal heap_bh245_w47_0, heap_bh245_w47_0_d1, heap_bh245_w47_0_d2 :  std_logic;
signal heap_bh245_w48_0, heap_bh245_w48_0_d1, heap_bh245_w48_0_d2 :  std_logic;
signal heap_bh245_w49_0, heap_bh245_w49_0_d1, heap_bh245_w49_0_d2 :  std_logic;
signal heap_bh245_w50_0, heap_bh245_w50_0_d1, heap_bh245_w50_0_d2, heap_bh245_w50_0_d3 :  std_logic;
signal heap_bh245_w51_0, heap_bh245_w51_0_d1, heap_bh245_w51_0_d2, heap_bh245_w51_0_d3 :  std_logic;
signal heap_bh245_w52_0, heap_bh245_w52_0_d1, heap_bh245_w52_0_d2, heap_bh245_w52_0_d3 :  std_logic;
signal heap_bh245_w53_0, heap_bh245_w53_0_d1, heap_bh245_w53_0_d2, heap_bh245_w53_0_d3 :  std_logic;
signal DSP_bh245_ch1_0 :  std_logic_vector(42 downto 0);
signal heap_bh245_w19_1, heap_bh245_w19_1_d1 :  std_logic;
signal heap_bh245_w18_1, heap_bh245_w18_1_d1 :  std_logic;
signal heap_bh245_w17_1, heap_bh245_w17_1_d1 :  std_logic;
signal heap_bh245_w16_1, heap_bh245_w16_1_d1 :  std_logic;
signal heap_bh245_w15_1, heap_bh245_w15_1_d1 :  std_logic;
signal heap_bh245_w14_1, heap_bh245_w14_1_d1 :  std_logic;
signal heap_bh245_w13_1, heap_bh245_w13_1_d1 :  std_logic;
signal heap_bh245_w12_1, heap_bh245_w12_1_d1 :  std_logic;
signal heap_bh245_w11_1, heap_bh245_w11_1_d1 :  std_logic;
signal heap_bh245_w10_1, heap_bh245_w10_1_d1 :  std_logic;
signal heap_bh245_w9_1, heap_bh245_w9_1_d1 :  std_logic;
signal heap_bh245_w8_2, heap_bh245_w8_2_d1 :  std_logic;
signal heap_bh245_w7_2, heap_bh245_w7_2_d1 :  std_logic;
signal heap_bh245_w6_2, heap_bh245_w6_2_d1 :  std_logic;
signal heap_bh245_w5_2, heap_bh245_w5_2_d1 :  std_logic;
signal heap_bh245_w4_2, heap_bh245_w4_2_d1, heap_bh245_w4_2_d2 :  std_logic;
signal heap_bh245_w3_1, heap_bh245_w3_1_d1 :  std_logic;
signal heap_bh245_w2_2, heap_bh245_w2_2_d1 :  std_logic;
signal heap_bh245_w1_2, heap_bh245_w1_2_d1 :  std_logic;
signal heap_bh245_w0_2, heap_bh245_w0_2_d1 :  std_logic;
signal DSP_bh245_ch2_0, DSP_bh245_ch2_0_d1 :  std_logic_vector(42 downto 0);
signal DSP_bh245_root2_1, DSP_bh245_root2_1_d1 :  std_logic_vector(42 downto 0);
signal DSP_bh245_ch2_1 :  std_logic_vector(43 downto 0);
signal heap_bh245_w0_3, heap_bh245_w0_3_d1, heap_bh245_w0_3_d2, heap_bh245_w0_3_d3, heap_bh245_w0_3_d4 :  std_logic;
signal heap_bh245_w44_1, heap_bh245_w44_1_d1 :  std_logic;
signal heap_bh245_w43_1, heap_bh245_w43_1_d1, heap_bh245_w43_1_d2, heap_bh245_w43_1_d3 :  std_logic;
signal heap_bh245_w42_1, heap_bh245_w42_1_d1 :  std_logic;
signal heap_bh245_w41_1, heap_bh245_w41_1_d1, heap_bh245_w41_1_d2, heap_bh245_w41_1_d3 :  std_logic;
signal heap_bh245_w40_1, heap_bh245_w40_1_d1 :  std_logic;
signal heap_bh245_w39_1, heap_bh245_w39_1_d1, heap_bh245_w39_1_d2 :  std_logic;
signal heap_bh245_w38_1, heap_bh245_w38_1_d1 :  std_logic;
signal heap_bh245_w37_1, heap_bh245_w37_1_d1, heap_bh245_w37_1_d2 :  std_logic;
signal heap_bh245_w36_1, heap_bh245_w36_1_d1 :  std_logic;
signal heap_bh245_w35_1, heap_bh245_w35_1_d1, heap_bh245_w35_1_d2 :  std_logic;
signal heap_bh245_w34_1, heap_bh245_w34_1_d1 :  std_logic;
signal heap_bh245_w33_1, heap_bh245_w33_1_d1, heap_bh245_w33_1_d2 :  std_logic;
signal heap_bh245_w32_1, heap_bh245_w32_1_d1 :  std_logic;
signal heap_bh245_w31_1, heap_bh245_w31_1_d1 :  std_logic;
signal heap_bh245_w30_1, heap_bh245_w30_1_d1 :  std_logic;
signal heap_bh245_w29_1, heap_bh245_w29_1_d1 :  std_logic;
signal heap_bh245_w28_1, heap_bh245_w28_1_d1 :  std_logic;
signal heap_bh245_w27_1, heap_bh245_w27_1_d1 :  std_logic;
signal heap_bh245_w26_1, heap_bh245_w26_1_d1 :  std_logic;
signal heap_bh245_w25_1, heap_bh245_w25_1_d1 :  std_logic;
signal heap_bh245_w24_1, heap_bh245_w24_1_d1 :  std_logic;
signal heap_bh245_w23_1, heap_bh245_w23_1_d1 :  std_logic;
signal heap_bh245_w22_1, heap_bh245_w22_1_d1 :  std_logic;
signal heap_bh245_w21_1, heap_bh245_w21_1_d1 :  std_logic;
signal heap_bh245_w20_1, heap_bh245_w20_1_d1 :  std_logic;
signal heap_bh245_w19_2, heap_bh245_w19_2_d1 :  std_logic;
signal heap_bh245_w18_2, heap_bh245_w18_2_d1 :  std_logic;
signal heap_bh245_w17_2, heap_bh245_w17_2_d1 :  std_logic;
signal heap_bh245_w16_2, heap_bh245_w16_2_d1 :  std_logic;
signal heap_bh245_w15_2, heap_bh245_w15_2_d1 :  std_logic;
signal heap_bh245_w14_2, heap_bh245_w14_2_d1 :  std_logic;
signal heap_bh245_w13_2, heap_bh245_w13_2_d1 :  std_logic;
signal heap_bh245_w12_2, heap_bh245_w12_2_d1 :  std_logic;
signal heap_bh245_w11_2, heap_bh245_w11_2_d1 :  std_logic;
signal heap_bh245_w10_2, heap_bh245_w10_2_d1 :  std_logic;
signal heap_bh245_w9_2, heap_bh245_w9_2_d1 :  std_logic;
signal heap_bh245_w8_3, heap_bh245_w8_3_d1 :  std_logic;
signal heap_bh245_w7_3, heap_bh245_w7_3_d1 :  std_logic;
signal heap_bh245_w6_3, heap_bh245_w6_3_d1 :  std_logic;
signal heap_bh245_w5_3, heap_bh245_w5_3_d1 :  std_logic;
signal heap_bh245_w4_3, heap_bh245_w4_3_d1, heap_bh245_w4_3_d2, heap_bh245_w4_3_d3, heap_bh245_w4_3_d4 :  std_logic;
signal heap_bh245_w3_2, heap_bh245_w3_2_d1 :  std_logic;
signal heap_bh245_w2_3, heap_bh245_w2_3_d1, heap_bh245_w2_3_d2, heap_bh245_w2_3_d3, heap_bh245_w2_3_d4 :  std_logic;
signal heap_bh245_w1_3, heap_bh245_w1_3_d1, heap_bh245_w1_3_d2, heap_bh245_w1_3_d3, heap_bh245_w1_3_d4 :  std_logic;
signal heap_bh245_w5_4 :  std_logic;
signal heap_bh245_w8_4 :  std_logic;
signal heap_bh245_w9_3 :  std_logic;
signal heap_bh245_w10_3 :  std_logic;
signal heap_bh245_w11_3 :  std_logic;
signal heap_bh245_w12_3, heap_bh245_w12_3_d1 :  std_logic;
signal heap_bh245_w13_3, heap_bh245_w13_3_d1 :  std_logic;
signal heap_bh245_w14_3, heap_bh245_w14_3_d1 :  std_logic;
signal heap_bh245_w15_3, heap_bh245_w15_3_d1 :  std_logic;
signal heap_bh245_w16_3, heap_bh245_w16_3_d1 :  std_logic;
signal heap_bh245_w17_3, heap_bh245_w17_3_d1 :  std_logic;
signal heap_bh245_w18_3, heap_bh245_w18_3_d1 :  std_logic;
signal heap_bh245_w20_2, heap_bh245_w20_2_d1 :  std_logic;
signal heap_bh245_w21_2, heap_bh245_w21_2_d1 :  std_logic;
signal heap_bh245_w22_2, heap_bh245_w22_2_d1 :  std_logic;
signal heap_bh245_w23_2, heap_bh245_w23_2_d1 :  std_logic;
signal heap_bh245_w24_2, heap_bh245_w24_2_d1 :  std_logic;
signal heap_bh245_w25_2, heap_bh245_w25_2_d1 :  std_logic;
signal heap_bh245_w26_2, heap_bh245_w26_2_d1, heap_bh245_w26_2_d2 :  std_logic;
signal heap_bh245_w27_2, heap_bh245_w27_2_d1, heap_bh245_w27_2_d2 :  std_logic;
signal heap_bh245_w28_2, heap_bh245_w28_2_d1, heap_bh245_w28_2_d2 :  std_logic;
signal heap_bh245_w29_2, heap_bh245_w29_2_d1, heap_bh245_w29_2_d2 :  std_logic;
signal heap_bh245_w30_2, heap_bh245_w30_2_d1, heap_bh245_w30_2_d2 :  std_logic;
signal heap_bh245_w31_2, heap_bh245_w31_2_d1, heap_bh245_w31_2_d2 :  std_logic;
signal heap_bh245_w32_2, heap_bh245_w32_2_d1, heap_bh245_w32_2_d2 :  std_logic;
signal heap_bh245_w33_2, heap_bh245_w33_2_d1, heap_bh245_w33_2_d2 :  std_logic;
signal heap_bh245_w34_2, heap_bh245_w34_2_d1, heap_bh245_w34_2_d2 :  std_logic;
signal heap_bh245_w35_2, heap_bh245_w35_2_d1, heap_bh245_w35_2_d2 :  std_logic;
signal heap_bh245_w36_2, heap_bh245_w36_2_d1, heap_bh245_w36_2_d2 :  std_logic;
signal heap_bh245_w37_2, heap_bh245_w37_2_d1, heap_bh245_w37_2_d2 :  std_logic;
signal heap_bh245_w38_2, heap_bh245_w38_2_d1, heap_bh245_w38_2_d2 :  std_logic;
signal heap_bh245_w39_2, heap_bh245_w39_2_d1, heap_bh245_w39_2_d2 :  std_logic;
signal heap_bh245_w40_2, heap_bh245_w40_2_d1, heap_bh245_w40_2_d2 :  std_logic;
signal heap_bh245_w41_2, heap_bh245_w41_2_d1, heap_bh245_w41_2_d2 :  std_logic;
signal heap_bh245_w42_2, heap_bh245_w42_2_d1, heap_bh245_w42_2_d2 :  std_logic;
signal heap_bh245_w43_2, heap_bh245_w43_2_d1, heap_bh245_w43_2_d2 :  std_logic;
signal heap_bh245_w45_1, heap_bh245_w45_1_d1, heap_bh245_w45_1_d2 :  std_logic;
signal heap_bh245_w46_1, heap_bh245_w46_1_d1, heap_bh245_w46_1_d2 :  std_logic;
signal heap_bh245_w47_1, heap_bh245_w47_1_d1, heap_bh245_w47_1_d2 :  std_logic;
signal heap_bh245_w48_1, heap_bh245_w48_1_d1, heap_bh245_w48_1_d2 :  std_logic;
signal heap_bh245_w49_1, heap_bh245_w49_1_d1, heap_bh245_w49_1_d2 :  std_logic;
signal heap_bh245_w50_1, heap_bh245_w50_1_d1, heap_bh245_w50_1_d2, heap_bh245_w50_1_d3 :  std_logic;
signal heap_bh245_w51_1, heap_bh245_w51_1_d1, heap_bh245_w51_1_d2, heap_bh245_w51_1_d3 :  std_logic;
signal heap_bh245_w52_1, heap_bh245_w52_1_d1, heap_bh245_w52_1_d2, heap_bh245_w52_1_d3 :  std_logic;
signal heap_bh245_w53_1, heap_bh245_w53_1_d1, heap_bh245_w53_1_d2, heap_bh245_w53_1_d3 :  std_logic;
signal inAdder0_bh245_0, inAdder0_bh245_0_d1 :  std_logic_vector(3 downto 0);
signal inAdder1_bh245_0, inAdder1_bh245_0_d1 :  std_logic_vector(3 downto 0);
signal cin_bh245_0, cin_bh245_0_d1 :  std_logic_vector(0 downto 0);
signal outAdder_bh245_0 :  std_logic_vector(3 downto 0);
signal heap_bh245_w0_4 :  std_logic;
signal heap_bh245_w1_4 :  std_logic;
signal heap_bh245_w2_4 :  std_logic;
signal heap_bh245_w3_3 :  std_logic;
signal CompressorIn_bh245_0_0 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_0_1 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh245_w5_5, heap_bh245_w5_5_d1 :  std_logic;
signal heap_bh245_w6_4, heap_bh245_w6_4_d1 :  std_logic;
signal heap_bh245_w7_4 :  std_logic;
signal CompressorIn_bh245_1_2 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_1_3 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh245_w8_5 :  std_logic;
signal heap_bh245_w9_4, heap_bh245_w9_4_d1 :  std_logic;
signal heap_bh245_w10_4 :  std_logic;
signal CompressorIn_bh245_2_4 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_2_5 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh245_w10_5, heap_bh245_w10_5_d1 :  std_logic;
signal heap_bh245_w11_4, heap_bh245_w11_4_d1 :  std_logic;
signal heap_bh245_w12_4, heap_bh245_w12_4_d1 :  std_logic;
signal CompressorIn_bh245_3_6 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_3_7 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh245_w7_5, heap_bh245_w7_5_d1 :  std_logic;
signal heap_bh245_w8_6, heap_bh245_w8_6_d1 :  std_logic;
signal heap_bh245_w9_5, heap_bh245_w9_5_d1 :  std_logic;
signal CompressorIn_bh245_4_8 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_4_9 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_4_4 :  std_logic_vector(2 downto 0);
signal heap_bh245_w12_5 :  std_logic;
signal heap_bh245_w13_4 :  std_logic;
signal heap_bh245_w14_4 :  std_logic;
signal inAdder0_bh245_1, inAdder0_bh245_1_d1 :  std_logic_vector(3 downto 0);
signal inAdder1_bh245_1, inAdder1_bh245_1_d1 :  std_logic_vector(3 downto 0);
signal cin_bh245_1, cin_bh245_1_d1 :  std_logic_vector(0 downto 0);
signal outAdder_bh245_1 :  std_logic_vector(3 downto 0);
signal heap_bh245_w0_5, heap_bh245_w0_5_d1, heap_bh245_w0_5_d2, heap_bh245_w0_5_d3 :  std_logic;
signal heap_bh245_w1_5, heap_bh245_w1_5_d1, heap_bh245_w1_5_d2, heap_bh245_w1_5_d3 :  std_logic;
signal heap_bh245_w2_5, heap_bh245_w2_5_d1, heap_bh245_w2_5_d2, heap_bh245_w2_5_d3 :  std_logic;
signal heap_bh245_w3_4 :  std_logic;
signal CompressorIn_bh245_5_10 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh245_5_11 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_5_5 :  std_logic_vector(2 downto 0);
signal heap_bh245_w14_5 :  std_logic;
signal heap_bh245_w15_4 :  std_logic;
signal heap_bh245_w16_4 :  std_logic;
signal CompressorIn_bh245_6_12 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_6_13 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_6_6 :  std_logic_vector(2 downto 0);
signal heap_bh245_w3_5, heap_bh245_w3_5_d1 :  std_logic;
signal heap_bh245_w4_4, heap_bh245_w4_4_d1 :  std_logic;
signal heap_bh245_w5_6 :  std_logic;
signal CompressorIn_bh245_7_14 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_7_15 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_7_7 :  std_logic_vector(2 downto 0);
signal heap_bh245_w9_6, heap_bh245_w9_6_d1 :  std_logic;
signal heap_bh245_w10_6, heap_bh245_w10_6_d1 :  std_logic;
signal heap_bh245_w11_5 :  std_logic;
signal CompressorIn_bh245_8_16 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_8_17 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_8_8 :  std_logic_vector(2 downto 0);
signal heap_bh245_w16_5 :  std_logic;
signal heap_bh245_w17_4 :  std_logic;
signal heap_bh245_w18_4 :  std_logic;
signal CompressorIn_bh245_9_18 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_9_19 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_9_9 :  std_logic_vector(2 downto 0);
signal heap_bh245_w18_5 :  std_logic;
signal heap_bh245_w19_3, heap_bh245_w19_3_d1 :  std_logic;
signal heap_bh245_w20_3 :  std_logic;
signal CompressorIn_bh245_10_20 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_10_21 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_10_10 :  std_logic_vector(2 downto 0);
signal heap_bh245_w5_7, heap_bh245_w5_7_d1 :  std_logic;
signal heap_bh245_w6_5, heap_bh245_w6_5_d1 :  std_logic;
signal heap_bh245_w7_6 :  std_logic;
signal CompressorIn_bh245_11_22 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_11_23 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_11_11 :  std_logic_vector(2 downto 0);
signal heap_bh245_w11_6, heap_bh245_w11_6_d1 :  std_logic;
signal heap_bh245_w12_6, heap_bh245_w12_6_d1 :  std_logic;
signal heap_bh245_w13_5 :  std_logic;
signal CompressorIn_bh245_12_24 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_12_25 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_12_12 :  std_logic_vector(2 downto 0);
signal heap_bh245_w15_5, heap_bh245_w15_5_d1 :  std_logic;
signal heap_bh245_w16_6, heap_bh245_w16_6_d1 :  std_logic;
signal heap_bh245_w17_5 :  std_logic;
signal CompressorIn_bh245_13_26 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_13_27 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_13_13 :  std_logic_vector(2 downto 0);
signal heap_bh245_w20_4, heap_bh245_w20_4_d1 :  std_logic;
signal heap_bh245_w21_3, heap_bh245_w21_3_d1 :  std_logic;
signal heap_bh245_w22_3 :  std_logic;
signal CompressorIn_bh245_14_28 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_14_29 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_14_14 :  std_logic_vector(2 downto 0);
signal heap_bh245_w7_7, heap_bh245_w7_7_d1 :  std_logic;
signal heap_bh245_w8_7, heap_bh245_w8_7_d1 :  std_logic;
signal heap_bh245_w9_7, heap_bh245_w9_7_d1 :  std_logic;
signal CompressorIn_bh245_15_30 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_15_31 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_15_15 :  std_logic_vector(2 downto 0);
signal heap_bh245_w17_6, heap_bh245_w17_6_d1 :  std_logic;
signal heap_bh245_w18_6, heap_bh245_w18_6_d1 :  std_logic;
signal heap_bh245_w19_4, heap_bh245_w19_4_d1 :  std_logic;
signal CompressorIn_bh245_16_32 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_16_33 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_16_16 :  std_logic_vector(2 downto 0);
signal heap_bh245_w22_4, heap_bh245_w22_4_d1 :  std_logic;
signal heap_bh245_w23_3, heap_bh245_w23_3_d1 :  std_logic;
signal heap_bh245_w24_3 :  std_logic;
signal CompressorIn_bh245_17_34 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_17_35 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_17_17 :  std_logic_vector(2 downto 0);
signal heap_bh245_w13_6, heap_bh245_w13_6_d1 :  std_logic;
signal heap_bh245_w14_6, heap_bh245_w14_6_d1 :  std_logic;
signal heap_bh245_w15_6, heap_bh245_w15_6_d1 :  std_logic;
signal CompressorIn_bh245_18_36 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_18_37 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_18_18 :  std_logic_vector(2 downto 0);
signal heap_bh245_w24_4, heap_bh245_w24_4_d1 :  std_logic;
signal heap_bh245_w25_3, heap_bh245_w25_3_d1 :  std_logic;
signal heap_bh245_w26_3, heap_bh245_w26_3_d1 :  std_logic;
signal CompressorIn_bh245_19_38 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh245_19_39 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_19_19 :  std_logic_vector(2 downto 0);
signal heap_bh245_w26_4 :  std_logic;
signal heap_bh245_w27_3 :  std_logic;
signal heap_bh245_w28_3 :  std_logic;
signal CompressorIn_bh245_20_40 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_20_41 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_20_20 :  std_logic_vector(2 downto 0);
signal heap_bh245_w3_6, heap_bh245_w3_6_d1, heap_bh245_w3_6_d2, heap_bh245_w3_6_d3 :  std_logic;
signal heap_bh245_w4_5, heap_bh245_w4_5_d1, heap_bh245_w4_5_d2, heap_bh245_w4_5_d3 :  std_logic;
signal heap_bh245_w5_8 :  std_logic;
signal CompressorIn_bh245_21_42 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_21_43 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_21_21 :  std_logic_vector(2 downto 0);
signal heap_bh245_w9_8, heap_bh245_w9_8_d1, heap_bh245_w9_8_d2, heap_bh245_w9_8_d3 :  std_logic;
signal heap_bh245_w10_7, heap_bh245_w10_7_d1, heap_bh245_w10_7_d2, heap_bh245_w10_7_d3 :  std_logic;
signal heap_bh245_w11_7 :  std_logic;
signal CompressorIn_bh245_22_44 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_22_45 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_22_22 :  std_logic_vector(2 downto 0);
signal heap_bh245_w15_7, heap_bh245_w15_7_d1, heap_bh245_w15_7_d2, heap_bh245_w15_7_d3 :  std_logic;
signal heap_bh245_w16_7, heap_bh245_w16_7_d1, heap_bh245_w16_7_d2, heap_bh245_w16_7_d3 :  std_logic;
signal heap_bh245_w17_7 :  std_logic;
signal CompressorIn_bh245_23_46 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_23_47 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_23_23 :  std_logic_vector(2 downto 0);
signal heap_bh245_w19_5, heap_bh245_w19_5_d1, heap_bh245_w19_5_d2, heap_bh245_w19_5_d3 :  std_logic;
signal heap_bh245_w20_5, heap_bh245_w20_5_d1, heap_bh245_w20_5_d2, heap_bh245_w20_5_d3 :  std_logic;
signal heap_bh245_w21_4 :  std_logic;
signal CompressorIn_bh245_24_48 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_24_49 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_24_24 :  std_logic_vector(2 downto 0);
signal heap_bh245_w28_4 :  std_logic;
signal heap_bh245_w29_3 :  std_logic;
signal heap_bh245_w30_3 :  std_logic;
signal CompressorIn_bh245_25_50 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_25_51 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_25_25 :  std_logic_vector(2 downto 0);
signal heap_bh245_w30_4 :  std_logic;
signal heap_bh245_w31_3 :  std_logic;
signal heap_bh245_w32_3 :  std_logic;
signal CompressorIn_bh245_26_52 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_26_53 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_26_26 :  std_logic_vector(2 downto 0);
signal heap_bh245_w32_4 :  std_logic;
signal heap_bh245_w33_3, heap_bh245_w33_3_d1 :  std_logic;
signal heap_bh245_w34_3, heap_bh245_w34_3_d1 :  std_logic;
signal CompressorIn_bh245_27_54 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_27_55 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_27_27 :  std_logic_vector(2 downto 0);
signal heap_bh245_w34_4, heap_bh245_w34_4_d1 :  std_logic;
signal heap_bh245_w35_3, heap_bh245_w35_3_d1 :  std_logic;
signal heap_bh245_w36_3, heap_bh245_w36_3_d1 :  std_logic;
signal CompressorIn_bh245_28_56 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_28_57 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_28_28 :  std_logic_vector(2 downto 0);
signal heap_bh245_w36_4, heap_bh245_w36_4_d1 :  std_logic;
signal heap_bh245_w37_3, heap_bh245_w37_3_d1 :  std_logic;
signal heap_bh245_w38_3, heap_bh245_w38_3_d1 :  std_logic;
signal CompressorIn_bh245_29_58 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_29_59 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_29_29 :  std_logic_vector(2 downto 0);
signal heap_bh245_w38_4, heap_bh245_w38_4_d1 :  std_logic;
signal heap_bh245_w39_3, heap_bh245_w39_3_d1 :  std_logic;
signal heap_bh245_w40_3, heap_bh245_w40_3_d1 :  std_logic;
signal CompressorIn_bh245_30_60 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_30_61 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_30_30 :  std_logic_vector(2 downto 0);
signal heap_bh245_w40_4, heap_bh245_w40_4_d1 :  std_logic;
signal heap_bh245_w41_3, heap_bh245_w41_3_d1, heap_bh245_w41_3_d2 :  std_logic;
signal heap_bh245_w42_3, heap_bh245_w42_3_d1, heap_bh245_w42_3_d2 :  std_logic;
signal CompressorIn_bh245_31_62 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_31_63 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_31_31 :  std_logic_vector(2 downto 0);
signal heap_bh245_w42_4, heap_bh245_w42_4_d1, heap_bh245_w42_4_d2 :  std_logic;
signal heap_bh245_w43_3, heap_bh245_w43_3_d1, heap_bh245_w43_3_d2 :  std_logic;
signal heap_bh245_w44_2 :  std_logic;
signal CompressorIn_bh245_32_64 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_32_65 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_32_32 :  std_logic_vector(2 downto 0);
signal heap_bh245_w5_9, heap_bh245_w5_9_d1, heap_bh245_w5_9_d2, heap_bh245_w5_9_d3 :  std_logic;
signal heap_bh245_w6_6, heap_bh245_w6_6_d1, heap_bh245_w6_6_d2, heap_bh245_w6_6_d3 :  std_logic;
signal heap_bh245_w7_8 :  std_logic;
signal CompressorIn_bh245_33_66 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_33_67 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_33_33 :  std_logic_vector(2 downto 0);
signal heap_bh245_w11_8, heap_bh245_w11_8_d1, heap_bh245_w11_8_d2, heap_bh245_w11_8_d3 :  std_logic;
signal heap_bh245_w12_7, heap_bh245_w12_7_d1, heap_bh245_w12_7_d2, heap_bh245_w12_7_d3 :  std_logic;
signal heap_bh245_w13_7 :  std_logic;
signal CompressorIn_bh245_34_68 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_34_69 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_34_34 :  std_logic_vector(2 downto 0);
signal heap_bh245_w17_8, heap_bh245_w17_8_d1, heap_bh245_w17_8_d2, heap_bh245_w17_8_d3 :  std_logic;
signal heap_bh245_w18_7, heap_bh245_w18_7_d1, heap_bh245_w18_7_d2, heap_bh245_w18_7_d3 :  std_logic;
signal heap_bh245_w19_6, heap_bh245_w19_6_d1, heap_bh245_w19_6_d2, heap_bh245_w19_6_d3 :  std_logic;
signal CompressorIn_bh245_35_70 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_35_71 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_35_35 :  std_logic_vector(2 downto 0);
signal heap_bh245_w21_5, heap_bh245_w21_5_d1, heap_bh245_w21_5_d2, heap_bh245_w21_5_d3 :  std_logic;
signal heap_bh245_w22_5, heap_bh245_w22_5_d1, heap_bh245_w22_5_d2, heap_bh245_w22_5_d3 :  std_logic;
signal heap_bh245_w23_4 :  std_logic;
signal CompressorIn_bh245_36_72 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_36_73 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_36_36 :  std_logic_vector(2 downto 0);
signal heap_bh245_w27_4, heap_bh245_w27_4_d1, heap_bh245_w27_4_d2, heap_bh245_w27_4_d3 :  std_logic;
signal heap_bh245_w28_5, heap_bh245_w28_5_d1, heap_bh245_w28_5_d2, heap_bh245_w28_5_d3 :  std_logic;
signal heap_bh245_w29_4 :  std_logic;
signal CompressorIn_bh245_37_74 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_37_75 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_37_37 :  std_logic_vector(2 downto 0);
signal heap_bh245_w44_3, heap_bh245_w44_3_d1, heap_bh245_w44_3_d2 :  std_logic;
signal heap_bh245_w45_2, heap_bh245_w45_2_d1, heap_bh245_w45_2_d2, heap_bh245_w45_2_d3 :  std_logic;
signal heap_bh245_w46_2 :  std_logic;
signal CompressorIn_bh245_38_76 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_38_77 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_38_38 :  std_logic_vector(2 downto 0);
signal heap_bh245_w7_9, heap_bh245_w7_9_d1, heap_bh245_w7_9_d2, heap_bh245_w7_9_d3 :  std_logic;
signal heap_bh245_w8_8, heap_bh245_w8_8_d1, heap_bh245_w8_8_d2, heap_bh245_w8_8_d3 :  std_logic;
signal heap_bh245_w9_9, heap_bh245_w9_9_d1, heap_bh245_w9_9_d2, heap_bh245_w9_9_d3 :  std_logic;
signal CompressorIn_bh245_39_78 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_39_79 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_39_39 :  std_logic_vector(2 downto 0);
signal heap_bh245_w13_8, heap_bh245_w13_8_d1, heap_bh245_w13_8_d2, heap_bh245_w13_8_d3 :  std_logic;
signal heap_bh245_w14_7, heap_bh245_w14_7_d1, heap_bh245_w14_7_d2, heap_bh245_w14_7_d3 :  std_logic;
signal heap_bh245_w15_8, heap_bh245_w15_8_d1, heap_bh245_w15_8_d2, heap_bh245_w15_8_d3 :  std_logic;
signal CompressorIn_bh245_40_80 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_40_81 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_40_40 :  std_logic_vector(2 downto 0);
signal heap_bh245_w23_5, heap_bh245_w23_5_d1, heap_bh245_w23_5_d2, heap_bh245_w23_5_d3 :  std_logic;
signal heap_bh245_w24_5, heap_bh245_w24_5_d1, heap_bh245_w24_5_d2, heap_bh245_w24_5_d3 :  std_logic;
signal heap_bh245_w25_4 :  std_logic;
signal CompressorIn_bh245_41_82 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_41_83 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_41_41 :  std_logic_vector(2 downto 0);
signal heap_bh245_w29_5, heap_bh245_w29_5_d1, heap_bh245_w29_5_d2, heap_bh245_w29_5_d3 :  std_logic;
signal heap_bh245_w30_5, heap_bh245_w30_5_d1, heap_bh245_w30_5_d2, heap_bh245_w30_5_d3 :  std_logic;
signal heap_bh245_w31_4 :  std_logic;
signal CompressorIn_bh245_42_84 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_42_85 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_42_42 :  std_logic_vector(2 downto 0);
signal heap_bh245_w46_3, heap_bh245_w46_3_d1, heap_bh245_w46_3_d2, heap_bh245_w46_3_d3 :  std_logic;
signal heap_bh245_w47_2, heap_bh245_w47_2_d1, heap_bh245_w47_2_d2, heap_bh245_w47_2_d3 :  std_logic;
signal heap_bh245_w48_2 :  std_logic;
signal CompressorIn_bh245_43_86 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_43_87 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_43_43 :  std_logic_vector(2 downto 0);
signal heap_bh245_w31_5, heap_bh245_w31_5_d1, heap_bh245_w31_5_d2, heap_bh245_w31_5_d3 :  std_logic;
signal heap_bh245_w32_5, heap_bh245_w32_5_d1, heap_bh245_w32_5_d2, heap_bh245_w32_5_d3 :  std_logic;
signal heap_bh245_w33_4, heap_bh245_w33_4_d1 :  std_logic;
signal CompressorIn_bh245_44_88 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_44_89 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_44_44 :  std_logic_vector(2 downto 0);
signal heap_bh245_w48_3, heap_bh245_w48_3_d1, heap_bh245_w48_3_d2, heap_bh245_w48_3_d3 :  std_logic;
signal heap_bh245_w49_2, heap_bh245_w49_2_d1, heap_bh245_w49_2_d2, heap_bh245_w49_2_d3 :  std_logic;
signal heap_bh245_w50_2, heap_bh245_w50_2_d1 :  std_logic;
signal CompressorIn_bh245_45_90 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_45_91 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_45_45 :  std_logic_vector(2 downto 0);
signal heap_bh245_w25_5, heap_bh245_w25_5_d1, heap_bh245_w25_5_d2, heap_bh245_w25_5_d3 :  std_logic;
signal heap_bh245_w26_5, heap_bh245_w26_5_d1, heap_bh245_w26_5_d2, heap_bh245_w26_5_d3 :  std_logic;
signal heap_bh245_w27_5, heap_bh245_w27_5_d1, heap_bh245_w27_5_d2, heap_bh245_w27_5_d3 :  std_logic;
signal CompressorIn_bh245_46_92 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_46_93 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_46_46 :  std_logic_vector(2 downto 0);
signal heap_bh245_w33_5, heap_bh245_w33_5_d1, heap_bh245_w33_5_d2 :  std_logic;
signal heap_bh245_w34_5, heap_bh245_w34_5_d1, heap_bh245_w34_5_d2 :  std_logic;
signal heap_bh245_w35_4 :  std_logic;
signal CompressorIn_bh245_47_94 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_47_95 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_47_47 :  std_logic_vector(2 downto 0);
signal heap_bh245_w50_3, heap_bh245_w50_3_d1, heap_bh245_w50_3_d2 :  std_logic;
signal heap_bh245_w51_2, heap_bh245_w51_2_d1, heap_bh245_w51_2_d2 :  std_logic;
signal heap_bh245_w52_2 :  std_logic;
signal CompressorIn_bh245_48_96 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_48_97 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_48_48 :  std_logic_vector(2 downto 0);
signal heap_bh245_w35_5, heap_bh245_w35_5_d1, heap_bh245_w35_5_d2 :  std_logic;
signal heap_bh245_w36_5, heap_bh245_w36_5_d1, heap_bh245_w36_5_d2 :  std_logic;
signal heap_bh245_w37_4 :  std_logic;
signal CompressorIn_bh245_49_98 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_49_99 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_49_49 :  std_logic_vector(2 downto 0);
signal heap_bh245_w52_3, heap_bh245_w52_3_d1, heap_bh245_w52_3_d2 :  std_logic;
signal heap_bh245_w53_2, heap_bh245_w53_2_d1, heap_bh245_w53_2_d2 :  std_logic;
signal CompressorIn_bh245_50_100 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_50_101 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_50_50 :  std_logic_vector(2 downto 0);
signal heap_bh245_w37_5, heap_bh245_w37_5_d1, heap_bh245_w37_5_d2 :  std_logic;
signal heap_bh245_w38_5, heap_bh245_w38_5_d1, heap_bh245_w38_5_d2 :  std_logic;
signal heap_bh245_w39_4 :  std_logic;
signal CompressorIn_bh245_51_102 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_51_103 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_51_51 :  std_logic_vector(2 downto 0);
signal heap_bh245_w39_5, heap_bh245_w39_5_d1, heap_bh245_w39_5_d2 :  std_logic;
signal heap_bh245_w40_5, heap_bh245_w40_5_d1, heap_bh245_w40_5_d2 :  std_logic;
signal heap_bh245_w41_4, heap_bh245_w41_4_d1 :  std_logic;
signal CompressorIn_bh245_52_104 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_52_105 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh245_52_52 :  std_logic_vector(2 downto 0);
signal heap_bh245_w41_5, heap_bh245_w41_5_d1 :  std_logic;
signal heap_bh245_w42_5, heap_bh245_w42_5_d1 :  std_logic;
signal heap_bh245_w43_4 :  std_logic;
signal CompressorIn_bh245_53_106 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh245_53_107 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh245_53_53 :  std_logic_vector(2 downto 0);
signal heap_bh245_w43_5, heap_bh245_w43_5_d1 :  std_logic;
signal heap_bh245_w44_4, heap_bh245_w44_4_d1 :  std_logic;
signal heap_bh245_w45_3, heap_bh245_w45_3_d1 :  std_logic;
signal finalAdderIn0_bh245 :  std_logic_vector(54 downto 0);
signal finalAdderIn1_bh245 :  std_logic_vector(54 downto 0);
signal finalAdderCin_bh245 :  std_logic;
signal finalAdderOut_bh245 :  std_logic_vector(54 downto 0);
signal CompressionResult245 :  std_logic_vector(54 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of SmallMultTableP3x3r6XsYu_F400_uid264: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid251: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid262: component is "yes";
attribute rom_style of SmallMultTableP3x3r6XsYu_F400_uid264: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid251: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid262: component is "distributed";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            heap_bh245_w3_0_d1 <=  heap_bh245_w3_0;
            heap_bh245_w4_0_d1 <=  heap_bh245_w4_0;
            heap_bh245_w4_1_d1 <=  heap_bh245_w4_1;
            heap_bh245_w12_0_d1 <=  heap_bh245_w12_0;
            heap_bh245_w13_0_d1 <=  heap_bh245_w13_0;
            heap_bh245_w14_0_d1 <=  heap_bh245_w14_0;
            heap_bh245_w15_0_d1 <=  heap_bh245_w15_0;
            heap_bh245_w16_0_d1 <=  heap_bh245_w16_0;
            heap_bh245_w17_0_d1 <=  heap_bh245_w17_0;
            heap_bh245_w18_0_d1 <=  heap_bh245_w18_0;
            heap_bh245_w19_0_d1 <=  heap_bh245_w19_0;
            heap_bh245_w20_0_d1 <=  heap_bh245_w20_0;
            heap_bh245_w21_0_d1 <=  heap_bh245_w21_0;
            heap_bh245_w22_0_d1 <=  heap_bh245_w22_0;
            heap_bh245_w23_0_d1 <=  heap_bh245_w23_0;
            heap_bh245_w24_0_d1 <=  heap_bh245_w24_0;
            heap_bh245_w25_0_d1 <=  heap_bh245_w25_0;
            heap_bh245_w26_0_d1 <=  heap_bh245_w26_0;
            heap_bh245_w26_0_d2 <=  heap_bh245_w26_0_d1;
            heap_bh245_w27_0_d1 <=  heap_bh245_w27_0;
            heap_bh245_w27_0_d2 <=  heap_bh245_w27_0_d1;
            heap_bh245_w28_0_d1 <=  heap_bh245_w28_0;
            heap_bh245_w28_0_d2 <=  heap_bh245_w28_0_d1;
            heap_bh245_w29_0_d1 <=  heap_bh245_w29_0;
            heap_bh245_w29_0_d2 <=  heap_bh245_w29_0_d1;
            heap_bh245_w30_0_d1 <=  heap_bh245_w30_0;
            heap_bh245_w30_0_d2 <=  heap_bh245_w30_0_d1;
            heap_bh245_w31_0_d1 <=  heap_bh245_w31_0;
            heap_bh245_w31_0_d2 <=  heap_bh245_w31_0_d1;
            heap_bh245_w32_0_d1 <=  heap_bh245_w32_0;
            heap_bh245_w32_0_d2 <=  heap_bh245_w32_0_d1;
            heap_bh245_w33_0_d1 <=  heap_bh245_w33_0;
            heap_bh245_w33_0_d2 <=  heap_bh245_w33_0_d1;
            heap_bh245_w34_0_d1 <=  heap_bh245_w34_0;
            heap_bh245_w34_0_d2 <=  heap_bh245_w34_0_d1;
            heap_bh245_w35_0_d1 <=  heap_bh245_w35_0;
            heap_bh245_w35_0_d2 <=  heap_bh245_w35_0_d1;
            heap_bh245_w36_0_d1 <=  heap_bh245_w36_0;
            heap_bh245_w36_0_d2 <=  heap_bh245_w36_0_d1;
            heap_bh245_w37_0_d1 <=  heap_bh245_w37_0;
            heap_bh245_w37_0_d2 <=  heap_bh245_w37_0_d1;
            heap_bh245_w38_0_d1 <=  heap_bh245_w38_0;
            heap_bh245_w38_0_d2 <=  heap_bh245_w38_0_d1;
            heap_bh245_w39_0_d1 <=  heap_bh245_w39_0;
            heap_bh245_w39_0_d2 <=  heap_bh245_w39_0_d1;
            heap_bh245_w40_0_d1 <=  heap_bh245_w40_0;
            heap_bh245_w40_0_d2 <=  heap_bh245_w40_0_d1;
            heap_bh245_w41_0_d1 <=  heap_bh245_w41_0;
            heap_bh245_w41_0_d2 <=  heap_bh245_w41_0_d1;
            heap_bh245_w42_0_d1 <=  heap_bh245_w42_0;
            heap_bh245_w42_0_d2 <=  heap_bh245_w42_0_d1;
            heap_bh245_w43_0_d1 <=  heap_bh245_w43_0;
            heap_bh245_w43_0_d2 <=  heap_bh245_w43_0_d1;
            heap_bh245_w44_0_d1 <=  heap_bh245_w44_0;
            heap_bh245_w44_0_d2 <=  heap_bh245_w44_0_d1;
            heap_bh245_w45_0_d1 <=  heap_bh245_w45_0;
            heap_bh245_w45_0_d2 <=  heap_bh245_w45_0_d1;
            heap_bh245_w46_0_d1 <=  heap_bh245_w46_0;
            heap_bh245_w46_0_d2 <=  heap_bh245_w46_0_d1;
            heap_bh245_w47_0_d1 <=  heap_bh245_w47_0;
            heap_bh245_w47_0_d2 <=  heap_bh245_w47_0_d1;
            heap_bh245_w48_0_d1 <=  heap_bh245_w48_0;
            heap_bh245_w48_0_d2 <=  heap_bh245_w48_0_d1;
            heap_bh245_w49_0_d1 <=  heap_bh245_w49_0;
            heap_bh245_w49_0_d2 <=  heap_bh245_w49_0_d1;
            heap_bh245_w50_0_d1 <=  heap_bh245_w50_0;
            heap_bh245_w50_0_d2 <=  heap_bh245_w50_0_d1;
            heap_bh245_w50_0_d3 <=  heap_bh245_w50_0_d2;
            heap_bh245_w51_0_d1 <=  heap_bh245_w51_0;
            heap_bh245_w51_0_d2 <=  heap_bh245_w51_0_d1;
            heap_bh245_w51_0_d3 <=  heap_bh245_w51_0_d2;
            heap_bh245_w52_0_d1 <=  heap_bh245_w52_0;
            heap_bh245_w52_0_d2 <=  heap_bh245_w52_0_d1;
            heap_bh245_w52_0_d3 <=  heap_bh245_w52_0_d2;
            heap_bh245_w53_0_d1 <=  heap_bh245_w53_0;
            heap_bh245_w53_0_d2 <=  heap_bh245_w53_0_d1;
            heap_bh245_w53_0_d3 <=  heap_bh245_w53_0_d2;
            heap_bh245_w19_1_d1 <=  heap_bh245_w19_1;
            heap_bh245_w18_1_d1 <=  heap_bh245_w18_1;
            heap_bh245_w17_1_d1 <=  heap_bh245_w17_1;
            heap_bh245_w16_1_d1 <=  heap_bh245_w16_1;
            heap_bh245_w15_1_d1 <=  heap_bh245_w15_1;
            heap_bh245_w14_1_d1 <=  heap_bh245_w14_1;
            heap_bh245_w13_1_d1 <=  heap_bh245_w13_1;
            heap_bh245_w12_1_d1 <=  heap_bh245_w12_1;
            heap_bh245_w11_1_d1 <=  heap_bh245_w11_1;
            heap_bh245_w10_1_d1 <=  heap_bh245_w10_1;
            heap_bh245_w9_1_d1 <=  heap_bh245_w9_1;
            heap_bh245_w8_2_d1 <=  heap_bh245_w8_2;
            heap_bh245_w7_2_d1 <=  heap_bh245_w7_2;
            heap_bh245_w6_2_d1 <=  heap_bh245_w6_2;
            heap_bh245_w5_2_d1 <=  heap_bh245_w5_2;
            heap_bh245_w4_2_d1 <=  heap_bh245_w4_2;
            heap_bh245_w4_2_d2 <=  heap_bh245_w4_2_d1;
            heap_bh245_w3_1_d1 <=  heap_bh245_w3_1;
            heap_bh245_w2_2_d1 <=  heap_bh245_w2_2;
            heap_bh245_w1_2_d1 <=  heap_bh245_w1_2;
            heap_bh245_w0_2_d1 <=  heap_bh245_w0_2;
            DSP_bh245_ch2_0_d1 <=  DSP_bh245_ch2_0;
            DSP_bh245_root2_1_d1 <=  DSP_bh245_root2_1;
            heap_bh245_w0_3_d1 <=  heap_bh245_w0_3;
            heap_bh245_w0_3_d2 <=  heap_bh245_w0_3_d1;
            heap_bh245_w0_3_d3 <=  heap_bh245_w0_3_d2;
            heap_bh245_w0_3_d4 <=  heap_bh245_w0_3_d3;
            heap_bh245_w44_1_d1 <=  heap_bh245_w44_1;
            heap_bh245_w43_1_d1 <=  heap_bh245_w43_1;
            heap_bh245_w43_1_d2 <=  heap_bh245_w43_1_d1;
            heap_bh245_w43_1_d3 <=  heap_bh245_w43_1_d2;
            heap_bh245_w42_1_d1 <=  heap_bh245_w42_1;
            heap_bh245_w41_1_d1 <=  heap_bh245_w41_1;
            heap_bh245_w41_1_d2 <=  heap_bh245_w41_1_d1;
            heap_bh245_w41_1_d3 <=  heap_bh245_w41_1_d2;
            heap_bh245_w40_1_d1 <=  heap_bh245_w40_1;
            heap_bh245_w39_1_d1 <=  heap_bh245_w39_1;
            heap_bh245_w39_1_d2 <=  heap_bh245_w39_1_d1;
            heap_bh245_w38_1_d1 <=  heap_bh245_w38_1;
            heap_bh245_w37_1_d1 <=  heap_bh245_w37_1;
            heap_bh245_w37_1_d2 <=  heap_bh245_w37_1_d1;
            heap_bh245_w36_1_d1 <=  heap_bh245_w36_1;
            heap_bh245_w35_1_d1 <=  heap_bh245_w35_1;
            heap_bh245_w35_1_d2 <=  heap_bh245_w35_1_d1;
            heap_bh245_w34_1_d1 <=  heap_bh245_w34_1;
            heap_bh245_w33_1_d1 <=  heap_bh245_w33_1;
            heap_bh245_w33_1_d2 <=  heap_bh245_w33_1_d1;
            heap_bh245_w32_1_d1 <=  heap_bh245_w32_1;
            heap_bh245_w31_1_d1 <=  heap_bh245_w31_1;
            heap_bh245_w30_1_d1 <=  heap_bh245_w30_1;
            heap_bh245_w29_1_d1 <=  heap_bh245_w29_1;
            heap_bh245_w28_1_d1 <=  heap_bh245_w28_1;
            heap_bh245_w27_1_d1 <=  heap_bh245_w27_1;
            heap_bh245_w26_1_d1 <=  heap_bh245_w26_1;
            heap_bh245_w25_1_d1 <=  heap_bh245_w25_1;
            heap_bh245_w24_1_d1 <=  heap_bh245_w24_1;
            heap_bh245_w23_1_d1 <=  heap_bh245_w23_1;
            heap_bh245_w22_1_d1 <=  heap_bh245_w22_1;
            heap_bh245_w21_1_d1 <=  heap_bh245_w21_1;
            heap_bh245_w20_1_d1 <=  heap_bh245_w20_1;
            heap_bh245_w19_2_d1 <=  heap_bh245_w19_2;
            heap_bh245_w18_2_d1 <=  heap_bh245_w18_2;
            heap_bh245_w17_2_d1 <=  heap_bh245_w17_2;
            heap_bh245_w16_2_d1 <=  heap_bh245_w16_2;
            heap_bh245_w15_2_d1 <=  heap_bh245_w15_2;
            heap_bh245_w14_2_d1 <=  heap_bh245_w14_2;
            heap_bh245_w13_2_d1 <=  heap_bh245_w13_2;
            heap_bh245_w12_2_d1 <=  heap_bh245_w12_2;
            heap_bh245_w11_2_d1 <=  heap_bh245_w11_2;
            heap_bh245_w10_2_d1 <=  heap_bh245_w10_2;
            heap_bh245_w9_2_d1 <=  heap_bh245_w9_2;
            heap_bh245_w8_3_d1 <=  heap_bh245_w8_3;
            heap_bh245_w7_3_d1 <=  heap_bh245_w7_3;
            heap_bh245_w6_3_d1 <=  heap_bh245_w6_3;
            heap_bh245_w5_3_d1 <=  heap_bh245_w5_3;
            heap_bh245_w4_3_d1 <=  heap_bh245_w4_3;
            heap_bh245_w4_3_d2 <=  heap_bh245_w4_3_d1;
            heap_bh245_w4_3_d3 <=  heap_bh245_w4_3_d2;
            heap_bh245_w4_3_d4 <=  heap_bh245_w4_3_d3;
            heap_bh245_w3_2_d1 <=  heap_bh245_w3_2;
            heap_bh245_w2_3_d1 <=  heap_bh245_w2_3;
            heap_bh245_w2_3_d2 <=  heap_bh245_w2_3_d1;
            heap_bh245_w2_3_d3 <=  heap_bh245_w2_3_d2;
            heap_bh245_w2_3_d4 <=  heap_bh245_w2_3_d3;
            heap_bh245_w1_3_d1 <=  heap_bh245_w1_3;
            heap_bh245_w1_3_d2 <=  heap_bh245_w1_3_d1;
            heap_bh245_w1_3_d3 <=  heap_bh245_w1_3_d2;
            heap_bh245_w1_3_d4 <=  heap_bh245_w1_3_d3;
            heap_bh245_w12_3_d1 <=  heap_bh245_w12_3;
            heap_bh245_w13_3_d1 <=  heap_bh245_w13_3;
            heap_bh245_w14_3_d1 <=  heap_bh245_w14_3;
            heap_bh245_w15_3_d1 <=  heap_bh245_w15_3;
            heap_bh245_w16_3_d1 <=  heap_bh245_w16_3;
            heap_bh245_w17_3_d1 <=  heap_bh245_w17_3;
            heap_bh245_w18_3_d1 <=  heap_bh245_w18_3;
            heap_bh245_w20_2_d1 <=  heap_bh245_w20_2;
            heap_bh245_w21_2_d1 <=  heap_bh245_w21_2;
            heap_bh245_w22_2_d1 <=  heap_bh245_w22_2;
            heap_bh245_w23_2_d1 <=  heap_bh245_w23_2;
            heap_bh245_w24_2_d1 <=  heap_bh245_w24_2;
            heap_bh245_w25_2_d1 <=  heap_bh245_w25_2;
            heap_bh245_w26_2_d1 <=  heap_bh245_w26_2;
            heap_bh245_w26_2_d2 <=  heap_bh245_w26_2_d1;
            heap_bh245_w27_2_d1 <=  heap_bh245_w27_2;
            heap_bh245_w27_2_d2 <=  heap_bh245_w27_2_d1;
            heap_bh245_w28_2_d1 <=  heap_bh245_w28_2;
            heap_bh245_w28_2_d2 <=  heap_bh245_w28_2_d1;
            heap_bh245_w29_2_d1 <=  heap_bh245_w29_2;
            heap_bh245_w29_2_d2 <=  heap_bh245_w29_2_d1;
            heap_bh245_w30_2_d1 <=  heap_bh245_w30_2;
            heap_bh245_w30_2_d2 <=  heap_bh245_w30_2_d1;
            heap_bh245_w31_2_d1 <=  heap_bh245_w31_2;
            heap_bh245_w31_2_d2 <=  heap_bh245_w31_2_d1;
            heap_bh245_w32_2_d1 <=  heap_bh245_w32_2;
            heap_bh245_w32_2_d2 <=  heap_bh245_w32_2_d1;
            heap_bh245_w33_2_d1 <=  heap_bh245_w33_2;
            heap_bh245_w33_2_d2 <=  heap_bh245_w33_2_d1;
            heap_bh245_w34_2_d1 <=  heap_bh245_w34_2;
            heap_bh245_w34_2_d2 <=  heap_bh245_w34_2_d1;
            heap_bh245_w35_2_d1 <=  heap_bh245_w35_2;
            heap_bh245_w35_2_d2 <=  heap_bh245_w35_2_d1;
            heap_bh245_w36_2_d1 <=  heap_bh245_w36_2;
            heap_bh245_w36_2_d2 <=  heap_bh245_w36_2_d1;
            heap_bh245_w37_2_d1 <=  heap_bh245_w37_2;
            heap_bh245_w37_2_d2 <=  heap_bh245_w37_2_d1;
            heap_bh245_w38_2_d1 <=  heap_bh245_w38_2;
            heap_bh245_w38_2_d2 <=  heap_bh245_w38_2_d1;
            heap_bh245_w39_2_d1 <=  heap_bh245_w39_2;
            heap_bh245_w39_2_d2 <=  heap_bh245_w39_2_d1;
            heap_bh245_w40_2_d1 <=  heap_bh245_w40_2;
            heap_bh245_w40_2_d2 <=  heap_bh245_w40_2_d1;
            heap_bh245_w41_2_d1 <=  heap_bh245_w41_2;
            heap_bh245_w41_2_d2 <=  heap_bh245_w41_2_d1;
            heap_bh245_w42_2_d1 <=  heap_bh245_w42_2;
            heap_bh245_w42_2_d2 <=  heap_bh245_w42_2_d1;
            heap_bh245_w43_2_d1 <=  heap_bh245_w43_2;
            heap_bh245_w43_2_d2 <=  heap_bh245_w43_2_d1;
            heap_bh245_w45_1_d1 <=  heap_bh245_w45_1;
            heap_bh245_w45_1_d2 <=  heap_bh245_w45_1_d1;
            heap_bh245_w46_1_d1 <=  heap_bh245_w46_1;
            heap_bh245_w46_1_d2 <=  heap_bh245_w46_1_d1;
            heap_bh245_w47_1_d1 <=  heap_bh245_w47_1;
            heap_bh245_w47_1_d2 <=  heap_bh245_w47_1_d1;
            heap_bh245_w48_1_d1 <=  heap_bh245_w48_1;
            heap_bh245_w48_1_d2 <=  heap_bh245_w48_1_d1;
            heap_bh245_w49_1_d1 <=  heap_bh245_w49_1;
            heap_bh245_w49_1_d2 <=  heap_bh245_w49_1_d1;
            heap_bh245_w50_1_d1 <=  heap_bh245_w50_1;
            heap_bh245_w50_1_d2 <=  heap_bh245_w50_1_d1;
            heap_bh245_w50_1_d3 <=  heap_bh245_w50_1_d2;
            heap_bh245_w51_1_d1 <=  heap_bh245_w51_1;
            heap_bh245_w51_1_d2 <=  heap_bh245_w51_1_d1;
            heap_bh245_w51_1_d3 <=  heap_bh245_w51_1_d2;
            heap_bh245_w52_1_d1 <=  heap_bh245_w52_1;
            heap_bh245_w52_1_d2 <=  heap_bh245_w52_1_d1;
            heap_bh245_w52_1_d3 <=  heap_bh245_w52_1_d2;
            heap_bh245_w53_1_d1 <=  heap_bh245_w53_1;
            heap_bh245_w53_1_d2 <=  heap_bh245_w53_1_d1;
            heap_bh245_w53_1_d3 <=  heap_bh245_w53_1_d2;
            inAdder0_bh245_0_d1 <=  inAdder0_bh245_0;
            inAdder1_bh245_0_d1 <=  inAdder1_bh245_0;
            cin_bh245_0_d1 <=  cin_bh245_0;
            heap_bh245_w5_5_d1 <=  heap_bh245_w5_5;
            heap_bh245_w6_4_d1 <=  heap_bh245_w6_4;
            heap_bh245_w9_4_d1 <=  heap_bh245_w9_4;
            heap_bh245_w10_5_d1 <=  heap_bh245_w10_5;
            heap_bh245_w11_4_d1 <=  heap_bh245_w11_4;
            heap_bh245_w12_4_d1 <=  heap_bh245_w12_4;
            heap_bh245_w7_5_d1 <=  heap_bh245_w7_5;
            heap_bh245_w8_6_d1 <=  heap_bh245_w8_6;
            heap_bh245_w9_5_d1 <=  heap_bh245_w9_5;
            inAdder0_bh245_1_d1 <=  inAdder0_bh245_1;
            inAdder1_bh245_1_d1 <=  inAdder1_bh245_1;
            cin_bh245_1_d1 <=  cin_bh245_1;
            heap_bh245_w0_5_d1 <=  heap_bh245_w0_5;
            heap_bh245_w0_5_d2 <=  heap_bh245_w0_5_d1;
            heap_bh245_w0_5_d3 <=  heap_bh245_w0_5_d2;
            heap_bh245_w1_5_d1 <=  heap_bh245_w1_5;
            heap_bh245_w1_5_d2 <=  heap_bh245_w1_5_d1;
            heap_bh245_w1_5_d3 <=  heap_bh245_w1_5_d2;
            heap_bh245_w2_5_d1 <=  heap_bh245_w2_5;
            heap_bh245_w2_5_d2 <=  heap_bh245_w2_5_d1;
            heap_bh245_w2_5_d3 <=  heap_bh245_w2_5_d2;
            heap_bh245_w3_5_d1 <=  heap_bh245_w3_5;
            heap_bh245_w4_4_d1 <=  heap_bh245_w4_4;
            heap_bh245_w9_6_d1 <=  heap_bh245_w9_6;
            heap_bh245_w10_6_d1 <=  heap_bh245_w10_6;
            heap_bh245_w19_3_d1 <=  heap_bh245_w19_3;
            heap_bh245_w5_7_d1 <=  heap_bh245_w5_7;
            heap_bh245_w6_5_d1 <=  heap_bh245_w6_5;
            heap_bh245_w11_6_d1 <=  heap_bh245_w11_6;
            heap_bh245_w12_6_d1 <=  heap_bh245_w12_6;
            heap_bh245_w15_5_d1 <=  heap_bh245_w15_5;
            heap_bh245_w16_6_d1 <=  heap_bh245_w16_6;
            heap_bh245_w20_4_d1 <=  heap_bh245_w20_4;
            heap_bh245_w21_3_d1 <=  heap_bh245_w21_3;
            heap_bh245_w7_7_d1 <=  heap_bh245_w7_7;
            heap_bh245_w8_7_d1 <=  heap_bh245_w8_7;
            heap_bh245_w9_7_d1 <=  heap_bh245_w9_7;
            heap_bh245_w17_6_d1 <=  heap_bh245_w17_6;
            heap_bh245_w18_6_d1 <=  heap_bh245_w18_6;
            heap_bh245_w19_4_d1 <=  heap_bh245_w19_4;
            heap_bh245_w22_4_d1 <=  heap_bh245_w22_4;
            heap_bh245_w23_3_d1 <=  heap_bh245_w23_3;
            heap_bh245_w13_6_d1 <=  heap_bh245_w13_6;
            heap_bh245_w14_6_d1 <=  heap_bh245_w14_6;
            heap_bh245_w15_6_d1 <=  heap_bh245_w15_6;
            heap_bh245_w24_4_d1 <=  heap_bh245_w24_4;
            heap_bh245_w25_3_d1 <=  heap_bh245_w25_3;
            heap_bh245_w26_3_d1 <=  heap_bh245_w26_3;
            heap_bh245_w3_6_d1 <=  heap_bh245_w3_6;
            heap_bh245_w3_6_d2 <=  heap_bh245_w3_6_d1;
            heap_bh245_w3_6_d3 <=  heap_bh245_w3_6_d2;
            heap_bh245_w4_5_d1 <=  heap_bh245_w4_5;
            heap_bh245_w4_5_d2 <=  heap_bh245_w4_5_d1;
            heap_bh245_w4_5_d3 <=  heap_bh245_w4_5_d2;
            heap_bh245_w9_8_d1 <=  heap_bh245_w9_8;
            heap_bh245_w9_8_d2 <=  heap_bh245_w9_8_d1;
            heap_bh245_w9_8_d3 <=  heap_bh245_w9_8_d2;
            heap_bh245_w10_7_d1 <=  heap_bh245_w10_7;
            heap_bh245_w10_7_d2 <=  heap_bh245_w10_7_d1;
            heap_bh245_w10_7_d3 <=  heap_bh245_w10_7_d2;
            heap_bh245_w15_7_d1 <=  heap_bh245_w15_7;
            heap_bh245_w15_7_d2 <=  heap_bh245_w15_7_d1;
            heap_bh245_w15_7_d3 <=  heap_bh245_w15_7_d2;
            heap_bh245_w16_7_d1 <=  heap_bh245_w16_7;
            heap_bh245_w16_7_d2 <=  heap_bh245_w16_7_d1;
            heap_bh245_w16_7_d3 <=  heap_bh245_w16_7_d2;
            heap_bh245_w19_5_d1 <=  heap_bh245_w19_5;
            heap_bh245_w19_5_d2 <=  heap_bh245_w19_5_d1;
            heap_bh245_w19_5_d3 <=  heap_bh245_w19_5_d2;
            heap_bh245_w20_5_d1 <=  heap_bh245_w20_5;
            heap_bh245_w20_5_d2 <=  heap_bh245_w20_5_d1;
            heap_bh245_w20_5_d3 <=  heap_bh245_w20_5_d2;
            heap_bh245_w33_3_d1 <=  heap_bh245_w33_3;
            heap_bh245_w34_3_d1 <=  heap_bh245_w34_3;
            heap_bh245_w34_4_d1 <=  heap_bh245_w34_4;
            heap_bh245_w35_3_d1 <=  heap_bh245_w35_3;
            heap_bh245_w36_3_d1 <=  heap_bh245_w36_3;
            heap_bh245_w36_4_d1 <=  heap_bh245_w36_4;
            heap_bh245_w37_3_d1 <=  heap_bh245_w37_3;
            heap_bh245_w38_3_d1 <=  heap_bh245_w38_3;
            heap_bh245_w38_4_d1 <=  heap_bh245_w38_4;
            heap_bh245_w39_3_d1 <=  heap_bh245_w39_3;
            heap_bh245_w40_3_d1 <=  heap_bh245_w40_3;
            heap_bh245_w40_4_d1 <=  heap_bh245_w40_4;
            heap_bh245_w41_3_d1 <=  heap_bh245_w41_3;
            heap_bh245_w41_3_d2 <=  heap_bh245_w41_3_d1;
            heap_bh245_w42_3_d1 <=  heap_bh245_w42_3;
            heap_bh245_w42_3_d2 <=  heap_bh245_w42_3_d1;
            heap_bh245_w42_4_d1 <=  heap_bh245_w42_4;
            heap_bh245_w42_4_d2 <=  heap_bh245_w42_4_d1;
            heap_bh245_w43_3_d1 <=  heap_bh245_w43_3;
            heap_bh245_w43_3_d2 <=  heap_bh245_w43_3_d1;
            heap_bh245_w5_9_d1 <=  heap_bh245_w5_9;
            heap_bh245_w5_9_d2 <=  heap_bh245_w5_9_d1;
            heap_bh245_w5_9_d3 <=  heap_bh245_w5_9_d2;
            heap_bh245_w6_6_d1 <=  heap_bh245_w6_6;
            heap_bh245_w6_6_d2 <=  heap_bh245_w6_6_d1;
            heap_bh245_w6_6_d3 <=  heap_bh245_w6_6_d2;
            heap_bh245_w11_8_d1 <=  heap_bh245_w11_8;
            heap_bh245_w11_8_d2 <=  heap_bh245_w11_8_d1;
            heap_bh245_w11_8_d3 <=  heap_bh245_w11_8_d2;
            heap_bh245_w12_7_d1 <=  heap_bh245_w12_7;
            heap_bh245_w12_7_d2 <=  heap_bh245_w12_7_d1;
            heap_bh245_w12_7_d3 <=  heap_bh245_w12_7_d2;
            heap_bh245_w17_8_d1 <=  heap_bh245_w17_8;
            heap_bh245_w17_8_d2 <=  heap_bh245_w17_8_d1;
            heap_bh245_w17_8_d3 <=  heap_bh245_w17_8_d2;
            heap_bh245_w18_7_d1 <=  heap_bh245_w18_7;
            heap_bh245_w18_7_d2 <=  heap_bh245_w18_7_d1;
            heap_bh245_w18_7_d3 <=  heap_bh245_w18_7_d2;
            heap_bh245_w19_6_d1 <=  heap_bh245_w19_6;
            heap_bh245_w19_6_d2 <=  heap_bh245_w19_6_d1;
            heap_bh245_w19_6_d3 <=  heap_bh245_w19_6_d2;
            heap_bh245_w21_5_d1 <=  heap_bh245_w21_5;
            heap_bh245_w21_5_d2 <=  heap_bh245_w21_5_d1;
            heap_bh245_w21_5_d3 <=  heap_bh245_w21_5_d2;
            heap_bh245_w22_5_d1 <=  heap_bh245_w22_5;
            heap_bh245_w22_5_d2 <=  heap_bh245_w22_5_d1;
            heap_bh245_w22_5_d3 <=  heap_bh245_w22_5_d2;
            heap_bh245_w27_4_d1 <=  heap_bh245_w27_4;
            heap_bh245_w27_4_d2 <=  heap_bh245_w27_4_d1;
            heap_bh245_w27_4_d3 <=  heap_bh245_w27_4_d2;
            heap_bh245_w28_5_d1 <=  heap_bh245_w28_5;
            heap_bh245_w28_5_d2 <=  heap_bh245_w28_5_d1;
            heap_bh245_w28_5_d3 <=  heap_bh245_w28_5_d2;
            heap_bh245_w44_3_d1 <=  heap_bh245_w44_3;
            heap_bh245_w44_3_d2 <=  heap_bh245_w44_3_d1;
            heap_bh245_w45_2_d1 <=  heap_bh245_w45_2;
            heap_bh245_w45_2_d2 <=  heap_bh245_w45_2_d1;
            heap_bh245_w45_2_d3 <=  heap_bh245_w45_2_d2;
            heap_bh245_w7_9_d1 <=  heap_bh245_w7_9;
            heap_bh245_w7_9_d2 <=  heap_bh245_w7_9_d1;
            heap_bh245_w7_9_d3 <=  heap_bh245_w7_9_d2;
            heap_bh245_w8_8_d1 <=  heap_bh245_w8_8;
            heap_bh245_w8_8_d2 <=  heap_bh245_w8_8_d1;
            heap_bh245_w8_8_d3 <=  heap_bh245_w8_8_d2;
            heap_bh245_w9_9_d1 <=  heap_bh245_w9_9;
            heap_bh245_w9_9_d2 <=  heap_bh245_w9_9_d1;
            heap_bh245_w9_9_d3 <=  heap_bh245_w9_9_d2;
            heap_bh245_w13_8_d1 <=  heap_bh245_w13_8;
            heap_bh245_w13_8_d2 <=  heap_bh245_w13_8_d1;
            heap_bh245_w13_8_d3 <=  heap_bh245_w13_8_d2;
            heap_bh245_w14_7_d1 <=  heap_bh245_w14_7;
            heap_bh245_w14_7_d2 <=  heap_bh245_w14_7_d1;
            heap_bh245_w14_7_d3 <=  heap_bh245_w14_7_d2;
            heap_bh245_w15_8_d1 <=  heap_bh245_w15_8;
            heap_bh245_w15_8_d2 <=  heap_bh245_w15_8_d1;
            heap_bh245_w15_8_d3 <=  heap_bh245_w15_8_d2;
            heap_bh245_w23_5_d1 <=  heap_bh245_w23_5;
            heap_bh245_w23_5_d2 <=  heap_bh245_w23_5_d1;
            heap_bh245_w23_5_d3 <=  heap_bh245_w23_5_d2;
            heap_bh245_w24_5_d1 <=  heap_bh245_w24_5;
            heap_bh245_w24_5_d2 <=  heap_bh245_w24_5_d1;
            heap_bh245_w24_5_d3 <=  heap_bh245_w24_5_d2;
            heap_bh245_w29_5_d1 <=  heap_bh245_w29_5;
            heap_bh245_w29_5_d2 <=  heap_bh245_w29_5_d1;
            heap_bh245_w29_5_d3 <=  heap_bh245_w29_5_d2;
            heap_bh245_w30_5_d1 <=  heap_bh245_w30_5;
            heap_bh245_w30_5_d2 <=  heap_bh245_w30_5_d1;
            heap_bh245_w30_5_d3 <=  heap_bh245_w30_5_d2;
            heap_bh245_w46_3_d1 <=  heap_bh245_w46_3;
            heap_bh245_w46_3_d2 <=  heap_bh245_w46_3_d1;
            heap_bh245_w46_3_d3 <=  heap_bh245_w46_3_d2;
            heap_bh245_w47_2_d1 <=  heap_bh245_w47_2;
            heap_bh245_w47_2_d2 <=  heap_bh245_w47_2_d1;
            heap_bh245_w47_2_d3 <=  heap_bh245_w47_2_d2;
            heap_bh245_w31_5_d1 <=  heap_bh245_w31_5;
            heap_bh245_w31_5_d2 <=  heap_bh245_w31_5_d1;
            heap_bh245_w31_5_d3 <=  heap_bh245_w31_5_d2;
            heap_bh245_w32_5_d1 <=  heap_bh245_w32_5;
            heap_bh245_w32_5_d2 <=  heap_bh245_w32_5_d1;
            heap_bh245_w32_5_d3 <=  heap_bh245_w32_5_d2;
            heap_bh245_w33_4_d1 <=  heap_bh245_w33_4;
            heap_bh245_w48_3_d1 <=  heap_bh245_w48_3;
            heap_bh245_w48_3_d2 <=  heap_bh245_w48_3_d1;
            heap_bh245_w48_3_d3 <=  heap_bh245_w48_3_d2;
            heap_bh245_w49_2_d1 <=  heap_bh245_w49_2;
            heap_bh245_w49_2_d2 <=  heap_bh245_w49_2_d1;
            heap_bh245_w49_2_d3 <=  heap_bh245_w49_2_d2;
            heap_bh245_w50_2_d1 <=  heap_bh245_w50_2;
            heap_bh245_w25_5_d1 <=  heap_bh245_w25_5;
            heap_bh245_w25_5_d2 <=  heap_bh245_w25_5_d1;
            heap_bh245_w25_5_d3 <=  heap_bh245_w25_5_d2;
            heap_bh245_w26_5_d1 <=  heap_bh245_w26_5;
            heap_bh245_w26_5_d2 <=  heap_bh245_w26_5_d1;
            heap_bh245_w26_5_d3 <=  heap_bh245_w26_5_d2;
            heap_bh245_w27_5_d1 <=  heap_bh245_w27_5;
            heap_bh245_w27_5_d2 <=  heap_bh245_w27_5_d1;
            heap_bh245_w27_5_d3 <=  heap_bh245_w27_5_d2;
            heap_bh245_w33_5_d1 <=  heap_bh245_w33_5;
            heap_bh245_w33_5_d2 <=  heap_bh245_w33_5_d1;
            heap_bh245_w34_5_d1 <=  heap_bh245_w34_5;
            heap_bh245_w34_5_d2 <=  heap_bh245_w34_5_d1;
            heap_bh245_w50_3_d1 <=  heap_bh245_w50_3;
            heap_bh245_w50_3_d2 <=  heap_bh245_w50_3_d1;
            heap_bh245_w51_2_d1 <=  heap_bh245_w51_2;
            heap_bh245_w51_2_d2 <=  heap_bh245_w51_2_d1;
            heap_bh245_w35_5_d1 <=  heap_bh245_w35_5;
            heap_bh245_w35_5_d2 <=  heap_bh245_w35_5_d1;
            heap_bh245_w36_5_d1 <=  heap_bh245_w36_5;
            heap_bh245_w36_5_d2 <=  heap_bh245_w36_5_d1;
            heap_bh245_w52_3_d1 <=  heap_bh245_w52_3;
            heap_bh245_w52_3_d2 <=  heap_bh245_w52_3_d1;
            heap_bh245_w53_2_d1 <=  heap_bh245_w53_2;
            heap_bh245_w53_2_d2 <=  heap_bh245_w53_2_d1;
            heap_bh245_w37_5_d1 <=  heap_bh245_w37_5;
            heap_bh245_w37_5_d2 <=  heap_bh245_w37_5_d1;
            heap_bh245_w38_5_d1 <=  heap_bh245_w38_5;
            heap_bh245_w38_5_d2 <=  heap_bh245_w38_5_d1;
            heap_bh245_w39_5_d1 <=  heap_bh245_w39_5;
            heap_bh245_w39_5_d2 <=  heap_bh245_w39_5_d1;
            heap_bh245_w40_5_d1 <=  heap_bh245_w40_5;
            heap_bh245_w40_5_d2 <=  heap_bh245_w40_5_d1;
            heap_bh245_w41_4_d1 <=  heap_bh245_w41_4;
            heap_bh245_w41_5_d1 <=  heap_bh245_w41_5;
            heap_bh245_w42_5_d1 <=  heap_bh245_w42_5;
            heap_bh245_w43_5_d1 <=  heap_bh245_w43_5;
            heap_bh245_w44_4_d1 <=  heap_bh245_w44_4;
            heap_bh245_w45_3_d1 <=  heap_bh245_w45_3;
         end if;
      end process;
   XX_m247 <= X ;
   YY_m247 <= Y ;
   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=11 lsbY=18 msbX=12 msbY=19
   Xp_m247b249 <= XX_m247(11 downto 11) & "00";
   Yp_m247b249 <= YY_m247(18 downto 18) & "00";
   x_m247b249_0 <= Xp_m247b249(2 downto 0);
   y_m247b249_0 <= Yp_m247b249(2 downto 0);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X0_249_m247 <= y_m247b249_0 & x_m247b249_0;
   PP_m247_249X0Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid251  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X0_249_m247,
                 Y => PP249X0Y0_m247);
   -- Adding the relevant bits to the heap of bits
   heap_bh245_w0_0 <= PP249X0Y0_m247(5); -- cycle= 0 cp= 5.3072e-10

   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=28 lsbY=0 msbX=37 msbY=2
   Xp_m247b260 <= XX_m247(36 downto 28) & "";
   Yp_m247b260 <= YY_m247(1 downto 0) & "0";
   x_m247b260_0 <= Xp_m247b260(2 downto 0);
   x_m247b260_1 <= Xp_m247b260(5 downto 3);
   x_m247b260_2 <= Xp_m247b260(8 downto 6);
   y_m247b260_0 <= Yp_m247b260(2 downto 0);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X0_260_m247 <= y_m247b260_0 & x_m247b260_0;
   PP_m247_260X0Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid262  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X0_260_m247,
                 Y => PP260X0Y0_m247);
   -- Adding the relevant bits to the heap of bits
   heap_bh245_w0_1 <= PP260X0Y0_m247(3); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w1_0 <= PP260X0Y0_m247(4); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w2_0 <= PP260X0Y0_m247(5); -- cycle= 0 cp= 5.4816e-10

   Y0X1_260_m247 <= y_m247b260_0 & x_m247b260_1;
   PP_m247_260X1Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid262  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X1_260_m247,
                 Y => PP260X1Y0_m247);
   -- Adding the relevant bits to the heap of bits
   heap_bh245_w1_1 <= PP260X1Y0_m247(1); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w2_1 <= PP260X1Y0_m247(2); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w3_0 <= PP260X1Y0_m247(3); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w4_0 <= PP260X1Y0_m247(4); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w5_0 <= PP260X1Y0_m247(5); -- cycle= 0 cp= 5.4816e-10

   Y0X2_260_m247 <= y_m247b260_0 & x_m247b260_2;
   PP_m247_260X2Y0_Tbl: SmallMultTableP3x3r6XsYu_F400_uid264  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X2_260_m247,
                 Y => PP260X2Y0_m247);
   -- Adding the relevant bits to the heap of bits
   heap_bh245_w4_1 <= PP260X2Y0_m247(1); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w5_1 <= PP260X2Y0_m247(2); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w6_0 <= PP260X2Y0_m247(3); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w7_0 <= PP260X2Y0_m247(4); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w8_0 <= not PP260X2Y0_m247(5); -- cycle= 0 cp= 5.4816e-10

   heap_bh245_w6_1 <= A(0); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w7_1 <= A(1); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w8_1 <= A(2); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w9_0 <= A(3); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w10_0 <= A(4); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w11_0 <= A(5); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w12_0 <= A(6); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w13_0 <= A(7); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w14_0 <= A(8); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w15_0 <= A(9); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w16_0 <= A(10); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w17_0 <= A(11); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w18_0 <= A(12); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w19_0 <= A(13); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w20_0 <= A(14); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w21_0 <= A(15); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w22_0 <= A(16); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w23_0 <= A(17); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w24_0 <= A(18); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w25_0 <= A(19); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w26_0 <= A(20); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w27_0 <= A(21); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w28_0 <= A(22); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w29_0 <= A(23); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w30_0 <= A(24); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w31_0 <= A(25); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w32_0 <= A(26); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w33_0 <= A(27); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w34_0 <= A(28); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w35_0 <= A(29); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w36_0 <= A(30); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w37_0 <= A(31); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w38_0 <= A(32); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w39_0 <= A(33); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w40_0 <= A(34); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w41_0 <= A(35); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w42_0 <= A(36); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w43_0 <= A(37); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w44_0 <= A(38); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w45_0 <= A(39); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w46_0 <= A(40); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w47_0 <= A(41); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w48_0 <= A(42); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w49_0 <= A(43); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w50_0 <= A(44); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w51_0 <= A(45); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w52_0 <= A(46); -- cycle= 0 cp= 5.4816e-10
   heap_bh245_w53_0 <= A(47); -- cycle= 0 cp= 5.4816e-10
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------
   DSP_bh245_ch1_0 <= std_logic_vector(signed("0" & XX_m247(11 downto 0) & "000000000000") * signed("" & YY_m247(36 downto 19) & ""));
   heap_bh245_w19_1 <= not( DSP_bh245_ch1_0(42) ); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w18_1 <= DSP_bh245_ch1_0(41); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w17_1 <= DSP_bh245_ch1_0(40); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w16_1 <= DSP_bh245_ch1_0(39); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w15_1 <= DSP_bh245_ch1_0(38); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w14_1 <= DSP_bh245_ch1_0(37); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w13_1 <= DSP_bh245_ch1_0(36); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w12_1 <= DSP_bh245_ch1_0(35); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w11_1 <= DSP_bh245_ch1_0(34); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w10_1 <= DSP_bh245_ch1_0(33); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w9_1 <= DSP_bh245_ch1_0(32); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w8_2 <= DSP_bh245_ch1_0(31); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w7_2 <= DSP_bh245_ch1_0(30); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w6_2 <= DSP_bh245_ch1_0(29); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w5_2 <= DSP_bh245_ch1_0(28); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w4_2 <= DSP_bh245_ch1_0(27); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w3_1 <= DSP_bh245_ch1_0(26); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w2_2 <= DSP_bh245_ch1_0(25); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w1_2 <= DSP_bh245_ch1_0(24); -- cycle= 0 cp= 2.387e-09
   heap_bh245_w0_2 <= DSP_bh245_ch1_0(23); -- cycle= 0 cp= 2.387e-09
   ----------------Synchro barrier, entering cycle 0----------------
   DSP_bh245_ch2_0 <= std_logic_vector(signed("" & XX_m247(36 downto 12) & "") * signed("0" & YY_m247(18 downto 2) & ""));
   DSP_bh245_root2_1 <= std_logic_vector(signed("" & XX_m247(36 downto 12) & "") * signed("" & YY_m247(36 downto 19) & ""));
   ----------------Synchro barrier, entering cycle 1----------------
   DSP_bh245_ch2_1<= std_logic_vector(signed(DSP_bh245_root2_1_d1) +  signed( DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) & DSP_bh245_ch2_0_d1(42) &   DSP_bh245_ch2_0_d1(42 downto 17) ));
   heap_bh245_w0_3 <= DSP_bh245_ch2_0_d1(16); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w44_1 <= not( DSP_bh245_ch2_1(43) ); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w43_1 <= DSP_bh245_ch2_1(42); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w42_1 <= DSP_bh245_ch2_1(41); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w41_1 <= DSP_bh245_ch2_1(40); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w40_1 <= DSP_bh245_ch2_1(39); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w39_1 <= DSP_bh245_ch2_1(38); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w38_1 <= DSP_bh245_ch2_1(37); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w37_1 <= DSP_bh245_ch2_1(36); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w36_1 <= DSP_bh245_ch2_1(35); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w35_1 <= DSP_bh245_ch2_1(34); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w34_1 <= DSP_bh245_ch2_1(33); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w33_1 <= DSP_bh245_ch2_1(32); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w32_1 <= DSP_bh245_ch2_1(31); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w31_1 <= DSP_bh245_ch2_1(30); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w30_1 <= DSP_bh245_ch2_1(29); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w29_1 <= DSP_bh245_ch2_1(28); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w28_1 <= DSP_bh245_ch2_1(27); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w27_1 <= DSP_bh245_ch2_1(26); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w26_1 <= DSP_bh245_ch2_1(25); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w25_1 <= DSP_bh245_ch2_1(24); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w24_1 <= DSP_bh245_ch2_1(23); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w23_1 <= DSP_bh245_ch2_1(22); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w22_1 <= DSP_bh245_ch2_1(21); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w21_1 <= DSP_bh245_ch2_1(20); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w20_1 <= DSP_bh245_ch2_1(19); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w19_2 <= DSP_bh245_ch2_1(18); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w18_2 <= DSP_bh245_ch2_1(17); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w17_2 <= DSP_bh245_ch2_1(16); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w16_2 <= DSP_bh245_ch2_1(15); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w15_2 <= DSP_bh245_ch2_1(14); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w14_2 <= DSP_bh245_ch2_1(13); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w13_2 <= DSP_bh245_ch2_1(12); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w12_2 <= DSP_bh245_ch2_1(11); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w11_2 <= DSP_bh245_ch2_1(10); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w10_2 <= DSP_bh245_ch2_1(9); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w9_2 <= DSP_bh245_ch2_1(8); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w8_3 <= DSP_bh245_ch2_1(7); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w7_3 <= DSP_bh245_ch2_1(6); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w6_3 <= DSP_bh245_ch2_1(5); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w5_3 <= DSP_bh245_ch2_1(4); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w4_3 <= DSP_bh245_ch2_1(3); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w3_2 <= DSP_bh245_ch2_1(2); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w2_3 <= DSP_bh245_ch2_1(1); -- cycle= 1 cp= 2.274e-09
   heap_bh245_w1_3 <= DSP_bh245_ch2_1(0); -- cycle= 1 cp= 2.274e-09
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh245_w5_4 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w8_4 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w9_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w10_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w11_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w12_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w13_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w14_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w15_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w16_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w17_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w18_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w20_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w21_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w22_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w23_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w24_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w25_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w26_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w27_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w28_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w29_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w30_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w31_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w32_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w33_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w34_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w35_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w36_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w37_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w38_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w39_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w40_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w41_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w42_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w43_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w45_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w46_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w47_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w48_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w49_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w50_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w51_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w52_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh245_w53_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 0----------------
   inAdder0_bh245_0 <= '0' & heap_bh245_w2_1 & heap_bh245_w1_1 & heap_bh245_w0_0;
   inAdder1_bh245_0 <= '0' & heap_bh245_w2_0 & heap_bh245_w1_0 & heap_bh245_w0_1;
   cin_bh245_0 <= "0";
   ----------------Synchro barrier, entering cycle 1----------------
   outAdder_bh245_0 <= std_logic_vector(signed(inAdder0_bh245_0_d1) + signed(inAdder1_bh245_0_d1) + signed(cin_bh245_0_d1));
   heap_bh245_w0_4 <= outAdder_bh245_0(0); -- cycle= 1 cp= 0
   heap_bh245_w1_4 <= outAdder_bh245_0(1); -- cycle= 1 cp= 0
   heap_bh245_w2_4 <= outAdder_bh245_0(2); -- cycle= 1 cp= 0
   heap_bh245_w3_3 <= outAdder_bh245_0(3); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh245_0_0 <= heap_bh245_w5_4 & heap_bh245_w5_1 & heap_bh245_w5_0;
   CompressorIn_bh245_0_1 <= heap_bh245_w6_1 & heap_bh245_w6_0;
      Compressor_bh245_0: Compressor_23_3
      port map ( R => CompressorOut_bh245_0_0,
                 X0 => CompressorIn_bh245_0_0,
                 X1 => CompressorIn_bh245_0_1);
   heap_bh245_w5_5 <= CompressorOut_bh245_0_0(0); -- cycle= 0 cp= 1.07888e-09
   heap_bh245_w6_4 <= CompressorOut_bh245_0_0(1); -- cycle= 0 cp= 1.07888e-09
   heap_bh245_w7_4 <= CompressorOut_bh245_0_0(2); -- cycle= 0 cp= 1.07888e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh245_1_2 <= heap_bh245_w8_4 & heap_bh245_w8_1 & heap_bh245_w8_0;
   CompressorIn_bh245_1_3 <= heap_bh245_w9_3 & heap_bh245_w9_0;
      Compressor_bh245_1: Compressor_23_3
      port map ( R => CompressorOut_bh245_1_1,
                 X0 => CompressorIn_bh245_1_2,
                 X1 => CompressorIn_bh245_1_3);
   heap_bh245_w8_5 <= CompressorOut_bh245_1_1(0); -- cycle= 0 cp= 1.07888e-09
   heap_bh245_w9_4 <= CompressorOut_bh245_1_1(1); -- cycle= 0 cp= 1.07888e-09
   heap_bh245_w10_4 <= CompressorOut_bh245_1_1(2); -- cycle= 0 cp= 1.07888e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh245_2_4 <= heap_bh245_w10_3 & heap_bh245_w10_0 & heap_bh245_w10_4;
   CompressorIn_bh245_2_5 <= heap_bh245_w11_3 & heap_bh245_w11_0;
      Compressor_bh245_2: Compressor_23_3
      port map ( R => CompressorOut_bh245_2_2,
                 X0 => CompressorIn_bh245_2_4,
                 X1 => CompressorIn_bh245_2_5);
   heap_bh245_w10_5 <= CompressorOut_bh245_2_2(0); -- cycle= 0 cp= 1.6096e-09
   heap_bh245_w11_4 <= CompressorOut_bh245_2_2(1); -- cycle= 0 cp= 1.6096e-09
   heap_bh245_w12_4 <= CompressorOut_bh245_2_2(2); -- cycle= 0 cp= 1.6096e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh245_3_6 <= heap_bh245_w7_1 & heap_bh245_w7_0 & heap_bh245_w7_4;
   CompressorIn_bh245_3_7(0) <= heap_bh245_w8_5;
      Compressor_bh245_3: Compressor_13_3
      port map ( R => CompressorOut_bh245_3_3,
                 X0 => CompressorIn_bh245_3_6,
                 X1 => CompressorIn_bh245_3_7);
   heap_bh245_w7_5 <= CompressorOut_bh245_3_3(0); -- cycle= 0 cp= 1.6096e-09
   heap_bh245_w8_6 <= CompressorOut_bh245_3_3(1); -- cycle= 0 cp= 1.6096e-09
   heap_bh245_w9_5 <= CompressorOut_bh245_3_3(2); -- cycle= 0 cp= 1.6096e-09

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_4_8 <= heap_bh245_w12_3_d1 & heap_bh245_w12_0_d1 & heap_bh245_w12_4_d1;
   CompressorIn_bh245_4_9 <= heap_bh245_w13_3_d1 & heap_bh245_w13_0_d1;
      Compressor_bh245_4: Compressor_23_3
      port map ( R => CompressorOut_bh245_4_4,
                 X0 => CompressorIn_bh245_4_8,
                 X1 => CompressorIn_bh245_4_9);
   heap_bh245_w12_5 <= CompressorOut_bh245_4_4(0); -- cycle= 1 cp= 0
   heap_bh245_w13_4 <= CompressorOut_bh245_4_4(1); -- cycle= 1 cp= 0
   heap_bh245_w14_4 <= CompressorOut_bh245_4_4(2); -- cycle= 1 cp= 0
   ----------------Synchro barrier, entering cycle 1----------------
   inAdder0_bh245_1 <= '0' & heap_bh245_w2_2_d1 & heap_bh245_w1_2_d1 & heap_bh245_w0_2_d1;
   inAdder1_bh245_1 <= '0' & heap_bh245_w2_4 & heap_bh245_w1_4 & heap_bh245_w0_4;
   cin_bh245_1 <= "0";
   ----------------Synchro barrier, entering cycle 2----------------
   outAdder_bh245_1 <= std_logic_vector(signed(inAdder0_bh245_1_d1) + signed(inAdder1_bh245_1_d1) + signed(cin_bh245_1_d1));
   heap_bh245_w0_5 <= outAdder_bh245_1(0); -- cycle= 2 cp= 0
   heap_bh245_w1_5 <= outAdder_bh245_1(1); -- cycle= 2 cp= 0
   heap_bh245_w2_5 <= outAdder_bh245_1(2); -- cycle= 2 cp= 0
   heap_bh245_w3_4 <= outAdder_bh245_1(3); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_5_10 <= heap_bh245_w14_3_d1 & heap_bh245_w14_0_d1 & heap_bh245_w14_1_d1 & heap_bh245_w14_4;
   CompressorIn_bh245_5_11(0) <= heap_bh245_w15_3_d1;
      Compressor_bh245_5: Compressor_14_3
      port map ( R => CompressorOut_bh245_5_5,
                 X0 => CompressorIn_bh245_5_10,
                 X1 => CompressorIn_bh245_5_11);
   heap_bh245_w14_5 <= CompressorOut_bh245_5_5(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w15_4 <= CompressorOut_bh245_5_5(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w16_4 <= CompressorOut_bh245_5_5(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_6_12 <= heap_bh245_w3_0_d1 & heap_bh245_w3_1_d1 & heap_bh245_w3_3;
   CompressorIn_bh245_6_13 <= heap_bh245_w4_1_d1 & heap_bh245_w4_0_d1;
      Compressor_bh245_6: Compressor_23_3
      port map ( R => CompressorOut_bh245_6_6,
                 X0 => CompressorIn_bh245_6_12,
                 X1 => CompressorIn_bh245_6_13);
   heap_bh245_w3_5 <= CompressorOut_bh245_6_6(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w4_4 <= CompressorOut_bh245_6_6(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w5_6 <= CompressorOut_bh245_6_6(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_7_14 <= heap_bh245_w9_4_d1 & heap_bh245_w9_5_d1 & heap_bh245_w9_1_d1;
   CompressorIn_bh245_7_15 <= heap_bh245_w10_5_d1 & heap_bh245_w10_1_d1;
      Compressor_bh245_7: Compressor_23_3
      port map ( R => CompressorOut_bh245_7_7,
                 X0 => CompressorIn_bh245_7_14,
                 X1 => CompressorIn_bh245_7_15);
   heap_bh245_w9_6 <= CompressorOut_bh245_7_7(0); -- cycle= 1 cp= 0
   heap_bh245_w10_6 <= CompressorOut_bh245_7_7(1); -- cycle= 1 cp= 0
   heap_bh245_w11_5 <= CompressorOut_bh245_7_7(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_8_16 <= heap_bh245_w16_3_d1 & heap_bh245_w16_0_d1 & heap_bh245_w16_1_d1;
   CompressorIn_bh245_8_17 <= heap_bh245_w17_3_d1 & heap_bh245_w17_0_d1;
      Compressor_bh245_8: Compressor_23_3
      port map ( R => CompressorOut_bh245_8_8,
                 X0 => CompressorIn_bh245_8_16,
                 X1 => CompressorIn_bh245_8_17);
   heap_bh245_w16_5 <= CompressorOut_bh245_8_8(0); -- cycle= 1 cp= 0
   heap_bh245_w17_4 <= CompressorOut_bh245_8_8(1); -- cycle= 1 cp= 0
   heap_bh245_w18_4 <= CompressorOut_bh245_8_8(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_9_18 <= heap_bh245_w18_3_d1 & heap_bh245_w18_0_d1 & heap_bh245_w18_1_d1;
   CompressorIn_bh245_9_19 <= heap_bh245_w19_0_d1 & heap_bh245_w19_1_d1;
      Compressor_bh245_9: Compressor_23_3
      port map ( R => CompressorOut_bh245_9_9,
                 X0 => CompressorIn_bh245_9_18,
                 X1 => CompressorIn_bh245_9_19);
   heap_bh245_w18_5 <= CompressorOut_bh245_9_9(0); -- cycle= 1 cp= 0
   heap_bh245_w19_3 <= CompressorOut_bh245_9_9(1); -- cycle= 1 cp= 0
   heap_bh245_w20_3 <= CompressorOut_bh245_9_9(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_10_20 <= heap_bh245_w5_5_d1 & heap_bh245_w5_2_d1 & heap_bh245_w5_6;
   CompressorIn_bh245_10_21 <= heap_bh245_w6_4_d1 & heap_bh245_w6_2_d1;
      Compressor_bh245_10: Compressor_23_3
      port map ( R => CompressorOut_bh245_10_10,
                 X0 => CompressorIn_bh245_10_20,
                 X1 => CompressorIn_bh245_10_21);
   heap_bh245_w5_7 <= CompressorOut_bh245_10_10(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w6_5 <= CompressorOut_bh245_10_10(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w7_6 <= CompressorOut_bh245_10_10(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_11_22 <= heap_bh245_w11_4_d1 & heap_bh245_w11_1_d1 & heap_bh245_w11_5;
   CompressorIn_bh245_11_23 <= heap_bh245_w12_1_d1 & heap_bh245_w12_5;
      Compressor_bh245_11: Compressor_23_3
      port map ( R => CompressorOut_bh245_11_11,
                 X0 => CompressorIn_bh245_11_22,
                 X1 => CompressorIn_bh245_11_23);
   heap_bh245_w11_6 <= CompressorOut_bh245_11_11(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w12_6 <= CompressorOut_bh245_11_11(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w13_5 <= CompressorOut_bh245_11_11(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_12_24 <= heap_bh245_w15_0_d1 & heap_bh245_w15_1_d1 & heap_bh245_w15_4;
   CompressorIn_bh245_12_25 <= heap_bh245_w16_5 & heap_bh245_w16_4;
      Compressor_bh245_12: Compressor_23_3
      port map ( R => CompressorOut_bh245_12_12,
                 X0 => CompressorIn_bh245_12_24,
                 X1 => CompressorIn_bh245_12_25);
   heap_bh245_w15_5 <= CompressorOut_bh245_12_12(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w16_6 <= CompressorOut_bh245_12_12(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w17_5 <= CompressorOut_bh245_12_12(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_13_26 <= heap_bh245_w20_2_d1 & heap_bh245_w20_0_d1 & heap_bh245_w20_3;
   CompressorIn_bh245_13_27 <= heap_bh245_w21_2_d1 & heap_bh245_w21_0_d1;
      Compressor_bh245_13: Compressor_23_3
      port map ( R => CompressorOut_bh245_13_13,
                 X0 => CompressorIn_bh245_13_26,
                 X1 => CompressorIn_bh245_13_27);
   heap_bh245_w20_4 <= CompressorOut_bh245_13_13(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w21_3 <= CompressorOut_bh245_13_13(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh245_w22_3 <= CompressorOut_bh245_13_13(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_14_28 <= heap_bh245_w7_5_d1 & heap_bh245_w7_2_d1 & heap_bh245_w7_6;
   CompressorIn_bh245_14_29 <= heap_bh245_w8_6_d1 & heap_bh245_w8_2_d1;
      Compressor_bh245_14: Compressor_23_3
      port map ( R => CompressorOut_bh245_14_14,
                 X0 => CompressorIn_bh245_14_28,
                 X1 => CompressorIn_bh245_14_29);
   heap_bh245_w7_7 <= CompressorOut_bh245_14_14(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w8_7 <= CompressorOut_bh245_14_14(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w9_7 <= CompressorOut_bh245_14_14(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_15_30 <= heap_bh245_w17_1_d1 & heap_bh245_w17_4 & heap_bh245_w17_5;
   CompressorIn_bh245_15_31 <= heap_bh245_w18_5 & heap_bh245_w18_4;
      Compressor_bh245_15: Compressor_23_3
      port map ( R => CompressorOut_bh245_15_15,
                 X0 => CompressorIn_bh245_15_30,
                 X1 => CompressorIn_bh245_15_31);
   heap_bh245_w17_6 <= CompressorOut_bh245_15_15(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w18_6 <= CompressorOut_bh245_15_15(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w19_4 <= CompressorOut_bh245_15_15(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_16_32 <= heap_bh245_w22_2_d1 & heap_bh245_w22_0_d1 & heap_bh245_w22_3;
   CompressorIn_bh245_16_33 <= heap_bh245_w23_2_d1 & heap_bh245_w23_0_d1;
      Compressor_bh245_16: Compressor_23_3
      port map ( R => CompressorOut_bh245_16_16,
                 X0 => CompressorIn_bh245_16_32,
                 X1 => CompressorIn_bh245_16_33);
   heap_bh245_w22_4 <= CompressorOut_bh245_16_16(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w23_3 <= CompressorOut_bh245_16_16(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w24_3 <= CompressorOut_bh245_16_16(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_17_34 <= heap_bh245_w13_1_d1 & heap_bh245_w13_4 & heap_bh245_w13_5;
   CompressorIn_bh245_17_35(0) <= heap_bh245_w14_5;
      Compressor_bh245_17: Compressor_13_3
      port map ( R => CompressorOut_bh245_17_17,
                 X0 => CompressorIn_bh245_17_34,
                 X1 => CompressorIn_bh245_17_35);
   heap_bh245_w13_6 <= CompressorOut_bh245_17_17(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w14_6 <= CompressorOut_bh245_17_17(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh245_w15_6 <= CompressorOut_bh245_17_17(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh245_18_36 <= heap_bh245_w24_2_d1 & heap_bh245_w24_0_d1 & heap_bh245_w24_3;
   CompressorIn_bh245_18_37 <= heap_bh245_w25_2_d1 & heap_bh245_w25_0_d1;
      Compressor_bh245_18: Compressor_23_3
      port map ( R => CompressorOut_bh245_18_18,
                 X0 => CompressorIn_bh245_18_36,
                 X1 => CompressorIn_bh245_18_37);
   heap_bh245_w24_4 <= CompressorOut_bh245_18_18(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w25_3 <= CompressorOut_bh245_18_18(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh245_w26_3 <= CompressorOut_bh245_18_18(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_19_38 <= heap_bh245_w26_2_d2 & heap_bh245_w26_0_d2 & heap_bh245_w26_3_d1 & heap_bh245_w26_1_d1;
   CompressorIn_bh245_19_39(0) <= heap_bh245_w27_2_d2;
      Compressor_bh245_19: Compressor_14_3
      port map ( R => CompressorOut_bh245_19_19,
                 X0 => CompressorIn_bh245_19_38,
                 X1 => CompressorIn_bh245_19_39);
   heap_bh245_w26_4 <= CompressorOut_bh245_19_19(0); -- cycle= 2 cp= 0
   heap_bh245_w27_3 <= CompressorOut_bh245_19_19(1); -- cycle= 2 cp= 0
   heap_bh245_w28_3 <= CompressorOut_bh245_19_19(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_20_40 <= heap_bh245_w3_5_d1 & heap_bh245_w3_2_d1 & heap_bh245_w3_4;
   CompressorIn_bh245_20_41 <= heap_bh245_w4_2_d2 & heap_bh245_w4_4_d1;
      Compressor_bh245_20: Compressor_23_3
      port map ( R => CompressorOut_bh245_20_20,
                 X0 => CompressorIn_bh245_20_40,
                 X1 => CompressorIn_bh245_20_41);
   heap_bh245_w3_6 <= CompressorOut_bh245_20_20(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w4_5 <= CompressorOut_bh245_20_20(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w5_8 <= CompressorOut_bh245_20_20(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_21_42 <= heap_bh245_w9_6_d1 & heap_bh245_w9_7_d1 & heap_bh245_w9_2_d1;
   CompressorIn_bh245_21_43 <= heap_bh245_w10_6_d1 & heap_bh245_w10_2_d1;
      Compressor_bh245_21: Compressor_23_3
      port map ( R => CompressorOut_bh245_21_21,
                 X0 => CompressorIn_bh245_21_42,
                 X1 => CompressorIn_bh245_21_43);
   heap_bh245_w9_8 <= CompressorOut_bh245_21_21(0); -- cycle= 2 cp= 0
   heap_bh245_w10_7 <= CompressorOut_bh245_21_21(1); -- cycle= 2 cp= 0
   heap_bh245_w11_7 <= CompressorOut_bh245_21_21(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_22_44 <= heap_bh245_w15_6_d1 & heap_bh245_w15_5_d1 & heap_bh245_w15_2_d1;
   CompressorIn_bh245_22_45 <= heap_bh245_w16_6_d1 & heap_bh245_w16_2_d1;
      Compressor_bh245_22: Compressor_23_3
      port map ( R => CompressorOut_bh245_22_22,
                 X0 => CompressorIn_bh245_22_44,
                 X1 => CompressorIn_bh245_22_45);
   heap_bh245_w15_7 <= CompressorOut_bh245_22_22(0); -- cycle= 2 cp= 0
   heap_bh245_w16_7 <= CompressorOut_bh245_22_22(1); -- cycle= 2 cp= 0
   heap_bh245_w17_7 <= CompressorOut_bh245_22_22(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_23_46 <= heap_bh245_w19_3_d1 & heap_bh245_w19_4_d1 & heap_bh245_w19_2_d1;
   CompressorIn_bh245_23_47 <= heap_bh245_w20_4_d1 & heap_bh245_w20_1_d1;
      Compressor_bh245_23: Compressor_23_3
      port map ( R => CompressorOut_bh245_23_23,
                 X0 => CompressorIn_bh245_23_46,
                 X1 => CompressorIn_bh245_23_47);
   heap_bh245_w19_5 <= CompressorOut_bh245_23_23(0); -- cycle= 2 cp= 0
   heap_bh245_w20_5 <= CompressorOut_bh245_23_23(1); -- cycle= 2 cp= 0
   heap_bh245_w21_4 <= CompressorOut_bh245_23_23(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_24_48 <= heap_bh245_w28_2_d2 & heap_bh245_w28_0_d2 & heap_bh245_w28_1_d1;
   CompressorIn_bh245_24_49 <= heap_bh245_w29_2_d2 & heap_bh245_w29_0_d2;
      Compressor_bh245_24: Compressor_23_3
      port map ( R => CompressorOut_bh245_24_24,
                 X0 => CompressorIn_bh245_24_48,
                 X1 => CompressorIn_bh245_24_49);
   heap_bh245_w28_4 <= CompressorOut_bh245_24_24(0); -- cycle= 2 cp= 0
   heap_bh245_w29_3 <= CompressorOut_bh245_24_24(1); -- cycle= 2 cp= 0
   heap_bh245_w30_3 <= CompressorOut_bh245_24_24(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_25_50 <= heap_bh245_w30_2_d2 & heap_bh245_w30_0_d2 & heap_bh245_w30_1_d1;
   CompressorIn_bh245_25_51 <= heap_bh245_w31_2_d2 & heap_bh245_w31_0_d2;
      Compressor_bh245_25: Compressor_23_3
      port map ( R => CompressorOut_bh245_25_25,
                 X0 => CompressorIn_bh245_25_50,
                 X1 => CompressorIn_bh245_25_51);
   heap_bh245_w30_4 <= CompressorOut_bh245_25_25(0); -- cycle= 2 cp= 0
   heap_bh245_w31_3 <= CompressorOut_bh245_25_25(1); -- cycle= 2 cp= 0
   heap_bh245_w32_3 <= CompressorOut_bh245_25_25(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_26_52 <= heap_bh245_w32_2_d2 & heap_bh245_w32_0_d2 & heap_bh245_w32_1_d1;
   CompressorIn_bh245_26_53 <= heap_bh245_w33_2_d2 & heap_bh245_w33_0_d2;
      Compressor_bh245_26: Compressor_23_3
      port map ( R => CompressorOut_bh245_26_26,
                 X0 => CompressorIn_bh245_26_52,
                 X1 => CompressorIn_bh245_26_53);
   heap_bh245_w32_4 <= CompressorOut_bh245_26_26(0); -- cycle= 2 cp= 0
   heap_bh245_w33_3 <= CompressorOut_bh245_26_26(1); -- cycle= 2 cp= 0
   heap_bh245_w34_3 <= CompressorOut_bh245_26_26(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_27_54 <= heap_bh245_w34_2_d2 & heap_bh245_w34_0_d2 & heap_bh245_w34_1_d1;
   CompressorIn_bh245_27_55 <= heap_bh245_w35_2_d2 & heap_bh245_w35_0_d2;
      Compressor_bh245_27: Compressor_23_3
      port map ( R => CompressorOut_bh245_27_27,
                 X0 => CompressorIn_bh245_27_54,
                 X1 => CompressorIn_bh245_27_55);
   heap_bh245_w34_4 <= CompressorOut_bh245_27_27(0); -- cycle= 2 cp= 0
   heap_bh245_w35_3 <= CompressorOut_bh245_27_27(1); -- cycle= 2 cp= 0
   heap_bh245_w36_3 <= CompressorOut_bh245_27_27(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_28_56 <= heap_bh245_w36_2_d2 & heap_bh245_w36_0_d2 & heap_bh245_w36_1_d1;
   CompressorIn_bh245_28_57 <= heap_bh245_w37_2_d2 & heap_bh245_w37_0_d2;
      Compressor_bh245_28: Compressor_23_3
      port map ( R => CompressorOut_bh245_28_28,
                 X0 => CompressorIn_bh245_28_56,
                 X1 => CompressorIn_bh245_28_57);
   heap_bh245_w36_4 <= CompressorOut_bh245_28_28(0); -- cycle= 2 cp= 0
   heap_bh245_w37_3 <= CompressorOut_bh245_28_28(1); -- cycle= 2 cp= 0
   heap_bh245_w38_3 <= CompressorOut_bh245_28_28(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_29_58 <= heap_bh245_w38_2_d2 & heap_bh245_w38_0_d2 & heap_bh245_w38_1_d1;
   CompressorIn_bh245_29_59 <= heap_bh245_w39_2_d2 & heap_bh245_w39_0_d2;
      Compressor_bh245_29: Compressor_23_3
      port map ( R => CompressorOut_bh245_29_29,
                 X0 => CompressorIn_bh245_29_58,
                 X1 => CompressorIn_bh245_29_59);
   heap_bh245_w38_4 <= CompressorOut_bh245_29_29(0); -- cycle= 2 cp= 0
   heap_bh245_w39_3 <= CompressorOut_bh245_29_29(1); -- cycle= 2 cp= 0
   heap_bh245_w40_3 <= CompressorOut_bh245_29_29(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_30_60 <= heap_bh245_w40_2_d2 & heap_bh245_w40_0_d2 & heap_bh245_w40_1_d1;
   CompressorIn_bh245_30_61 <= heap_bh245_w41_2_d2 & heap_bh245_w41_0_d2;
      Compressor_bh245_30: Compressor_23_3
      port map ( R => CompressorOut_bh245_30_30,
                 X0 => CompressorIn_bh245_30_60,
                 X1 => CompressorIn_bh245_30_61);
   heap_bh245_w40_4 <= CompressorOut_bh245_30_30(0); -- cycle= 2 cp= 0
   heap_bh245_w41_3 <= CompressorOut_bh245_30_30(1); -- cycle= 2 cp= 0
   heap_bh245_w42_3 <= CompressorOut_bh245_30_30(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_31_62 <= heap_bh245_w42_2_d2 & heap_bh245_w42_0_d2 & heap_bh245_w42_1_d1;
   CompressorIn_bh245_31_63 <= heap_bh245_w43_2_d2 & heap_bh245_w43_0_d2;
      Compressor_bh245_31: Compressor_23_3
      port map ( R => CompressorOut_bh245_31_31,
                 X0 => CompressorIn_bh245_31_62,
                 X1 => CompressorIn_bh245_31_63);
   heap_bh245_w42_4 <= CompressorOut_bh245_31_31(0); -- cycle= 2 cp= 0
   heap_bh245_w43_3 <= CompressorOut_bh245_31_31(1); -- cycle= 2 cp= 0
   heap_bh245_w44_2 <= CompressorOut_bh245_31_31(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_32_64 <= heap_bh245_w5_7_d1 & heap_bh245_w5_3_d1 & heap_bh245_w5_8;
   CompressorIn_bh245_32_65 <= heap_bh245_w6_5_d1 & heap_bh245_w6_3_d1;
      Compressor_bh245_32: Compressor_23_3
      port map ( R => CompressorOut_bh245_32_32,
                 X0 => CompressorIn_bh245_32_64,
                 X1 => CompressorIn_bh245_32_65);
   heap_bh245_w5_9 <= CompressorOut_bh245_32_32(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w6_6 <= CompressorOut_bh245_32_32(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w7_8 <= CompressorOut_bh245_32_32(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_33_66 <= heap_bh245_w11_6_d1 & heap_bh245_w11_2_d1 & heap_bh245_w11_7;
   CompressorIn_bh245_33_67 <= heap_bh245_w12_6_d1 & heap_bh245_w12_2_d1;
      Compressor_bh245_33: Compressor_23_3
      port map ( R => CompressorOut_bh245_33_33,
                 X0 => CompressorIn_bh245_33_66,
                 X1 => CompressorIn_bh245_33_67);
   heap_bh245_w11_8 <= CompressorOut_bh245_33_33(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w12_7 <= CompressorOut_bh245_33_33(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w13_7 <= CompressorOut_bh245_33_33(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_34_68 <= heap_bh245_w17_6_d1 & heap_bh245_w17_2_d1 & heap_bh245_w17_7;
   CompressorIn_bh245_34_69 <= heap_bh245_w18_6_d1 & heap_bh245_w18_2_d1;
      Compressor_bh245_34: Compressor_23_3
      port map ( R => CompressorOut_bh245_34_34,
                 X0 => CompressorIn_bh245_34_68,
                 X1 => CompressorIn_bh245_34_69);
   heap_bh245_w17_8 <= CompressorOut_bh245_34_34(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w18_7 <= CompressorOut_bh245_34_34(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w19_6 <= CompressorOut_bh245_34_34(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_35_70 <= heap_bh245_w21_3_d1 & heap_bh245_w21_1_d1 & heap_bh245_w21_4;
   CompressorIn_bh245_35_71 <= heap_bh245_w22_4_d1 & heap_bh245_w22_1_d1;
      Compressor_bh245_35: Compressor_23_3
      port map ( R => CompressorOut_bh245_35_35,
                 X0 => CompressorIn_bh245_35_70,
                 X1 => CompressorIn_bh245_35_71);
   heap_bh245_w21_5 <= CompressorOut_bh245_35_35(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w22_5 <= CompressorOut_bh245_35_35(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w23_4 <= CompressorOut_bh245_35_35(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_36_72 <= heap_bh245_w27_0_d2 & heap_bh245_w27_1_d1 & heap_bh245_w27_3;
   CompressorIn_bh245_36_73 <= heap_bh245_w28_4 & heap_bh245_w28_3;
      Compressor_bh245_36: Compressor_23_3
      port map ( R => CompressorOut_bh245_36_36,
                 X0 => CompressorIn_bh245_36_72,
                 X1 => CompressorIn_bh245_36_73);
   heap_bh245_w27_4 <= CompressorOut_bh245_36_36(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w28_5 <= CompressorOut_bh245_36_36(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w29_4 <= CompressorOut_bh245_36_36(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_37_74 <= heap_bh245_w44_0_d2 & heap_bh245_w44_1_d1 & heap_bh245_w44_2;
   CompressorIn_bh245_37_75 <= heap_bh245_w45_1_d2 & heap_bh245_w45_0_d2;
      Compressor_bh245_37: Compressor_23_3
      port map ( R => CompressorOut_bh245_37_37,
                 X0 => CompressorIn_bh245_37_74,
                 X1 => CompressorIn_bh245_37_75);
   heap_bh245_w44_3 <= CompressorOut_bh245_37_37(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w45_2 <= CompressorOut_bh245_37_37(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh245_w46_2 <= CompressorOut_bh245_37_37(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_38_76 <= heap_bh245_w7_7_d1 & heap_bh245_w7_3_d1 & heap_bh245_w7_8;
   CompressorIn_bh245_38_77 <= heap_bh245_w8_7_d1 & heap_bh245_w8_3_d1;
      Compressor_bh245_38: Compressor_23_3
      port map ( R => CompressorOut_bh245_38_38,
                 X0 => CompressorIn_bh245_38_76,
                 X1 => CompressorIn_bh245_38_77);
   heap_bh245_w7_9 <= CompressorOut_bh245_38_38(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w8_8 <= CompressorOut_bh245_38_38(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w9_9 <= CompressorOut_bh245_38_38(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_39_78 <= heap_bh245_w13_6_d1 & heap_bh245_w13_2_d1 & heap_bh245_w13_7;
   CompressorIn_bh245_39_79 <= heap_bh245_w14_6_d1 & heap_bh245_w14_2_d1;
      Compressor_bh245_39: Compressor_23_3
      port map ( R => CompressorOut_bh245_39_39,
                 X0 => CompressorIn_bh245_39_78,
                 X1 => CompressorIn_bh245_39_79);
   heap_bh245_w13_8 <= CompressorOut_bh245_39_39(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w14_7 <= CompressorOut_bh245_39_39(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w15_8 <= CompressorOut_bh245_39_39(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_40_80 <= heap_bh245_w23_3_d1 & heap_bh245_w23_1_d1 & heap_bh245_w23_4;
   CompressorIn_bh245_40_81 <= heap_bh245_w24_4_d1 & heap_bh245_w24_1_d1;
      Compressor_bh245_40: Compressor_23_3
      port map ( R => CompressorOut_bh245_40_40,
                 X0 => CompressorIn_bh245_40_80,
                 X1 => CompressorIn_bh245_40_81);
   heap_bh245_w23_5 <= CompressorOut_bh245_40_40(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w24_5 <= CompressorOut_bh245_40_40(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w25_4 <= CompressorOut_bh245_40_40(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_41_82 <= heap_bh245_w29_1_d1 & heap_bh245_w29_3 & heap_bh245_w29_4;
   CompressorIn_bh245_41_83 <= heap_bh245_w30_4 & heap_bh245_w30_3;
      Compressor_bh245_41: Compressor_23_3
      port map ( R => CompressorOut_bh245_41_41,
                 X0 => CompressorIn_bh245_41_82,
                 X1 => CompressorIn_bh245_41_83);
   heap_bh245_w29_5 <= CompressorOut_bh245_41_41(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w30_5 <= CompressorOut_bh245_41_41(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w31_4 <= CompressorOut_bh245_41_41(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_42_84 <= heap_bh245_w46_1_d2 & heap_bh245_w46_0_d2 & heap_bh245_w46_2;
   CompressorIn_bh245_42_85 <= heap_bh245_w47_1_d2 & heap_bh245_w47_0_d2;
      Compressor_bh245_42: Compressor_23_3
      port map ( R => CompressorOut_bh245_42_42,
                 X0 => CompressorIn_bh245_42_84,
                 X1 => CompressorIn_bh245_42_85);
   heap_bh245_w46_3 <= CompressorOut_bh245_42_42(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w47_2 <= CompressorOut_bh245_42_42(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh245_w48_2 <= CompressorOut_bh245_42_42(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_43_86 <= heap_bh245_w31_1_d1 & heap_bh245_w31_3 & heap_bh245_w31_4;
   CompressorIn_bh245_43_87 <= heap_bh245_w32_4 & heap_bh245_w32_3;
      Compressor_bh245_43: Compressor_23_3
      port map ( R => CompressorOut_bh245_43_43,
                 X0 => CompressorIn_bh245_43_86,
                 X1 => CompressorIn_bh245_43_87);
   heap_bh245_w31_5 <= CompressorOut_bh245_43_43(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w32_5 <= CompressorOut_bh245_43_43(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w33_4 <= CompressorOut_bh245_43_43(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_44_88 <= heap_bh245_w48_1_d2 & heap_bh245_w48_0_d2 & heap_bh245_w48_2;
   CompressorIn_bh245_44_89 <= heap_bh245_w49_1_d2 & heap_bh245_w49_0_d2;
      Compressor_bh245_44: Compressor_23_3
      port map ( R => CompressorOut_bh245_44_44,
                 X0 => CompressorIn_bh245_44_88,
                 X1 => CompressorIn_bh245_44_89);
   heap_bh245_w48_3 <= CompressorOut_bh245_44_44(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w49_2 <= CompressorOut_bh245_44_44(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w50_2 <= CompressorOut_bh245_44_44(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh245_45_90 <= heap_bh245_w25_3_d1 & heap_bh245_w25_1_d1 & heap_bh245_w25_4;
   CompressorIn_bh245_45_91(0) <= heap_bh245_w26_4;
      Compressor_bh245_45: Compressor_13_3
      port map ( R => CompressorOut_bh245_45_45,
                 X0 => CompressorIn_bh245_45_90,
                 X1 => CompressorIn_bh245_45_91);
   heap_bh245_w25_5 <= CompressorOut_bh245_45_45(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w26_5 <= CompressorOut_bh245_45_45(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh245_w27_5 <= CompressorOut_bh245_45_45(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_46_92 <= heap_bh245_w33_1_d2 & heap_bh245_w33_3_d1 & heap_bh245_w33_4_d1;
   CompressorIn_bh245_46_93 <= heap_bh245_w34_4_d1 & heap_bh245_w34_3_d1;
      Compressor_bh245_46: Compressor_23_3
      port map ( R => CompressorOut_bh245_46_46,
                 X0 => CompressorIn_bh245_46_92,
                 X1 => CompressorIn_bh245_46_93);
   heap_bh245_w33_5 <= CompressorOut_bh245_46_46(0); -- cycle= 3 cp= 0
   heap_bh245_w34_5 <= CompressorOut_bh245_46_46(1); -- cycle= 3 cp= 0
   heap_bh245_w35_4 <= CompressorOut_bh245_46_46(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_47_94 <= heap_bh245_w50_1_d3 & heap_bh245_w50_0_d3 & heap_bh245_w50_2_d1;
   CompressorIn_bh245_47_95 <= heap_bh245_w51_1_d3 & heap_bh245_w51_0_d3;
      Compressor_bh245_47: Compressor_23_3
      port map ( R => CompressorOut_bh245_47_47,
                 X0 => CompressorIn_bh245_47_94,
                 X1 => CompressorIn_bh245_47_95);
   heap_bh245_w50_3 <= CompressorOut_bh245_47_47(0); -- cycle= 3 cp= 0
   heap_bh245_w51_2 <= CompressorOut_bh245_47_47(1); -- cycle= 3 cp= 0
   heap_bh245_w52_2 <= CompressorOut_bh245_47_47(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_48_96 <= heap_bh245_w35_1_d2 & heap_bh245_w35_3_d1 & heap_bh245_w35_4;
   CompressorIn_bh245_48_97 <= heap_bh245_w36_4_d1 & heap_bh245_w36_3_d1;
      Compressor_bh245_48: Compressor_23_3
      port map ( R => CompressorOut_bh245_48_48,
                 X0 => CompressorIn_bh245_48_96,
                 X1 => CompressorIn_bh245_48_97);
   heap_bh245_w35_5 <= CompressorOut_bh245_48_48(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh245_w36_5 <= CompressorOut_bh245_48_48(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh245_w37_4 <= CompressorOut_bh245_48_48(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_49_98 <= heap_bh245_w52_1_d3 & heap_bh245_w52_0_d3 & heap_bh245_w52_2;
   CompressorIn_bh245_49_99 <= heap_bh245_w53_1_d3 & heap_bh245_w53_0_d3;
      Compressor_bh245_49: Compressor_23_3
      port map ( R => CompressorOut_bh245_49_49,
                 X0 => CompressorIn_bh245_49_98,
                 X1 => CompressorIn_bh245_49_99);
   heap_bh245_w52_3 <= CompressorOut_bh245_49_49(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh245_w53_2 <= CompressorOut_bh245_49_49(1); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_50_100 <= heap_bh245_w37_1_d2 & heap_bh245_w37_3_d1 & heap_bh245_w37_4;
   CompressorIn_bh245_50_101 <= heap_bh245_w38_4_d1 & heap_bh245_w38_3_d1;
      Compressor_bh245_50: Compressor_23_3
      port map ( R => CompressorOut_bh245_50_50,
                 X0 => CompressorIn_bh245_50_100,
                 X1 => CompressorIn_bh245_50_101);
   heap_bh245_w37_5 <= CompressorOut_bh245_50_50(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh245_w38_5 <= CompressorOut_bh245_50_50(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh245_w39_4 <= CompressorOut_bh245_50_50(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh245_51_102 <= heap_bh245_w39_1_d2 & heap_bh245_w39_3_d1 & heap_bh245_w39_4;
   CompressorIn_bh245_51_103 <= heap_bh245_w40_4_d1 & heap_bh245_w40_3_d1;
      Compressor_bh245_51: Compressor_23_3
      port map ( R => CompressorOut_bh245_51_51,
                 X0 => CompressorIn_bh245_51_102,
                 X1 => CompressorIn_bh245_51_103);
   heap_bh245_w39_5 <= CompressorOut_bh245_51_51(0); -- cycle= 3 cp= 1.59216e-09
   heap_bh245_w40_5 <= CompressorOut_bh245_51_51(1); -- cycle= 3 cp= 1.59216e-09
   heap_bh245_w41_4 <= CompressorOut_bh245_51_51(2); -- cycle= 3 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh245_52_104 <= heap_bh245_w41_1_d3 & heap_bh245_w41_3_d2 & heap_bh245_w41_4_d1;
   CompressorIn_bh245_52_105 <= heap_bh245_w42_4_d2 & heap_bh245_w42_3_d2;
      Compressor_bh245_52: Compressor_23_3
      port map ( R => CompressorOut_bh245_52_52,
                 X0 => CompressorIn_bh245_52_104,
                 X1 => CompressorIn_bh245_52_105);
   heap_bh245_w41_5 <= CompressorOut_bh245_52_52(0); -- cycle= 4 cp= 0
   heap_bh245_w42_5 <= CompressorOut_bh245_52_52(1); -- cycle= 4 cp= 0
   heap_bh245_w43_4 <= CompressorOut_bh245_52_52(2); -- cycle= 4 cp= 0

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh245_53_106 <= heap_bh245_w43_1_d3 & heap_bh245_w43_3_d2 & heap_bh245_w43_4;
   CompressorIn_bh245_53_107(0) <= heap_bh245_w44_3_d2;
      Compressor_bh245_53: Compressor_13_3
      port map ( R => CompressorOut_bh245_53_53,
                 X0 => CompressorIn_bh245_53_106,
                 X1 => CompressorIn_bh245_53_107);
   heap_bh245_w43_5 <= CompressorOut_bh245_53_53(0); -- cycle= 4 cp= 5.3072e-10
   heap_bh245_w44_4 <= CompressorOut_bh245_53_53(1); -- cycle= 4 cp= 5.3072e-10
   heap_bh245_w45_3 <= CompressorOut_bh245_53_53(2); -- cycle= 4 cp= 5.3072e-10
   ----------------Synchro barrier, entering cycle 4----------------
   ----------------Synchro barrier, entering cycle 5----------------
   finalAdderIn0_bh245 <= "0" & heap_bh245_w53_2_d2 & heap_bh245_w52_3_d2 & heap_bh245_w51_2_d2 & heap_bh245_w50_3_d2 & heap_bh245_w49_2_d3 & heap_bh245_w48_3_d3 & heap_bh245_w47_2_d3 & heap_bh245_w46_3_d3 & heap_bh245_w45_2_d3 & heap_bh245_w44_4_d1 & heap_bh245_w43_5_d1 & heap_bh245_w42_5_d1 & heap_bh245_w41_5_d1 & heap_bh245_w40_5_d2 & heap_bh245_w39_5_d2 & heap_bh245_w38_5_d2 & heap_bh245_w37_5_d2 & heap_bh245_w36_5_d2 & heap_bh245_w35_5_d2 & heap_bh245_w34_5_d2 & heap_bh245_w33_5_d2 & heap_bh245_w32_5_d3 & heap_bh245_w31_5_d3 & heap_bh245_w30_5_d3 & heap_bh245_w29_5_d3 & heap_bh245_w28_5_d3 & heap_bh245_w27_4_d3 & heap_bh245_w26_5_d3 & heap_bh245_w25_5_d3 & heap_bh245_w24_5_d3 & heap_bh245_w23_5_d3 & heap_bh245_w22_5_d3 & heap_bh245_w21_5_d3 & heap_bh245_w20_5_d3 & heap_bh245_w19_5_d3 & heap_bh245_w18_7_d3 & heap_bh245_w17_8_d3 & heap_bh245_w16_7_d3 & heap_bh245_w15_7_d3 & heap_bh245_w14_7_d3 & heap_bh245_w13_8_d3 & heap_bh245_w12_7_d3 & heap_bh245_w11_8_d3 & heap_bh245_w10_7_d3 & heap_bh245_w9_8_d3 & heap_bh245_w8_8_d3 & heap_bh245_w7_9_d3 & heap_bh245_w6_6_d3 & heap_bh245_w5_9_d3 & heap_bh245_w4_3_d4 & heap_bh245_w3_6_d3 & heap_bh245_w2_3_d4 & heap_bh245_w1_3_d4 & heap_bh245_w0_3_d4;
   finalAdderIn1_bh245 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh245_w45_3_d1 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh245_w27_5_d3 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh245_w19_6_d3 & '0' & '0' & '0' & heap_bh245_w15_8_d3 & '0' & '0' & '0' & '0' & '0' & heap_bh245_w9_9_d3 & '0' & '0' & '0' & '0' & heap_bh245_w4_5_d3 & '0' & heap_bh245_w2_5_d3 & heap_bh245_w1_5_d3 & heap_bh245_w0_5_d3;
   finalAdderCin_bh245 <= '0';
      Adder_final245_2: IntAdder_55_f400_uid391  -- pipelineDepth=1 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh245,
                 R => finalAdderOut_bh245,
                 X => finalAdderIn0_bh245,
                 Y => finalAdderIn1_bh245);
   ----------------Synchro barrier, entering cycle 6----------------
   -- concatenate all the compressed chunks
   CompressionResult245 <= finalAdderOut_bh245;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult245(53 downto 6);
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_67_f400_uid625
--                    (IntAdderAlternative_67_F400_uid629)
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2010)
--------------------------------------------------------------------------------
-- Pipeline depth: 1 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_67_f400_uid625 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(66 downto 0);
          Y : in  std_logic_vector(66 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(66 downto 0)   );
end entity;

architecture arch of IntAdder_67_f400_uid625 is
signal s_sum_l0_idx0 :  std_logic_vector(42 downto 0);
signal s_sum_l0_idx1, s_sum_l0_idx1_d1 :  std_logic_vector(25 downto 0);
signal sum_l0_idx0, sum_l0_idx0_d1 :  std_logic_vector(41 downto 0);
signal c_l0_idx0, c_l0_idx0_d1 :  std_logic_vector(0 downto 0);
signal sum_l0_idx1 :  std_logic_vector(24 downto 0);
signal c_l0_idx1 :  std_logic_vector(0 downto 0);
signal s_sum_l1_idx1 :  std_logic_vector(25 downto 0);
signal sum_l1_idx1 :  std_logic_vector(24 downto 0);
signal c_l1_idx1 :  std_logic_vector(0 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            s_sum_l0_idx1_d1 <=  s_sum_l0_idx1;
            sum_l0_idx0_d1 <=  sum_l0_idx0;
            c_l0_idx0_d1 <=  c_l0_idx0;
         end if;
      end process;
   --Alternative
   s_sum_l0_idx0 <= ( "0" & X(41 downto 0)) + ( "0" & Y(41 downto 0)) + Cin;
   s_sum_l0_idx1 <= ( "0" & X(66 downto 42)) + ( "0" & Y(66 downto 42));
   sum_l0_idx0 <= s_sum_l0_idx0(41 downto 0);
   c_l0_idx0 <= s_sum_l0_idx0(42 downto 42);
   sum_l0_idx1 <= s_sum_l0_idx1(24 downto 0);
   c_l0_idx1 <= s_sum_l0_idx1(25 downto 25);
   ----------------Synchro barrier, entering cycle 1----------------
   s_sum_l1_idx1 <=  s_sum_l0_idx1_d1 + c_l0_idx0_d1(0 downto 0);
   sum_l1_idx1 <= s_sum_l1_idx1(24 downto 0);
   c_l1_idx1 <= s_sum_l1_idx1(25 downto 25);
   R <= sum_l1_idx1(24 downto 0) & sum_l0_idx0_d1(41 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                  FixMultAdd_44x48p59r59signed_F400_uid401
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan, 2012-2014
--------------------------------------------------------------------------------
-- Pipeline depth: 7 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixMultAdd_44x48p59r59signed_F400_uid401 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(43 downto 0);
          Y : in  std_logic_vector(47 downto 0);
          A : in  std_logic_vector(58 downto 0);
          R : out  std_logic_vector(58 downto 0)   );
end entity;

architecture arch of FixMultAdd_44x48p59r59signed_F400_uid401 is
   component IntAdder_67_f400_uid625 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(66 downto 0);
             Y : in  std_logic_vector(66 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(66 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid408 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid410 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid412 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid414 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component Compressor_6_3 is
      port ( X0 : in  std_logic_vector(5 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_14_3 is
      port ( X0 : in  std_logic_vector(3 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_4_3 is
      port ( X0 : in  std_logic_vector(3 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_23_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(1 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_13_3 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             X1 : in  std_logic_vector(0 downto 0);
             R : out  std_logic_vector(2 downto 0)   );
   end component;

   component Compressor_3_2 is
      port ( X0 : in  std_logic_vector(2 downto 0);
             R : out  std_logic_vector(1 downto 0)   );
   end component;

signal XX_m404, XX_m404_d1 :  std_logic_vector(47 downto 0);
signal YY_m404, YY_m404_d1 :  std_logic_vector(43 downto 0);
signal Xp_m404b406 :  std_logic_vector(14 downto 0);
signal Yp_m404b406 :  std_logic_vector(14 downto 0);
signal x_m404b406_0 :  std_logic_vector(2 downto 0);
signal x_m404b406_1 :  std_logic_vector(2 downto 0);
signal x_m404b406_2 :  std_logic_vector(2 downto 0);
signal x_m404b406_3 :  std_logic_vector(2 downto 0);
signal x_m404b406_4 :  std_logic_vector(2 downto 0);
signal y_m404b406_0 :  std_logic_vector(2 downto 0);
signal y_m404b406_1 :  std_logic_vector(2 downto 0);
signal y_m404b406_2 :  std_logic_vector(2 downto 0);
signal y_m404b406_3 :  std_logic_vector(2 downto 0);
signal y_m404b406_4 :  std_logic_vector(2 downto 0);
signal Y0X4_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X4Y0_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_0 :  std_logic;
signal Y1X3_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X3Y1_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_1 :  std_logic;
signal Y1X4_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X4Y1_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_2 :  std_logic;
signal heap_bh402_w1_0 :  std_logic;
signal heap_bh402_w2_0 :  std_logic;
signal heap_bh402_w3_0 :  std_logic;
signal Y2X2_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X2Y2_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_3 :  std_logic;
signal Y2X3_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X3Y2_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_4 :  std_logic;
signal heap_bh402_w1_1 :  std_logic;
signal heap_bh402_w2_1 :  std_logic;
signal heap_bh402_w3_1 :  std_logic;
signal Y2X4_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X4Y2_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w1_2 :  std_logic;
signal heap_bh402_w2_2 :  std_logic;
signal heap_bh402_w3_2 :  std_logic;
signal heap_bh402_w4_0 :  std_logic;
signal heap_bh402_w5_0 :  std_logic;
signal heap_bh402_w6_0 :  std_logic;
signal Y3X1_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X1Y3_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_5 :  std_logic;
signal Y3X2_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X2Y3_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_6 :  std_logic;
signal heap_bh402_w1_3 :  std_logic;
signal heap_bh402_w2_3 :  std_logic;
signal heap_bh402_w3_3 :  std_logic;
signal Y3X3_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X3Y3_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w1_4 :  std_logic;
signal heap_bh402_w2_4 :  std_logic;
signal heap_bh402_w3_4 :  std_logic;
signal heap_bh402_w4_1 :  std_logic;
signal heap_bh402_w5_1 :  std_logic;
signal heap_bh402_w6_1 :  std_logic;
signal Y3X4_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X4Y3_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w4_2 :  std_logic;
signal heap_bh402_w5_2 :  std_logic;
signal heap_bh402_w6_2 :  std_logic;
signal heap_bh402_w7_0 :  std_logic;
signal heap_bh402_w8_0 :  std_logic;
signal heap_bh402_w9_0 :  std_logic;
signal Y4X0_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X0Y4_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_7 :  std_logic;
signal Y4X1_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X1Y4_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w0_8 :  std_logic;
signal heap_bh402_w1_5 :  std_logic;
signal heap_bh402_w2_5 :  std_logic;
signal heap_bh402_w3_5 :  std_logic;
signal Y4X2_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X2Y4_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w1_6 :  std_logic;
signal heap_bh402_w2_6 :  std_logic;
signal heap_bh402_w3_6 :  std_logic;
signal heap_bh402_w4_3 :  std_logic;
signal heap_bh402_w5_3 :  std_logic;
signal heap_bh402_w6_3 :  std_logic;
signal Y4X3_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X3Y4_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w4_4 :  std_logic;
signal heap_bh402_w5_4 :  std_logic;
signal heap_bh402_w6_4 :  std_logic;
signal heap_bh402_w7_1 :  std_logic;
signal heap_bh402_w8_1 :  std_logic;
signal heap_bh402_w9_1 :  std_logic;
signal Y4X4_406_m404 :  std_logic_vector(5 downto 0);
signal PP406X4Y4_m404 :  std_logic_vector(5 downto 0);
signal heap_bh402_w7_2 :  std_logic;
signal heap_bh402_w8_2 :  std_logic;
signal heap_bh402_w9_2 :  std_logic;
signal heap_bh402_w10_0, heap_bh402_w10_0_d1 :  std_logic;
signal heap_bh402_w11_0 :  std_logic;
signal heap_bh402_w12_0 :  std_logic;
signal heap_bh402_w7_3 :  std_logic;
signal heap_bh402_w8_3 :  std_logic;
signal heap_bh402_w9_3 :  std_logic;
signal heap_bh402_w10_1 :  std_logic;
signal heap_bh402_w11_1 :  std_logic;
signal heap_bh402_w12_1 :  std_logic;
signal heap_bh402_w13_0, heap_bh402_w13_0_d1 :  std_logic;
signal heap_bh402_w14_0, heap_bh402_w14_0_d1 :  std_logic;
signal heap_bh402_w15_0, heap_bh402_w15_0_d1 :  std_logic;
signal heap_bh402_w16_0, heap_bh402_w16_0_d1 :  std_logic;
signal heap_bh402_w17_0, heap_bh402_w17_0_d1 :  std_logic;
signal heap_bh402_w18_0, heap_bh402_w18_0_d1, heap_bh402_w18_0_d2 :  std_logic;
signal heap_bh402_w19_0, heap_bh402_w19_0_d1, heap_bh402_w19_0_d2 :  std_logic;
signal heap_bh402_w20_0, heap_bh402_w20_0_d1, heap_bh402_w20_0_d2 :  std_logic;
signal heap_bh402_w21_0, heap_bh402_w21_0_d1, heap_bh402_w21_0_d2 :  std_logic;
signal heap_bh402_w22_0, heap_bh402_w22_0_d1, heap_bh402_w22_0_d2 :  std_logic;
signal heap_bh402_w23_0, heap_bh402_w23_0_d1, heap_bh402_w23_0_d2 :  std_logic;
signal heap_bh402_w24_0, heap_bh402_w24_0_d1, heap_bh402_w24_0_d2 :  std_logic;
signal heap_bh402_w25_0, heap_bh402_w25_0_d1, heap_bh402_w25_0_d2 :  std_logic;
signal heap_bh402_w26_0, heap_bh402_w26_0_d1, heap_bh402_w26_0_d2, heap_bh402_w26_0_d3 :  std_logic;
signal heap_bh402_w27_0, heap_bh402_w27_0_d1, heap_bh402_w27_0_d2, heap_bh402_w27_0_d3 :  std_logic;
signal heap_bh402_w28_0, heap_bh402_w28_0_d1, heap_bh402_w28_0_d2, heap_bh402_w28_0_d3 :  std_logic;
signal heap_bh402_w29_0, heap_bh402_w29_0_d1, heap_bh402_w29_0_d2, heap_bh402_w29_0_d3 :  std_logic;
signal heap_bh402_w30_0, heap_bh402_w30_0_d1, heap_bh402_w30_0_d2, heap_bh402_w30_0_d3 :  std_logic;
signal heap_bh402_w31_0, heap_bh402_w31_0_d1 :  std_logic;
signal heap_bh402_w32_0, heap_bh402_w32_0_d1 :  std_logic;
signal heap_bh402_w33_0, heap_bh402_w33_0_d1 :  std_logic;
signal heap_bh402_w34_0, heap_bh402_w34_0_d1 :  std_logic;
signal heap_bh402_w35_0, heap_bh402_w35_0_d1 :  std_logic;
signal heap_bh402_w36_0, heap_bh402_w36_0_d1 :  std_logic;
signal heap_bh402_w37_0, heap_bh402_w37_0_d1 :  std_logic;
signal heap_bh402_w38_0, heap_bh402_w38_0_d1 :  std_logic;
signal heap_bh402_w39_0, heap_bh402_w39_0_d1, heap_bh402_w39_0_d2 :  std_logic;
signal heap_bh402_w40_0, heap_bh402_w40_0_d1, heap_bh402_w40_0_d2 :  std_logic;
signal heap_bh402_w41_0, heap_bh402_w41_0_d1, heap_bh402_w41_0_d2 :  std_logic;
signal heap_bh402_w42_0, heap_bh402_w42_0_d1, heap_bh402_w42_0_d2 :  std_logic;
signal heap_bh402_w43_0, heap_bh402_w43_0_d1, heap_bh402_w43_0_d2 :  std_logic;
signal heap_bh402_w44_0, heap_bh402_w44_0_d1, heap_bh402_w44_0_d2 :  std_logic;
signal heap_bh402_w45_0, heap_bh402_w45_0_d1, heap_bh402_w45_0_d2 :  std_logic;
signal heap_bh402_w46_0, heap_bh402_w46_0_d1, heap_bh402_w46_0_d2 :  std_logic;
signal heap_bh402_w47_0, heap_bh402_w47_0_d1, heap_bh402_w47_0_d2, heap_bh402_w47_0_d3 :  std_logic;
signal heap_bh402_w48_0, heap_bh402_w48_0_d1, heap_bh402_w48_0_d2, heap_bh402_w48_0_d3 :  std_logic;
signal heap_bh402_w49_0, heap_bh402_w49_0_d1, heap_bh402_w49_0_d2, heap_bh402_w49_0_d3 :  std_logic;
signal heap_bh402_w50_0, heap_bh402_w50_0_d1, heap_bh402_w50_0_d2, heap_bh402_w50_0_d3 :  std_logic;
signal heap_bh402_w51_0, heap_bh402_w51_0_d1, heap_bh402_w51_0_d2, heap_bh402_w51_0_d3 :  std_logic;
signal heap_bh402_w52_0, heap_bh402_w52_0_d1, heap_bh402_w52_0_d2, heap_bh402_w52_0_d3 :  std_logic;
signal heap_bh402_w53_0, heap_bh402_w53_0_d1, heap_bh402_w53_0_d2, heap_bh402_w53_0_d3 :  std_logic;
signal heap_bh402_w54_0, heap_bh402_w54_0_d1, heap_bh402_w54_0_d2, heap_bh402_w54_0_d3 :  std_logic;
signal heap_bh402_w55_0, heap_bh402_w55_0_d1, heap_bh402_w55_0_d2, heap_bh402_w55_0_d3 :  std_logic;
signal heap_bh402_w56_0, heap_bh402_w56_0_d1, heap_bh402_w56_0_d2, heap_bh402_w56_0_d3 :  std_logic;
signal heap_bh402_w57_0, heap_bh402_w57_0_d1, heap_bh402_w57_0_d2, heap_bh402_w57_0_d3 :  std_logic;
signal heap_bh402_w58_0, heap_bh402_w58_0_d1, heap_bh402_w58_0_d2, heap_bh402_w58_0_d3 :  std_logic;
signal heap_bh402_w59_0, heap_bh402_w59_0_d1, heap_bh402_w59_0_d2, heap_bh402_w59_0_d3 :  std_logic;
signal heap_bh402_w60_0, heap_bh402_w60_0_d1, heap_bh402_w60_0_d2, heap_bh402_w60_0_d3 :  std_logic;
signal heap_bh402_w61_0, heap_bh402_w61_0_d1, heap_bh402_w61_0_d2, heap_bh402_w61_0_d3 :  std_logic;
signal heap_bh402_w62_0, heap_bh402_w62_0_d1, heap_bh402_w62_0_d2, heap_bh402_w62_0_d3 :  std_logic;
signal heap_bh402_w63_0, heap_bh402_w63_0_d1, heap_bh402_w63_0_d2, heap_bh402_w63_0_d3, heap_bh402_w63_0_d4 :  std_logic;
signal heap_bh402_w64_0, heap_bh402_w64_0_d1, heap_bh402_w64_0_d2, heap_bh402_w64_0_d3, heap_bh402_w64_0_d4 :  std_logic;
signal heap_bh402_w65_0, heap_bh402_w65_0_d1, heap_bh402_w65_0_d2, heap_bh402_w65_0_d3, heap_bh402_w65_0_d4 :  std_logic;
signal DSP_bh402_ch1_0 :  std_logic_vector(42 downto 0);
signal heap_bh402_w31_1, heap_bh402_w31_1_d1 :  std_logic;
signal heap_bh402_w30_1, heap_bh402_w30_1_d1, heap_bh402_w30_1_d2, heap_bh402_w30_1_d3 :  std_logic;
signal heap_bh402_w29_1, heap_bh402_w29_1_d1, heap_bh402_w29_1_d2, heap_bh402_w29_1_d3 :  std_logic;
signal heap_bh402_w28_1, heap_bh402_w28_1_d1, heap_bh402_w28_1_d2, heap_bh402_w28_1_d3 :  std_logic;
signal heap_bh402_w27_1, heap_bh402_w27_1_d1, heap_bh402_w27_1_d2, heap_bh402_w27_1_d3 :  std_logic;
signal heap_bh402_w26_1, heap_bh402_w26_1_d1, heap_bh402_w26_1_d2, heap_bh402_w26_1_d3 :  std_logic;
signal heap_bh402_w25_1, heap_bh402_w25_1_d1, heap_bh402_w25_1_d2 :  std_logic;
signal heap_bh402_w24_1, heap_bh402_w24_1_d1, heap_bh402_w24_1_d2 :  std_logic;
signal heap_bh402_w23_1, heap_bh402_w23_1_d1, heap_bh402_w23_1_d2 :  std_logic;
signal heap_bh402_w22_1, heap_bh402_w22_1_d1, heap_bh402_w22_1_d2 :  std_logic;
signal heap_bh402_w21_1, heap_bh402_w21_1_d1, heap_bh402_w21_1_d2 :  std_logic;
signal heap_bh402_w20_1, heap_bh402_w20_1_d1, heap_bh402_w20_1_d2 :  std_logic;
signal heap_bh402_w19_1, heap_bh402_w19_1_d1, heap_bh402_w19_1_d2 :  std_logic;
signal heap_bh402_w18_1, heap_bh402_w18_1_d1, heap_bh402_w18_1_d2 :  std_logic;
signal heap_bh402_w17_1, heap_bh402_w17_1_d1 :  std_logic;
signal heap_bh402_w16_1, heap_bh402_w16_1_d1 :  std_logic;
signal heap_bh402_w15_1, heap_bh402_w15_1_d1 :  std_logic;
signal heap_bh402_w14_1, heap_bh402_w14_1_d1 :  std_logic;
signal heap_bh402_w13_1, heap_bh402_w13_1_d1 :  std_logic;
signal heap_bh402_w12_2, heap_bh402_w12_2_d1 :  std_logic;
signal heap_bh402_w11_2, heap_bh402_w11_2_d1 :  std_logic;
signal heap_bh402_w10_2, heap_bh402_w10_2_d1 :  std_logic;
signal heap_bh402_w9_4, heap_bh402_w9_4_d1 :  std_logic;
signal heap_bh402_w8_4, heap_bh402_w8_4_d1 :  std_logic;
signal heap_bh402_w7_4, heap_bh402_w7_4_d1 :  std_logic;
signal heap_bh402_w6_5, heap_bh402_w6_5_d1 :  std_logic;
signal heap_bh402_w5_5, heap_bh402_w5_5_d1 :  std_logic;
signal heap_bh402_w4_5, heap_bh402_w4_5_d1 :  std_logic;
signal heap_bh402_w3_7, heap_bh402_w3_7_d1 :  std_logic;
signal heap_bh402_w2_7, heap_bh402_w2_7_d1 :  std_logic;
signal heap_bh402_w1_7, heap_bh402_w1_7_d1 :  std_logic;
signal heap_bh402_w0_9, heap_bh402_w0_9_d1 :  std_logic;
signal DSP_bh402_ch3_0, DSP_bh402_ch3_0_d1 :  std_logic_vector(42 downto 0);
signal DSP_bh402_root3_1, DSP_bh402_root3_1_d1 :  std_logic_vector(42 downto 0);
signal DSP_bh402_ch3_1, DSP_bh402_ch3_1_d1 :  std_logic_vector(43 downto 0);
signal DSP_bh402_root3_2, DSP_bh402_root3_2_d1 :  std_logic_vector(42 downto 0);
signal DSP_bh402_ch3_2 :  std_logic_vector(44 downto 0);
signal heap_bh402_w12_3, heap_bh402_w12_3_d1 :  std_logic;
signal heap_bh402_w11_3, heap_bh402_w11_3_d1 :  std_logic;
signal heap_bh402_w10_3, heap_bh402_w10_3_d1 :  std_logic;
signal heap_bh402_w9_5, heap_bh402_w9_5_d1 :  std_logic;
signal heap_bh402_w8_5, heap_bh402_w8_5_d1 :  std_logic;
signal heap_bh402_w7_5, heap_bh402_w7_5_d1 :  std_logic;
signal heap_bh402_w6_6, heap_bh402_w6_6_d1 :  std_logic;
signal heap_bh402_w5_6, heap_bh402_w5_6_d1, heap_bh402_w5_6_d2, heap_bh402_w5_6_d3, heap_bh402_w5_6_d4 :  std_logic;
signal heap_bh402_w4_6, heap_bh402_w4_6_d1 :  std_logic;
signal heap_bh402_w3_8, heap_bh402_w3_8_d1 :  std_logic;
signal heap_bh402_w2_8, heap_bh402_w2_8_d1 :  std_logic;
signal heap_bh402_w1_8, heap_bh402_w1_8_d1, heap_bh402_w1_8_d2, heap_bh402_w1_8_d3, heap_bh402_w1_8_d4 :  std_logic;
signal heap_bh402_w0_10, heap_bh402_w0_10_d1, heap_bh402_w0_10_d2, heap_bh402_w0_10_d3, heap_bh402_w0_10_d4 :  std_logic;
signal heap_bh402_w57_1, heap_bh402_w57_1_d1 :  std_logic;
signal heap_bh402_w56_1, heap_bh402_w56_1_d1, heap_bh402_w56_1_d2 :  std_logic;
signal heap_bh402_w55_1, heap_bh402_w55_1_d1 :  std_logic;
signal heap_bh402_w54_1, heap_bh402_w54_1_d1, heap_bh402_w54_1_d2 :  std_logic;
signal heap_bh402_w53_1, heap_bh402_w53_1_d1 :  std_logic;
signal heap_bh402_w52_1, heap_bh402_w52_1_d1 :  std_logic;
signal heap_bh402_w51_1, heap_bh402_w51_1_d1 :  std_logic;
signal heap_bh402_w50_1, heap_bh402_w50_1_d1 :  std_logic;
signal heap_bh402_w49_1, heap_bh402_w49_1_d1 :  std_logic;
signal heap_bh402_w48_1, heap_bh402_w48_1_d1 :  std_logic;
signal heap_bh402_w47_1, heap_bh402_w47_1_d1 :  std_logic;
signal heap_bh402_w46_1, heap_bh402_w46_1_d1, heap_bh402_w46_1_d2, heap_bh402_w46_1_d3 :  std_logic;
signal heap_bh402_w45_1, heap_bh402_w45_1_d1, heap_bh402_w45_1_d2 :  std_logic;
signal heap_bh402_w44_1, heap_bh402_w44_1_d1, heap_bh402_w44_1_d2 :  std_logic;
signal heap_bh402_w43_1, heap_bh402_w43_1_d1, heap_bh402_w43_1_d2 :  std_logic;
signal heap_bh402_w42_1, heap_bh402_w42_1_d1, heap_bh402_w42_1_d2 :  std_logic;
signal heap_bh402_w41_1, heap_bh402_w41_1_d1, heap_bh402_w41_1_d2 :  std_logic;
signal heap_bh402_w40_1, heap_bh402_w40_1_d1, heap_bh402_w40_1_d2 :  std_logic;
signal heap_bh402_w39_1, heap_bh402_w39_1_d1, heap_bh402_w39_1_d2 :  std_logic;
signal heap_bh402_w38_1, heap_bh402_w38_1_d1, heap_bh402_w38_1_d2 :  std_logic;
signal heap_bh402_w37_1, heap_bh402_w37_1_d1 :  std_logic;
signal heap_bh402_w36_1, heap_bh402_w36_1_d1 :  std_logic;
signal heap_bh402_w35_1, heap_bh402_w35_1_d1 :  std_logic;
signal heap_bh402_w34_1, heap_bh402_w34_1_d1 :  std_logic;
signal heap_bh402_w33_1, heap_bh402_w33_1_d1 :  std_logic;
signal heap_bh402_w32_1, heap_bh402_w32_1_d1 :  std_logic;
signal heap_bh402_w31_2, heap_bh402_w31_2_d1 :  std_logic;
signal heap_bh402_w30_2, heap_bh402_w30_2_d1 :  std_logic;
signal heap_bh402_w29_2, heap_bh402_w29_2_d1 :  std_logic;
signal heap_bh402_w28_2, heap_bh402_w28_2_d1 :  std_logic;
signal heap_bh402_w27_2, heap_bh402_w27_2_d1, heap_bh402_w27_2_d2 :  std_logic;
signal heap_bh402_w26_2, heap_bh402_w26_2_d1, heap_bh402_w26_2_d2 :  std_logic;
signal heap_bh402_w25_2, heap_bh402_w25_2_d1, heap_bh402_w25_2_d2 :  std_logic;
signal heap_bh402_w24_2, heap_bh402_w24_2_d1, heap_bh402_w24_2_d2 :  std_logic;
signal heap_bh402_w23_2, heap_bh402_w23_2_d1, heap_bh402_w23_2_d2 :  std_logic;
signal heap_bh402_w22_2, heap_bh402_w22_2_d1 :  std_logic;
signal heap_bh402_w21_2, heap_bh402_w21_2_d1 :  std_logic;
signal heap_bh402_w20_2, heap_bh402_w20_2_d1 :  std_logic;
signal heap_bh402_w19_2, heap_bh402_w19_2_d1 :  std_logic;
signal heap_bh402_w18_2, heap_bh402_w18_2_d1 :  std_logic;
signal heap_bh402_w17_2, heap_bh402_w17_2_d1 :  std_logic;
signal heap_bh402_w16_2, heap_bh402_w16_2_d1 :  std_logic;
signal heap_bh402_w15_2, heap_bh402_w15_2_d1 :  std_logic;
signal heap_bh402_w14_2, heap_bh402_w14_2_d1 :  std_logic;
signal heap_bh402_w13_2, heap_bh402_w13_2_d1 :  std_logic;
signal heap_bh402_w6_7 :  std_logic;
signal heap_bh402_w31_3, heap_bh402_w31_3_d1 :  std_logic;
signal heap_bh402_w32_2, heap_bh402_w32_2_d1 :  std_logic;
signal heap_bh402_w33_2, heap_bh402_w33_2_d1 :  std_logic;
signal heap_bh402_w34_2, heap_bh402_w34_2_d1 :  std_logic;
signal heap_bh402_w35_2, heap_bh402_w35_2_d1 :  std_logic;
signal heap_bh402_w36_2, heap_bh402_w36_2_d1 :  std_logic;
signal heap_bh402_w37_2, heap_bh402_w37_2_d1 :  std_logic;
signal heap_bh402_w38_2, heap_bh402_w38_2_d1 :  std_logic;
signal heap_bh402_w39_2, heap_bh402_w39_2_d1, heap_bh402_w39_2_d2 :  std_logic;
signal heap_bh402_w40_2, heap_bh402_w40_2_d1, heap_bh402_w40_2_d2 :  std_logic;
signal heap_bh402_w41_2, heap_bh402_w41_2_d1, heap_bh402_w41_2_d2 :  std_logic;
signal heap_bh402_w42_2, heap_bh402_w42_2_d1, heap_bh402_w42_2_d2 :  std_logic;
signal heap_bh402_w43_2, heap_bh402_w43_2_d1, heap_bh402_w43_2_d2 :  std_logic;
signal heap_bh402_w44_2, heap_bh402_w44_2_d1, heap_bh402_w44_2_d2 :  std_logic;
signal heap_bh402_w45_2, heap_bh402_w45_2_d1, heap_bh402_w45_2_d2 :  std_logic;
signal heap_bh402_w46_2, heap_bh402_w46_2_d1, heap_bh402_w46_2_d2 :  std_logic;
signal heap_bh402_w47_2, heap_bh402_w47_2_d1, heap_bh402_w47_2_d2, heap_bh402_w47_2_d3 :  std_logic;
signal heap_bh402_w48_2, heap_bh402_w48_2_d1, heap_bh402_w48_2_d2, heap_bh402_w48_2_d3 :  std_logic;
signal heap_bh402_w49_2, heap_bh402_w49_2_d1, heap_bh402_w49_2_d2, heap_bh402_w49_2_d3 :  std_logic;
signal heap_bh402_w50_2, heap_bh402_w50_2_d1, heap_bh402_w50_2_d2, heap_bh402_w50_2_d3 :  std_logic;
signal heap_bh402_w51_2, heap_bh402_w51_2_d1, heap_bh402_w51_2_d2, heap_bh402_w51_2_d3 :  std_logic;
signal heap_bh402_w52_2, heap_bh402_w52_2_d1, heap_bh402_w52_2_d2, heap_bh402_w52_2_d3 :  std_logic;
signal heap_bh402_w53_2, heap_bh402_w53_2_d1, heap_bh402_w53_2_d2, heap_bh402_w53_2_d3 :  std_logic;
signal heap_bh402_w54_2, heap_bh402_w54_2_d1, heap_bh402_w54_2_d2, heap_bh402_w54_2_d3 :  std_logic;
signal heap_bh402_w55_2, heap_bh402_w55_2_d1, heap_bh402_w55_2_d2, heap_bh402_w55_2_d3 :  std_logic;
signal heap_bh402_w56_2, heap_bh402_w56_2_d1, heap_bh402_w56_2_d2, heap_bh402_w56_2_d3 :  std_logic;
signal heap_bh402_w58_1, heap_bh402_w58_1_d1, heap_bh402_w58_1_d2, heap_bh402_w58_1_d3 :  std_logic;
signal heap_bh402_w59_1, heap_bh402_w59_1_d1, heap_bh402_w59_1_d2, heap_bh402_w59_1_d3 :  std_logic;
signal heap_bh402_w60_1, heap_bh402_w60_1_d1, heap_bh402_w60_1_d2, heap_bh402_w60_1_d3 :  std_logic;
signal heap_bh402_w61_1, heap_bh402_w61_1_d1, heap_bh402_w61_1_d2, heap_bh402_w61_1_d3 :  std_logic;
signal heap_bh402_w62_1, heap_bh402_w62_1_d1, heap_bh402_w62_1_d2, heap_bh402_w62_1_d3 :  std_logic;
signal heap_bh402_w63_1, heap_bh402_w63_1_d1, heap_bh402_w63_1_d2, heap_bh402_w63_1_d3, heap_bh402_w63_1_d4 :  std_logic;
signal heap_bh402_w64_1, heap_bh402_w64_1_d1, heap_bh402_w64_1_d2, heap_bh402_w64_1_d3, heap_bh402_w64_1_d4 :  std_logic;
signal heap_bh402_w65_1, heap_bh402_w65_1_d1, heap_bh402_w65_1_d2, heap_bh402_w65_1_d3, heap_bh402_w65_1_d4 :  std_logic;
signal CompressorIn_bh402_0_0 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh402_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh402_w0_11, heap_bh402_w0_11_d1 :  std_logic;
signal heap_bh402_w1_9 :  std_logic;
signal heap_bh402_w2_9 :  std_logic;
signal CompressorIn_bh402_1_1 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh402_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh402_w1_10 :  std_logic;
signal heap_bh402_w2_10 :  std_logic;
signal heap_bh402_w3_9 :  std_logic;
signal CompressorIn_bh402_2_2 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh402_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh402_w2_11 :  std_logic;
signal heap_bh402_w3_10 :  std_logic;
signal heap_bh402_w4_7 :  std_logic;
signal CompressorIn_bh402_3_3 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh402_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh402_w3_11 :  std_logic;
signal heap_bh402_w4_8 :  std_logic;
signal heap_bh402_w5_7, heap_bh402_w5_7_d1 :  std_logic;
signal CompressorIn_bh402_4_4 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh402_4_4 :  std_logic_vector(2 downto 0);
signal heap_bh402_w6_8 :  std_logic;
signal heap_bh402_w7_6, heap_bh402_w7_6_d1 :  std_logic;
signal heap_bh402_w8_6 :  std_logic;
signal CompressorIn_bh402_5_5 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_5_6 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_5_5 :  std_logic_vector(2 downto 0);
signal heap_bh402_w4_9 :  std_logic;
signal heap_bh402_w5_8, heap_bh402_w5_8_d1 :  std_logic;
signal heap_bh402_w6_9 :  std_logic;
signal CompressorIn_bh402_6_7 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_6_8 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_6_6 :  std_logic_vector(2 downto 0);
signal heap_bh402_w7_7 :  std_logic;
signal heap_bh402_w8_7 :  std_logic;
signal heap_bh402_w9_6, heap_bh402_w9_6_d1 :  std_logic;
signal CompressorIn_bh402_7_9 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_7_10 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_7_7 :  std_logic_vector(2 downto 0);
signal heap_bh402_w9_7 :  std_logic;
signal heap_bh402_w10_4, heap_bh402_w10_4_d1 :  std_logic;
signal heap_bh402_w11_4 :  std_logic;
signal CompressorIn_bh402_8_11 :  std_logic_vector(3 downto 0);
signal CompressorOut_bh402_8_8 :  std_logic_vector(2 downto 0);
signal heap_bh402_w5_9 :  std_logic;
signal heap_bh402_w6_10 :  std_logic;
signal heap_bh402_w7_8 :  std_logic;
signal CompressorIn_bh402_9_12 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_9_13 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_9_9 :  std_logic_vector(2 downto 0);
signal heap_bh402_w0_12, heap_bh402_w0_12_d1 :  std_logic;
signal heap_bh402_w1_11 :  std_logic;
signal heap_bh402_w2_12 :  std_logic;
signal CompressorIn_bh402_10_14 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh402_10_10 :  std_logic_vector(1 downto 0);
signal heap_bh402_w8_8 :  std_logic;
signal heap_bh402_w9_8 :  std_logic;
signal CompressorIn_bh402_11_15 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_11_16 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_11_11 :  std_logic_vector(2 downto 0);
signal heap_bh402_w2_13, heap_bh402_w2_13_d1 :  std_logic;
signal heap_bh402_w3_12, heap_bh402_w3_12_d1 :  std_logic;
signal heap_bh402_w4_10, heap_bh402_w4_10_d1 :  std_logic;
signal CompressorIn_bh402_12_17 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_12_18 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_12_12 :  std_logic_vector(2 downto 0);
signal heap_bh402_w4_11, heap_bh402_w4_11_d1 :  std_logic;
signal heap_bh402_w5_10, heap_bh402_w5_10_d1 :  std_logic;
signal heap_bh402_w6_11, heap_bh402_w6_11_d1 :  std_logic;
signal CompressorIn_bh402_13_19 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_13_20 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_13_13 :  std_logic_vector(2 downto 0);
signal heap_bh402_w6_12, heap_bh402_w6_12_d1 :  std_logic;
signal heap_bh402_w7_9, heap_bh402_w7_9_d1 :  std_logic;
signal heap_bh402_w8_9, heap_bh402_w8_9_d1 :  std_logic;
signal CompressorIn_bh402_14_21 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_14_22 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_14_14 :  std_logic_vector(2 downto 0);
signal heap_bh402_w8_10, heap_bh402_w8_10_d1 :  std_logic;
signal heap_bh402_w9_9, heap_bh402_w9_9_d1 :  std_logic;
signal heap_bh402_w10_5, heap_bh402_w10_5_d1 :  std_logic;
signal CompressorIn_bh402_15_23 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_15_24 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_15_15 :  std_logic_vector(2 downto 0);
signal heap_bh402_w11_5, heap_bh402_w11_5_d1 :  std_logic;
signal heap_bh402_w12_4, heap_bh402_w12_4_d1 :  std_logic;
signal heap_bh402_w13_3, heap_bh402_w13_3_d1 :  std_logic;
signal CompressorIn_bh402_16_25 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_16_26 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_16_16 :  std_logic_vector(2 downto 0);
signal heap_bh402_w1_12, heap_bh402_w1_12_d1 :  std_logic;
signal heap_bh402_w2_14, heap_bh402_w2_14_d1 :  std_logic;
signal heap_bh402_w3_13, heap_bh402_w3_13_d1 :  std_logic;
signal CompressorIn_bh402_17_27 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh402_17_17 :  std_logic_vector(1 downto 0);
signal heap_bh402_w3_14, heap_bh402_w3_14_d1 :  std_logic;
signal heap_bh402_w4_12, heap_bh402_w4_12_d1 :  std_logic;
signal CompressorIn_bh402_18_28 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_18_29 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_18_18 :  std_logic_vector(2 downto 0);
signal heap_bh402_w3_15 :  std_logic;
signal heap_bh402_w4_13 :  std_logic;
signal heap_bh402_w5_11, heap_bh402_w5_11_d1, heap_bh402_w5_11_d2 :  std_logic;
signal CompressorIn_bh402_19_30 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_19_31 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_19_19 :  std_logic_vector(2 downto 0);
signal heap_bh402_w5_12 :  std_logic;
signal heap_bh402_w6_13 :  std_logic;
signal heap_bh402_w7_10 :  std_logic;
signal CompressorIn_bh402_20_32 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_20_33 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_20_20 :  std_logic_vector(2 downto 0);
signal heap_bh402_w10_6 :  std_logic;
signal heap_bh402_w11_6 :  std_logic;
signal heap_bh402_w12_5 :  std_logic;
signal CompressorIn_bh402_21_34 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_21_35 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_21_21 :  std_logic_vector(2 downto 0);
signal heap_bh402_w7_11 :  std_logic;
signal heap_bh402_w8_11 :  std_logic;
signal heap_bh402_w9_10 :  std_logic;
signal CompressorIn_bh402_22_36 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_22_37 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_22_22 :  std_logic_vector(2 downto 0);
signal heap_bh402_w0_13, heap_bh402_w0_13_d1, heap_bh402_w0_13_d2, heap_bh402_w0_13_d3, heap_bh402_w0_13_d4, heap_bh402_w0_13_d5 :  std_logic;
signal heap_bh402_w1_13, heap_bh402_w1_13_d1, heap_bh402_w1_13_d2, heap_bh402_w1_13_d3, heap_bh402_w1_13_d4, heap_bh402_w1_13_d5 :  std_logic;
signal heap_bh402_w2_15, heap_bh402_w2_15_d1, heap_bh402_w2_15_d2 :  std_logic;
signal CompressorIn_bh402_23_38 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_23_39 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_23_23 :  std_logic_vector(2 downto 0);
signal heap_bh402_w2_16, heap_bh402_w2_16_d1, heap_bh402_w2_16_d2 :  std_logic;
signal heap_bh402_w3_16, heap_bh402_w3_16_d1, heap_bh402_w3_16_d2 :  std_logic;
signal heap_bh402_w4_14, heap_bh402_w4_14_d1, heap_bh402_w4_14_d2 :  std_logic;
signal CompressorIn_bh402_24_40 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_24_41 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_24_24 :  std_logic_vector(2 downto 0);
signal heap_bh402_w4_15, heap_bh402_w4_15_d1, heap_bh402_w4_15_d2 :  std_logic;
signal heap_bh402_w5_13, heap_bh402_w5_13_d1, heap_bh402_w5_13_d2 :  std_logic;
signal heap_bh402_w6_14 :  std_logic;
signal CompressorIn_bh402_25_42 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_25_43 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_25_25 :  std_logic_vector(2 downto 0);
signal heap_bh402_w9_11 :  std_logic;
signal heap_bh402_w10_7, heap_bh402_w10_7_d1, heap_bh402_w10_7_d2 :  std_logic;
signal heap_bh402_w11_7 :  std_logic;
signal CompressorIn_bh402_26_44 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_26_45 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_26_26 :  std_logic_vector(2 downto 0);
signal heap_bh402_w12_6 :  std_logic;
signal heap_bh402_w13_4 :  std_logic;
signal heap_bh402_w14_3 :  std_logic;
signal CompressorIn_bh402_27_46 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_27_47 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_27_27 :  std_logic_vector(2 downto 0);
signal heap_bh402_w31_4, heap_bh402_w31_4_d1, heap_bh402_w31_4_d2 :  std_logic;
signal heap_bh402_w32_3, heap_bh402_w32_3_d1, heap_bh402_w32_3_d2 :  std_logic;
signal heap_bh402_w33_3 :  std_logic;
signal CompressorIn_bh402_28_48 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_28_49 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_28_28 :  std_logic_vector(2 downto 0);
signal heap_bh402_w8_12, heap_bh402_w8_12_d1, heap_bh402_w8_12_d2 :  std_logic;
signal heap_bh402_w9_12, heap_bh402_w9_12_d1, heap_bh402_w9_12_d2 :  std_logic;
signal heap_bh402_w10_8, heap_bh402_w10_8_d1, heap_bh402_w10_8_d2 :  std_logic;
signal CompressorIn_bh402_29_50 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_29_51 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_29_29 :  std_logic_vector(2 downto 0);
signal heap_bh402_w14_4 :  std_logic;
signal heap_bh402_w15_3, heap_bh402_w15_3_d1, heap_bh402_w15_3_d2 :  std_logic;
signal heap_bh402_w16_3 :  std_logic;
signal CompressorIn_bh402_30_52 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_30_53 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_30_30 :  std_logic_vector(2 downto 0);
signal heap_bh402_w33_4, heap_bh402_w33_4_d1, heap_bh402_w33_4_d2 :  std_logic;
signal heap_bh402_w34_3, heap_bh402_w34_3_d1, heap_bh402_w34_3_d2 :  std_logic;
signal heap_bh402_w35_3 :  std_logic;
signal CompressorIn_bh402_31_54 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_31_55 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_31_31 :  std_logic_vector(2 downto 0);
signal heap_bh402_w6_15, heap_bh402_w6_15_d1, heap_bh402_w6_15_d2 :  std_logic;
signal heap_bh402_w7_12, heap_bh402_w7_12_d1, heap_bh402_w7_12_d2 :  std_logic;
signal heap_bh402_w8_13, heap_bh402_w8_13_d1, heap_bh402_w8_13_d2 :  std_logic;
signal CompressorIn_bh402_32_56 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_32_57 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_32_32 :  std_logic_vector(2 downto 0);
signal heap_bh402_w11_8, heap_bh402_w11_8_d1, heap_bh402_w11_8_d2 :  std_logic;
signal heap_bh402_w12_7, heap_bh402_w12_7_d1, heap_bh402_w12_7_d2 :  std_logic;
signal heap_bh402_w13_5 :  std_logic;
signal CompressorIn_bh402_33_58 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_33_59 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_33_33 :  std_logic_vector(2 downto 0);
signal heap_bh402_w16_4, heap_bh402_w16_4_d1, heap_bh402_w16_4_d2 :  std_logic;
signal heap_bh402_w17_3, heap_bh402_w17_3_d1, heap_bh402_w17_3_d2 :  std_logic;
signal heap_bh402_w18_3, heap_bh402_w18_3_d1 :  std_logic;
signal CompressorIn_bh402_34_60 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_34_61 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_34_34 :  std_logic_vector(2 downto 0);
signal heap_bh402_w35_4, heap_bh402_w35_4_d1, heap_bh402_w35_4_d2 :  std_logic;
signal heap_bh402_w36_3, heap_bh402_w36_3_d1, heap_bh402_w36_3_d2 :  std_logic;
signal heap_bh402_w37_3 :  std_logic;
signal CompressorIn_bh402_35_62 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_35_63 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_35_35 :  std_logic_vector(2 downto 0);
signal heap_bh402_w13_6, heap_bh402_w13_6_d1, heap_bh402_w13_6_d2 :  std_logic;
signal heap_bh402_w14_5, heap_bh402_w14_5_d1, heap_bh402_w14_5_d2 :  std_logic;
signal heap_bh402_w15_4, heap_bh402_w15_4_d1, heap_bh402_w15_4_d2 :  std_logic;
signal CompressorIn_bh402_36_64 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_36_65 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_36_36 :  std_logic_vector(2 downto 0);
signal heap_bh402_w18_4, heap_bh402_w18_4_d1 :  std_logic;
signal heap_bh402_w19_3, heap_bh402_w19_3_d1 :  std_logic;
signal heap_bh402_w20_3 :  std_logic;
signal CompressorIn_bh402_37_66 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_37_67 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_37_37 :  std_logic_vector(2 downto 0);
signal heap_bh402_w37_4, heap_bh402_w37_4_d1, heap_bh402_w37_4_d2 :  std_logic;
signal heap_bh402_w38_3, heap_bh402_w38_3_d1, heap_bh402_w38_3_d2, heap_bh402_w38_3_d3 :  std_logic;
signal heap_bh402_w39_3, heap_bh402_w39_3_d1 :  std_logic;
signal CompressorIn_bh402_38_68 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_38_69 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_38_38 :  std_logic_vector(2 downto 0);
signal heap_bh402_w20_4, heap_bh402_w20_4_d1 :  std_logic;
signal heap_bh402_w21_3, heap_bh402_w21_3_d1 :  std_logic;
signal heap_bh402_w22_3 :  std_logic;
signal CompressorIn_bh402_39_70 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_39_71 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_39_39 :  std_logic_vector(2 downto 0);
signal heap_bh402_w39_4, heap_bh402_w39_4_d1, heap_bh402_w39_4_d2 :  std_logic;
signal heap_bh402_w40_3, heap_bh402_w40_3_d1, heap_bh402_w40_3_d2 :  std_logic;
signal heap_bh402_w41_3 :  std_logic;
signal CompressorIn_bh402_40_72 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_40_73 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_40_40 :  std_logic_vector(2 downto 0);
signal heap_bh402_w22_4, heap_bh402_w22_4_d1 :  std_logic;
signal heap_bh402_w23_3, heap_bh402_w23_3_d1, heap_bh402_w23_3_d2 :  std_logic;
signal heap_bh402_w24_3 :  std_logic;
signal CompressorIn_bh402_41_74 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_41_75 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_41_41 :  std_logic_vector(2 downto 0);
signal heap_bh402_w41_4, heap_bh402_w41_4_d1, heap_bh402_w41_4_d2 :  std_logic;
signal heap_bh402_w42_3, heap_bh402_w42_3_d1, heap_bh402_w42_3_d2 :  std_logic;
signal heap_bh402_w43_3 :  std_logic;
signal CompressorIn_bh402_42_76 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_42_77 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_42_42 :  std_logic_vector(2 downto 0);
signal heap_bh402_w24_4, heap_bh402_w24_4_d1, heap_bh402_w24_4_d2 :  std_logic;
signal heap_bh402_w25_3, heap_bh402_w25_3_d1, heap_bh402_w25_3_d2 :  std_logic;
signal heap_bh402_w26_3, heap_bh402_w26_3_d1 :  std_logic;
signal CompressorIn_bh402_43_78 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_43_79 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_43_43 :  std_logic_vector(2 downto 0);
signal heap_bh402_w43_4, heap_bh402_w43_4_d1, heap_bh402_w43_4_d2 :  std_logic;
signal heap_bh402_w44_3, heap_bh402_w44_3_d1, heap_bh402_w44_3_d2 :  std_logic;
signal heap_bh402_w45_3 :  std_logic;
signal CompressorIn_bh402_44_80 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_44_81 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_44_44 :  std_logic_vector(2 downto 0);
signal heap_bh402_w26_4, heap_bh402_w26_4_d1 :  std_logic;
signal heap_bh402_w27_3, heap_bh402_w27_3_d1 :  std_logic;
signal heap_bh402_w28_3 :  std_logic;
signal CompressorIn_bh402_45_82 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_45_83 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_45_45 :  std_logic_vector(2 downto 0);
signal heap_bh402_w45_4, heap_bh402_w45_4_d1, heap_bh402_w45_4_d2 :  std_logic;
signal heap_bh402_w46_3, heap_bh402_w46_3_d1, heap_bh402_w46_3_d2, heap_bh402_w46_3_d3 :  std_logic;
signal heap_bh402_w47_3, heap_bh402_w47_3_d1 :  std_logic;
signal CompressorIn_bh402_46_84 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_46_85 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_46_46 :  std_logic_vector(2 downto 0);
signal heap_bh402_w28_4, heap_bh402_w28_4_d1 :  std_logic;
signal heap_bh402_w29_3 :  std_logic;
signal heap_bh402_w30_3 :  std_logic;
signal CompressorIn_bh402_47_86 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh402_47_87 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_47_47 :  std_logic_vector(2 downto 0);
signal heap_bh402_w47_4, heap_bh402_w47_4_d1, heap_bh402_w47_4_d2 :  std_logic;
signal heap_bh402_w48_3 :  std_logic;
signal heap_bh402_w49_3 :  std_logic;
signal CompressorIn_bh402_48_88 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_48_89 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_48_48 :  std_logic_vector(2 downto 0);
signal heap_bh402_w2_17, heap_bh402_w2_17_d1, heap_bh402_w2_17_d2, heap_bh402_w2_17_d3 :  std_logic;
signal heap_bh402_w3_17, heap_bh402_w3_17_d1, heap_bh402_w3_17_d2, heap_bh402_w3_17_d3 :  std_logic;
signal heap_bh402_w4_16, heap_bh402_w4_16_d1, heap_bh402_w4_16_d2, heap_bh402_w4_16_d3 :  std_logic;
signal CompressorIn_bh402_49_90 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_49_91 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_49_49 :  std_logic_vector(2 downto 0);
signal heap_bh402_w4_17, heap_bh402_w4_17_d1, heap_bh402_w4_17_d2, heap_bh402_w4_17_d3 :  std_logic;
signal heap_bh402_w5_14, heap_bh402_w5_14_d1, heap_bh402_w5_14_d2, heap_bh402_w5_14_d3 :  std_logic;
signal heap_bh402_w6_16 :  std_logic;
signal CompressorIn_bh402_50_92 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_50_93 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_50_50 :  std_logic_vector(2 downto 0);
signal heap_bh402_w8_14, heap_bh402_w8_14_d1, heap_bh402_w8_14_d2, heap_bh402_w8_14_d3 :  std_logic;
signal heap_bh402_w9_13, heap_bh402_w9_13_d1, heap_bh402_w9_13_d2, heap_bh402_w9_13_d3 :  std_logic;
signal heap_bh402_w10_9, heap_bh402_w10_9_d1, heap_bh402_w10_9_d2, heap_bh402_w10_9_d3 :  std_logic;
signal CompressorIn_bh402_51_94 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_51_95 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_51_51 :  std_logic_vector(2 downto 0);
signal heap_bh402_w10_10, heap_bh402_w10_10_d1, heap_bh402_w10_10_d2, heap_bh402_w10_10_d3 :  std_logic;
signal heap_bh402_w11_9, heap_bh402_w11_9_d1, heap_bh402_w11_9_d2, heap_bh402_w11_9_d3 :  std_logic;
signal heap_bh402_w12_8 :  std_logic;
signal CompressorIn_bh402_52_96 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_52_97 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_52_52 :  std_logic_vector(2 downto 0);
signal heap_bh402_w15_5 :  std_logic;
signal heap_bh402_w16_5, heap_bh402_w16_5_d1, heap_bh402_w16_5_d2, heap_bh402_w16_5_d3 :  std_logic;
signal heap_bh402_w17_4 :  std_logic;
signal CompressorIn_bh402_53_98 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_53_99 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_53_53 :  std_logic_vector(2 downto 0);
signal heap_bh402_w30_4 :  std_logic;
signal heap_bh402_w31_5, heap_bh402_w31_5_d1, heap_bh402_w31_5_d2, heap_bh402_w31_5_d3 :  std_logic;
signal heap_bh402_w32_4 :  std_logic;
signal CompressorIn_bh402_54_100 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_54_101 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_54_54 :  std_logic_vector(2 downto 0);
signal heap_bh402_w49_4 :  std_logic;
signal heap_bh402_w50_3 :  std_logic;
signal heap_bh402_w51_3 :  std_logic;
signal CompressorIn_bh402_55_102 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_55_103 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_55_55 :  std_logic_vector(2 downto 0);
signal heap_bh402_w51_4 :  std_logic;
signal heap_bh402_w52_3 :  std_logic;
signal heap_bh402_w53_3 :  std_logic;
signal CompressorIn_bh402_56_104 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_56_105 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_56_56 :  std_logic_vector(2 downto 0);
signal heap_bh402_w53_4 :  std_logic;
signal heap_bh402_w54_3, heap_bh402_w54_3_d1 :  std_logic;
signal heap_bh402_w55_3, heap_bh402_w55_3_d1 :  std_logic;
signal CompressorIn_bh402_57_106 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_57_107 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_57_57 :  std_logic_vector(2 downto 0);
signal heap_bh402_w55_4, heap_bh402_w55_4_d1 :  std_logic;
signal heap_bh402_w56_3, heap_bh402_w56_3_d1 :  std_logic;
signal heap_bh402_w57_2 :  std_logic;
signal CompressorIn_bh402_58_108 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_58_109 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_58_58 :  std_logic_vector(2 downto 0);
signal heap_bh402_w6_17, heap_bh402_w6_17_d1, heap_bh402_w6_17_d2, heap_bh402_w6_17_d3 :  std_logic;
signal heap_bh402_w7_13, heap_bh402_w7_13_d1, heap_bh402_w7_13_d2, heap_bh402_w7_13_d3 :  std_logic;
signal heap_bh402_w8_15, heap_bh402_w8_15_d1, heap_bh402_w8_15_d2, heap_bh402_w8_15_d3 :  std_logic;
signal CompressorIn_bh402_59_110 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_59_111 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_59_59 :  std_logic_vector(2 downto 0);
signal heap_bh402_w12_9, heap_bh402_w12_9_d1, heap_bh402_w12_9_d2, heap_bh402_w12_9_d3 :  std_logic;
signal heap_bh402_w13_7, heap_bh402_w13_7_d1, heap_bh402_w13_7_d2, heap_bh402_w13_7_d3 :  std_logic;
signal heap_bh402_w14_6 :  std_logic;
signal CompressorIn_bh402_60_112 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_60_113 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_60_60 :  std_logic_vector(2 downto 0);
signal heap_bh402_w17_5, heap_bh402_w17_5_d1, heap_bh402_w17_5_d2, heap_bh402_w17_5_d3 :  std_logic;
signal heap_bh402_w18_5, heap_bh402_w18_5_d1, heap_bh402_w18_5_d2, heap_bh402_w18_5_d3 :  std_logic;
signal heap_bh402_w19_4 :  std_logic;
signal CompressorIn_bh402_61_114 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_61_115 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_61_61 :  std_logic_vector(2 downto 0);
signal heap_bh402_w29_4, heap_bh402_w29_4_d1, heap_bh402_w29_4_d2, heap_bh402_w29_4_d3 :  std_logic;
signal heap_bh402_w30_5, heap_bh402_w30_5_d1, heap_bh402_w30_5_d2, heap_bh402_w30_5_d3 :  std_logic;
signal heap_bh402_w31_6, heap_bh402_w31_6_d1, heap_bh402_w31_6_d2, heap_bh402_w31_6_d3 :  std_logic;
signal CompressorIn_bh402_62_116 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_62_117 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_62_62 :  std_logic_vector(2 downto 0);
signal heap_bh402_w32_5, heap_bh402_w32_5_d1, heap_bh402_w32_5_d2, heap_bh402_w32_5_d3 :  std_logic;
signal heap_bh402_w33_5, heap_bh402_w33_5_d1, heap_bh402_w33_5_d2, heap_bh402_w33_5_d3 :  std_logic;
signal heap_bh402_w34_4 :  std_logic;
signal CompressorIn_bh402_63_118 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_63_119 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_63_63 :  std_logic_vector(2 downto 0);
signal heap_bh402_w48_4, heap_bh402_w48_4_d1, heap_bh402_w48_4_d2, heap_bh402_w48_4_d3 :  std_logic;
signal heap_bh402_w49_5, heap_bh402_w49_5_d1, heap_bh402_w49_5_d2, heap_bh402_w49_5_d3 :  std_logic;
signal heap_bh402_w50_4 :  std_logic;
signal CompressorIn_bh402_64_120 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_64_121 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_64_64 :  std_logic_vector(2 downto 0);
signal heap_bh402_w57_3, heap_bh402_w57_3_d1 :  std_logic;
signal heap_bh402_w58_2, heap_bh402_w58_2_d1, heap_bh402_w58_2_d2, heap_bh402_w58_2_d3 :  std_logic;
signal heap_bh402_w59_2 :  std_logic;
signal CompressorIn_bh402_65_122 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_65_123 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_65_65 :  std_logic_vector(2 downto 0);
signal heap_bh402_w19_5, heap_bh402_w19_5_d1, heap_bh402_w19_5_d2, heap_bh402_w19_5_d3 :  std_logic;
signal heap_bh402_w20_5, heap_bh402_w20_5_d1, heap_bh402_w20_5_d2, heap_bh402_w20_5_d3 :  std_logic;
signal heap_bh402_w21_4 :  std_logic;
signal CompressorIn_bh402_66_124 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_66_125 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_66_66 :  std_logic_vector(2 downto 0);
signal heap_bh402_w34_5, heap_bh402_w34_5_d1, heap_bh402_w34_5_d2, heap_bh402_w34_5_d3 :  std_logic;
signal heap_bh402_w35_5, heap_bh402_w35_5_d1, heap_bh402_w35_5_d2, heap_bh402_w35_5_d3 :  std_logic;
signal heap_bh402_w36_4 :  std_logic;
signal CompressorIn_bh402_67_126 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_67_127 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_67_67 :  std_logic_vector(2 downto 0);
signal heap_bh402_w50_5, heap_bh402_w50_5_d1, heap_bh402_w50_5_d2, heap_bh402_w50_5_d3 :  std_logic;
signal heap_bh402_w51_5, heap_bh402_w51_5_d1, heap_bh402_w51_5_d2, heap_bh402_w51_5_d3 :  std_logic;
signal heap_bh402_w52_4 :  std_logic;
signal CompressorIn_bh402_68_128 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_68_129 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_68_68 :  std_logic_vector(2 downto 0);
signal heap_bh402_w59_3, heap_bh402_w59_3_d1, heap_bh402_w59_3_d2, heap_bh402_w59_3_d3 :  std_logic;
signal heap_bh402_w60_2, heap_bh402_w60_2_d1, heap_bh402_w60_2_d2, heap_bh402_w60_2_d3 :  std_logic;
signal heap_bh402_w61_2 :  std_logic;
signal CompressorIn_bh402_69_130 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_69_131 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_69_69 :  std_logic_vector(2 downto 0);
signal heap_bh402_w14_7, heap_bh402_w14_7_d1, heap_bh402_w14_7_d2, heap_bh402_w14_7_d3 :  std_logic;
signal heap_bh402_w15_6, heap_bh402_w15_6_d1, heap_bh402_w15_6_d2, heap_bh402_w15_6_d3 :  std_logic;
signal heap_bh402_w16_6, heap_bh402_w16_6_d1, heap_bh402_w16_6_d2, heap_bh402_w16_6_d3 :  std_logic;
signal CompressorIn_bh402_70_132 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_70_133 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_70_70 :  std_logic_vector(2 downto 0);
signal heap_bh402_w21_5, heap_bh402_w21_5_d1, heap_bh402_w21_5_d2, heap_bh402_w21_5_d3 :  std_logic;
signal heap_bh402_w22_5, heap_bh402_w22_5_d1, heap_bh402_w22_5_d2, heap_bh402_w22_5_d3 :  std_logic;
signal heap_bh402_w23_4, heap_bh402_w23_4_d1 :  std_logic;
signal CompressorIn_bh402_71_134 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_71_135 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_71_71 :  std_logic_vector(2 downto 0);
signal heap_bh402_w36_5, heap_bh402_w36_5_d1, heap_bh402_w36_5_d2, heap_bh402_w36_5_d3 :  std_logic;
signal heap_bh402_w37_5, heap_bh402_w37_5_d1, heap_bh402_w37_5_d2, heap_bh402_w37_5_d3 :  std_logic;
signal heap_bh402_w38_4, heap_bh402_w38_4_d1 :  std_logic;
signal CompressorIn_bh402_72_136 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_72_137 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_72_72 :  std_logic_vector(2 downto 0);
signal heap_bh402_w52_5, heap_bh402_w52_5_d1, heap_bh402_w52_5_d2, heap_bh402_w52_5_d3 :  std_logic;
signal heap_bh402_w53_5, heap_bh402_w53_5_d1, heap_bh402_w53_5_d2, heap_bh402_w53_5_d3 :  std_logic;
signal heap_bh402_w54_4, heap_bh402_w54_4_d1 :  std_logic;
signal CompressorIn_bh402_73_138 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_73_139 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_73_73 :  std_logic_vector(2 downto 0);
signal heap_bh402_w61_3, heap_bh402_w61_3_d1, heap_bh402_w61_3_d2, heap_bh402_w61_3_d3 :  std_logic;
signal heap_bh402_w62_2, heap_bh402_w62_2_d1, heap_bh402_w62_2_d2, heap_bh402_w62_2_d3 :  std_logic;
signal heap_bh402_w63_2, heap_bh402_w63_2_d1 :  std_logic;
signal CompressorIn_bh402_74_140 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_74_141 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_74_74 :  std_logic_vector(2 downto 0);
signal heap_bh402_w23_5, heap_bh402_w23_5_d1, heap_bh402_w23_5_d2 :  std_logic;
signal heap_bh402_w24_5, heap_bh402_w24_5_d1, heap_bh402_w24_5_d2 :  std_logic;
signal heap_bh402_w25_4 :  std_logic;
signal CompressorIn_bh402_75_142 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_75_143 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_75_75 :  std_logic_vector(2 downto 0);
signal heap_bh402_w38_5, heap_bh402_w38_5_d1, heap_bh402_w38_5_d2 :  std_logic;
signal heap_bh402_w39_5, heap_bh402_w39_5_d1, heap_bh402_w39_5_d2 :  std_logic;
signal heap_bh402_w40_4 :  std_logic;
signal CompressorIn_bh402_76_144 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_76_145 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_76_76 :  std_logic_vector(2 downto 0);
signal heap_bh402_w54_5, heap_bh402_w54_5_d1, heap_bh402_w54_5_d2 :  std_logic;
signal heap_bh402_w55_5, heap_bh402_w55_5_d1, heap_bh402_w55_5_d2 :  std_logic;
signal heap_bh402_w56_4 :  std_logic;
signal CompressorIn_bh402_77_146 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_77_147 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_77_77 :  std_logic_vector(2 downto 0);
signal heap_bh402_w63_3, heap_bh402_w63_3_d1, heap_bh402_w63_3_d2 :  std_logic;
signal heap_bh402_w64_2, heap_bh402_w64_2_d1, heap_bh402_w64_2_d2 :  std_logic;
signal heap_bh402_w65_2 :  std_logic;
signal CompressorIn_bh402_78_148 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_78_149 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_78_78 :  std_logic_vector(2 downto 0);
signal heap_bh402_w25_5, heap_bh402_w25_5_d1, heap_bh402_w25_5_d2 :  std_logic;
signal heap_bh402_w26_5, heap_bh402_w26_5_d1, heap_bh402_w26_5_d2 :  std_logic;
signal heap_bh402_w27_4 :  std_logic;
signal CompressorIn_bh402_79_150 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_79_151 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_79_79 :  std_logic_vector(2 downto 0);
signal heap_bh402_w40_5, heap_bh402_w40_5_d1, heap_bh402_w40_5_d2 :  std_logic;
signal heap_bh402_w41_5, heap_bh402_w41_5_d1, heap_bh402_w41_5_d2 :  std_logic;
signal heap_bh402_w42_4 :  std_logic;
signal CompressorIn_bh402_80_152 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_80_153 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_80_80 :  std_logic_vector(2 downto 0);
signal heap_bh402_w56_5, heap_bh402_w56_5_d1, heap_bh402_w56_5_d2 :  std_logic;
signal heap_bh402_w57_4, heap_bh402_w57_4_d1, heap_bh402_w57_4_d2 :  std_logic;
signal heap_bh402_w58_3, heap_bh402_w58_3_d1, heap_bh402_w58_3_d2 :  std_logic;
signal CompressorIn_bh402_81_154 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh402_81_81 :  std_logic_vector(1 downto 0);
signal heap_bh402_w65_3, heap_bh402_w65_3_d1, heap_bh402_w65_3_d2 :  std_logic;
signal CompressorIn_bh402_82_155 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_82_156 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_82_82 :  std_logic_vector(2 downto 0);
signal heap_bh402_w42_5, heap_bh402_w42_5_d1, heap_bh402_w42_5_d2 :  std_logic;
signal heap_bh402_w43_5, heap_bh402_w43_5_d1, heap_bh402_w43_5_d2 :  std_logic;
signal heap_bh402_w44_4 :  std_logic;
signal CompressorIn_bh402_83_157 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_83_158 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_83_83 :  std_logic_vector(2 downto 0);
signal heap_bh402_w27_5, heap_bh402_w27_5_d1, heap_bh402_w27_5_d2 :  std_logic;
signal heap_bh402_w28_5, heap_bh402_w28_5_d1, heap_bh402_w28_5_d2 :  std_logic;
signal heap_bh402_w29_5, heap_bh402_w29_5_d1, heap_bh402_w29_5_d2 :  std_logic;
signal CompressorIn_bh402_84_159 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_84_160 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh402_84_84 :  std_logic_vector(2 downto 0);
signal heap_bh402_w44_5, heap_bh402_w44_5_d1, heap_bh402_w44_5_d2 :  std_logic;
signal heap_bh402_w45_5, heap_bh402_w45_5_d1, heap_bh402_w45_5_d2 :  std_logic;
signal heap_bh402_w46_4, heap_bh402_w46_4_d1 :  std_logic;
signal CompressorIn_bh402_85_161 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh402_85_162 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh402_85_85 :  std_logic_vector(2 downto 0);
signal heap_bh402_w46_5, heap_bh402_w46_5_d1 :  std_logic;
signal heap_bh402_w47_5, heap_bh402_w47_5_d1 :  std_logic;
signal heap_bh402_w48_5, heap_bh402_w48_5_d1 :  std_logic;
signal finalAdderIn0_bh402 :  std_logic_vector(66 downto 0);
signal finalAdderIn1_bh402 :  std_logic_vector(66 downto 0);
signal finalAdderCin_bh402 :  std_logic;
signal finalAdderOut_bh402 :  std_logic_vector(66 downto 0);
signal CompressionResult402 :  std_logic_vector(66 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid408: component is "yes";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid408: component is "distributed";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            XX_m404_d1 <=  XX_m404;
            YY_m404_d1 <=  YY_m404;
            heap_bh402_w10_0_d1 <=  heap_bh402_w10_0;
            heap_bh402_w13_0_d1 <=  heap_bh402_w13_0;
            heap_bh402_w14_0_d1 <=  heap_bh402_w14_0;
            heap_bh402_w15_0_d1 <=  heap_bh402_w15_0;
            heap_bh402_w16_0_d1 <=  heap_bh402_w16_0;
            heap_bh402_w17_0_d1 <=  heap_bh402_w17_0;
            heap_bh402_w18_0_d1 <=  heap_bh402_w18_0;
            heap_bh402_w18_0_d2 <=  heap_bh402_w18_0_d1;
            heap_bh402_w19_0_d1 <=  heap_bh402_w19_0;
            heap_bh402_w19_0_d2 <=  heap_bh402_w19_0_d1;
            heap_bh402_w20_0_d1 <=  heap_bh402_w20_0;
            heap_bh402_w20_0_d2 <=  heap_bh402_w20_0_d1;
            heap_bh402_w21_0_d1 <=  heap_bh402_w21_0;
            heap_bh402_w21_0_d2 <=  heap_bh402_w21_0_d1;
            heap_bh402_w22_0_d1 <=  heap_bh402_w22_0;
            heap_bh402_w22_0_d2 <=  heap_bh402_w22_0_d1;
            heap_bh402_w23_0_d1 <=  heap_bh402_w23_0;
            heap_bh402_w23_0_d2 <=  heap_bh402_w23_0_d1;
            heap_bh402_w24_0_d1 <=  heap_bh402_w24_0;
            heap_bh402_w24_0_d2 <=  heap_bh402_w24_0_d1;
            heap_bh402_w25_0_d1 <=  heap_bh402_w25_0;
            heap_bh402_w25_0_d2 <=  heap_bh402_w25_0_d1;
            heap_bh402_w26_0_d1 <=  heap_bh402_w26_0;
            heap_bh402_w26_0_d2 <=  heap_bh402_w26_0_d1;
            heap_bh402_w26_0_d3 <=  heap_bh402_w26_0_d2;
            heap_bh402_w27_0_d1 <=  heap_bh402_w27_0;
            heap_bh402_w27_0_d2 <=  heap_bh402_w27_0_d1;
            heap_bh402_w27_0_d3 <=  heap_bh402_w27_0_d2;
            heap_bh402_w28_0_d1 <=  heap_bh402_w28_0;
            heap_bh402_w28_0_d2 <=  heap_bh402_w28_0_d1;
            heap_bh402_w28_0_d3 <=  heap_bh402_w28_0_d2;
            heap_bh402_w29_0_d1 <=  heap_bh402_w29_0;
            heap_bh402_w29_0_d2 <=  heap_bh402_w29_0_d1;
            heap_bh402_w29_0_d3 <=  heap_bh402_w29_0_d2;
            heap_bh402_w30_0_d1 <=  heap_bh402_w30_0;
            heap_bh402_w30_0_d2 <=  heap_bh402_w30_0_d1;
            heap_bh402_w30_0_d3 <=  heap_bh402_w30_0_d2;
            heap_bh402_w31_0_d1 <=  heap_bh402_w31_0;
            heap_bh402_w32_0_d1 <=  heap_bh402_w32_0;
            heap_bh402_w33_0_d1 <=  heap_bh402_w33_0;
            heap_bh402_w34_0_d1 <=  heap_bh402_w34_0;
            heap_bh402_w35_0_d1 <=  heap_bh402_w35_0;
            heap_bh402_w36_0_d1 <=  heap_bh402_w36_0;
            heap_bh402_w37_0_d1 <=  heap_bh402_w37_0;
            heap_bh402_w38_0_d1 <=  heap_bh402_w38_0;
            heap_bh402_w39_0_d1 <=  heap_bh402_w39_0;
            heap_bh402_w39_0_d2 <=  heap_bh402_w39_0_d1;
            heap_bh402_w40_0_d1 <=  heap_bh402_w40_0;
            heap_bh402_w40_0_d2 <=  heap_bh402_w40_0_d1;
            heap_bh402_w41_0_d1 <=  heap_bh402_w41_0;
            heap_bh402_w41_0_d2 <=  heap_bh402_w41_0_d1;
            heap_bh402_w42_0_d1 <=  heap_bh402_w42_0;
            heap_bh402_w42_0_d2 <=  heap_bh402_w42_0_d1;
            heap_bh402_w43_0_d1 <=  heap_bh402_w43_0;
            heap_bh402_w43_0_d2 <=  heap_bh402_w43_0_d1;
            heap_bh402_w44_0_d1 <=  heap_bh402_w44_0;
            heap_bh402_w44_0_d2 <=  heap_bh402_w44_0_d1;
            heap_bh402_w45_0_d1 <=  heap_bh402_w45_0;
            heap_bh402_w45_0_d2 <=  heap_bh402_w45_0_d1;
            heap_bh402_w46_0_d1 <=  heap_bh402_w46_0;
            heap_bh402_w46_0_d2 <=  heap_bh402_w46_0_d1;
            heap_bh402_w47_0_d1 <=  heap_bh402_w47_0;
            heap_bh402_w47_0_d2 <=  heap_bh402_w47_0_d1;
            heap_bh402_w47_0_d3 <=  heap_bh402_w47_0_d2;
            heap_bh402_w48_0_d1 <=  heap_bh402_w48_0;
            heap_bh402_w48_0_d2 <=  heap_bh402_w48_0_d1;
            heap_bh402_w48_0_d3 <=  heap_bh402_w48_0_d2;
            heap_bh402_w49_0_d1 <=  heap_bh402_w49_0;
            heap_bh402_w49_0_d2 <=  heap_bh402_w49_0_d1;
            heap_bh402_w49_0_d3 <=  heap_bh402_w49_0_d2;
            heap_bh402_w50_0_d1 <=  heap_bh402_w50_0;
            heap_bh402_w50_0_d2 <=  heap_bh402_w50_0_d1;
            heap_bh402_w50_0_d3 <=  heap_bh402_w50_0_d2;
            heap_bh402_w51_0_d1 <=  heap_bh402_w51_0;
            heap_bh402_w51_0_d2 <=  heap_bh402_w51_0_d1;
            heap_bh402_w51_0_d3 <=  heap_bh402_w51_0_d2;
            heap_bh402_w52_0_d1 <=  heap_bh402_w52_0;
            heap_bh402_w52_0_d2 <=  heap_bh402_w52_0_d1;
            heap_bh402_w52_0_d3 <=  heap_bh402_w52_0_d2;
            heap_bh402_w53_0_d1 <=  heap_bh402_w53_0;
            heap_bh402_w53_0_d2 <=  heap_bh402_w53_0_d1;
            heap_bh402_w53_0_d3 <=  heap_bh402_w53_0_d2;
            heap_bh402_w54_0_d1 <=  heap_bh402_w54_0;
            heap_bh402_w54_0_d2 <=  heap_bh402_w54_0_d1;
            heap_bh402_w54_0_d3 <=  heap_bh402_w54_0_d2;
            heap_bh402_w55_0_d1 <=  heap_bh402_w55_0;
            heap_bh402_w55_0_d2 <=  heap_bh402_w55_0_d1;
            heap_bh402_w55_0_d3 <=  heap_bh402_w55_0_d2;
            heap_bh402_w56_0_d1 <=  heap_bh402_w56_0;
            heap_bh402_w56_0_d2 <=  heap_bh402_w56_0_d1;
            heap_bh402_w56_0_d3 <=  heap_bh402_w56_0_d2;
            heap_bh402_w57_0_d1 <=  heap_bh402_w57_0;
            heap_bh402_w57_0_d2 <=  heap_bh402_w57_0_d1;
            heap_bh402_w57_0_d3 <=  heap_bh402_w57_0_d2;
            heap_bh402_w58_0_d1 <=  heap_bh402_w58_0;
            heap_bh402_w58_0_d2 <=  heap_bh402_w58_0_d1;
            heap_bh402_w58_0_d3 <=  heap_bh402_w58_0_d2;
            heap_bh402_w59_0_d1 <=  heap_bh402_w59_0;
            heap_bh402_w59_0_d2 <=  heap_bh402_w59_0_d1;
            heap_bh402_w59_0_d3 <=  heap_bh402_w59_0_d2;
            heap_bh402_w60_0_d1 <=  heap_bh402_w60_0;
            heap_bh402_w60_0_d2 <=  heap_bh402_w60_0_d1;
            heap_bh402_w60_0_d3 <=  heap_bh402_w60_0_d2;
            heap_bh402_w61_0_d1 <=  heap_bh402_w61_0;
            heap_bh402_w61_0_d2 <=  heap_bh402_w61_0_d1;
            heap_bh402_w61_0_d3 <=  heap_bh402_w61_0_d2;
            heap_bh402_w62_0_d1 <=  heap_bh402_w62_0;
            heap_bh402_w62_0_d2 <=  heap_bh402_w62_0_d1;
            heap_bh402_w62_0_d3 <=  heap_bh402_w62_0_d2;
            heap_bh402_w63_0_d1 <=  heap_bh402_w63_0;
            heap_bh402_w63_0_d2 <=  heap_bh402_w63_0_d1;
            heap_bh402_w63_0_d3 <=  heap_bh402_w63_0_d2;
            heap_bh402_w63_0_d4 <=  heap_bh402_w63_0_d3;
            heap_bh402_w64_0_d1 <=  heap_bh402_w64_0;
            heap_bh402_w64_0_d2 <=  heap_bh402_w64_0_d1;
            heap_bh402_w64_0_d3 <=  heap_bh402_w64_0_d2;
            heap_bh402_w64_0_d4 <=  heap_bh402_w64_0_d3;
            heap_bh402_w65_0_d1 <=  heap_bh402_w65_0;
            heap_bh402_w65_0_d2 <=  heap_bh402_w65_0_d1;
            heap_bh402_w65_0_d3 <=  heap_bh402_w65_0_d2;
            heap_bh402_w65_0_d4 <=  heap_bh402_w65_0_d3;
            heap_bh402_w31_1_d1 <=  heap_bh402_w31_1;
            heap_bh402_w30_1_d1 <=  heap_bh402_w30_1;
            heap_bh402_w30_1_d2 <=  heap_bh402_w30_1_d1;
            heap_bh402_w30_1_d3 <=  heap_bh402_w30_1_d2;
            heap_bh402_w29_1_d1 <=  heap_bh402_w29_1;
            heap_bh402_w29_1_d2 <=  heap_bh402_w29_1_d1;
            heap_bh402_w29_1_d3 <=  heap_bh402_w29_1_d2;
            heap_bh402_w28_1_d1 <=  heap_bh402_w28_1;
            heap_bh402_w28_1_d2 <=  heap_bh402_w28_1_d1;
            heap_bh402_w28_1_d3 <=  heap_bh402_w28_1_d2;
            heap_bh402_w27_1_d1 <=  heap_bh402_w27_1;
            heap_bh402_w27_1_d2 <=  heap_bh402_w27_1_d1;
            heap_bh402_w27_1_d3 <=  heap_bh402_w27_1_d2;
            heap_bh402_w26_1_d1 <=  heap_bh402_w26_1;
            heap_bh402_w26_1_d2 <=  heap_bh402_w26_1_d1;
            heap_bh402_w26_1_d3 <=  heap_bh402_w26_1_d2;
            heap_bh402_w25_1_d1 <=  heap_bh402_w25_1;
            heap_bh402_w25_1_d2 <=  heap_bh402_w25_1_d1;
            heap_bh402_w24_1_d1 <=  heap_bh402_w24_1;
            heap_bh402_w24_1_d2 <=  heap_bh402_w24_1_d1;
            heap_bh402_w23_1_d1 <=  heap_bh402_w23_1;
            heap_bh402_w23_1_d2 <=  heap_bh402_w23_1_d1;
            heap_bh402_w22_1_d1 <=  heap_bh402_w22_1;
            heap_bh402_w22_1_d2 <=  heap_bh402_w22_1_d1;
            heap_bh402_w21_1_d1 <=  heap_bh402_w21_1;
            heap_bh402_w21_1_d2 <=  heap_bh402_w21_1_d1;
            heap_bh402_w20_1_d1 <=  heap_bh402_w20_1;
            heap_bh402_w20_1_d2 <=  heap_bh402_w20_1_d1;
            heap_bh402_w19_1_d1 <=  heap_bh402_w19_1;
            heap_bh402_w19_1_d2 <=  heap_bh402_w19_1_d1;
            heap_bh402_w18_1_d1 <=  heap_bh402_w18_1;
            heap_bh402_w18_1_d2 <=  heap_bh402_w18_1_d1;
            heap_bh402_w17_1_d1 <=  heap_bh402_w17_1;
            heap_bh402_w16_1_d1 <=  heap_bh402_w16_1;
            heap_bh402_w15_1_d1 <=  heap_bh402_w15_1;
            heap_bh402_w14_1_d1 <=  heap_bh402_w14_1;
            heap_bh402_w13_1_d1 <=  heap_bh402_w13_1;
            heap_bh402_w12_2_d1 <=  heap_bh402_w12_2;
            heap_bh402_w11_2_d1 <=  heap_bh402_w11_2;
            heap_bh402_w10_2_d1 <=  heap_bh402_w10_2;
            heap_bh402_w9_4_d1 <=  heap_bh402_w9_4;
            heap_bh402_w8_4_d1 <=  heap_bh402_w8_4;
            heap_bh402_w7_4_d1 <=  heap_bh402_w7_4;
            heap_bh402_w6_5_d1 <=  heap_bh402_w6_5;
            heap_bh402_w5_5_d1 <=  heap_bh402_w5_5;
            heap_bh402_w4_5_d1 <=  heap_bh402_w4_5;
            heap_bh402_w3_7_d1 <=  heap_bh402_w3_7;
            heap_bh402_w2_7_d1 <=  heap_bh402_w2_7;
            heap_bh402_w1_7_d1 <=  heap_bh402_w1_7;
            heap_bh402_w0_9_d1 <=  heap_bh402_w0_9;
            DSP_bh402_ch3_0_d1 <=  DSP_bh402_ch3_0;
            DSP_bh402_root3_1_d1 <=  DSP_bh402_root3_1;
            DSP_bh402_ch3_1_d1 <=  DSP_bh402_ch3_1;
            DSP_bh402_root3_2_d1 <=  DSP_bh402_root3_2;
            heap_bh402_w12_3_d1 <=  heap_bh402_w12_3;
            heap_bh402_w11_3_d1 <=  heap_bh402_w11_3;
            heap_bh402_w10_3_d1 <=  heap_bh402_w10_3;
            heap_bh402_w9_5_d1 <=  heap_bh402_w9_5;
            heap_bh402_w8_5_d1 <=  heap_bh402_w8_5;
            heap_bh402_w7_5_d1 <=  heap_bh402_w7_5;
            heap_bh402_w6_6_d1 <=  heap_bh402_w6_6;
            heap_bh402_w5_6_d1 <=  heap_bh402_w5_6;
            heap_bh402_w5_6_d2 <=  heap_bh402_w5_6_d1;
            heap_bh402_w5_6_d3 <=  heap_bh402_w5_6_d2;
            heap_bh402_w5_6_d4 <=  heap_bh402_w5_6_d3;
            heap_bh402_w4_6_d1 <=  heap_bh402_w4_6;
            heap_bh402_w3_8_d1 <=  heap_bh402_w3_8;
            heap_bh402_w2_8_d1 <=  heap_bh402_w2_8;
            heap_bh402_w1_8_d1 <=  heap_bh402_w1_8;
            heap_bh402_w1_8_d2 <=  heap_bh402_w1_8_d1;
            heap_bh402_w1_8_d3 <=  heap_bh402_w1_8_d2;
            heap_bh402_w1_8_d4 <=  heap_bh402_w1_8_d3;
            heap_bh402_w0_10_d1 <=  heap_bh402_w0_10;
            heap_bh402_w0_10_d2 <=  heap_bh402_w0_10_d1;
            heap_bh402_w0_10_d3 <=  heap_bh402_w0_10_d2;
            heap_bh402_w0_10_d4 <=  heap_bh402_w0_10_d3;
            heap_bh402_w57_1_d1 <=  heap_bh402_w57_1;
            heap_bh402_w56_1_d1 <=  heap_bh402_w56_1;
            heap_bh402_w56_1_d2 <=  heap_bh402_w56_1_d1;
            heap_bh402_w55_1_d1 <=  heap_bh402_w55_1;
            heap_bh402_w54_1_d1 <=  heap_bh402_w54_1;
            heap_bh402_w54_1_d2 <=  heap_bh402_w54_1_d1;
            heap_bh402_w53_1_d1 <=  heap_bh402_w53_1;
            heap_bh402_w52_1_d1 <=  heap_bh402_w52_1;
            heap_bh402_w51_1_d1 <=  heap_bh402_w51_1;
            heap_bh402_w50_1_d1 <=  heap_bh402_w50_1;
            heap_bh402_w49_1_d1 <=  heap_bh402_w49_1;
            heap_bh402_w48_1_d1 <=  heap_bh402_w48_1;
            heap_bh402_w47_1_d1 <=  heap_bh402_w47_1;
            heap_bh402_w46_1_d1 <=  heap_bh402_w46_1;
            heap_bh402_w46_1_d2 <=  heap_bh402_w46_1_d1;
            heap_bh402_w46_1_d3 <=  heap_bh402_w46_1_d2;
            heap_bh402_w45_1_d1 <=  heap_bh402_w45_1;
            heap_bh402_w45_1_d2 <=  heap_bh402_w45_1_d1;
            heap_bh402_w44_1_d1 <=  heap_bh402_w44_1;
            heap_bh402_w44_1_d2 <=  heap_bh402_w44_1_d1;
            heap_bh402_w43_1_d1 <=  heap_bh402_w43_1;
            heap_bh402_w43_1_d2 <=  heap_bh402_w43_1_d1;
            heap_bh402_w42_1_d1 <=  heap_bh402_w42_1;
            heap_bh402_w42_1_d2 <=  heap_bh402_w42_1_d1;
            heap_bh402_w41_1_d1 <=  heap_bh402_w41_1;
            heap_bh402_w41_1_d2 <=  heap_bh402_w41_1_d1;
            heap_bh402_w40_1_d1 <=  heap_bh402_w40_1;
            heap_bh402_w40_1_d2 <=  heap_bh402_w40_1_d1;
            heap_bh402_w39_1_d1 <=  heap_bh402_w39_1;
            heap_bh402_w39_1_d2 <=  heap_bh402_w39_1_d1;
            heap_bh402_w38_1_d1 <=  heap_bh402_w38_1;
            heap_bh402_w38_1_d2 <=  heap_bh402_w38_1_d1;
            heap_bh402_w37_1_d1 <=  heap_bh402_w37_1;
            heap_bh402_w36_1_d1 <=  heap_bh402_w36_1;
            heap_bh402_w35_1_d1 <=  heap_bh402_w35_1;
            heap_bh402_w34_1_d1 <=  heap_bh402_w34_1;
            heap_bh402_w33_1_d1 <=  heap_bh402_w33_1;
            heap_bh402_w32_1_d1 <=  heap_bh402_w32_1;
            heap_bh402_w31_2_d1 <=  heap_bh402_w31_2;
            heap_bh402_w30_2_d1 <=  heap_bh402_w30_2;
            heap_bh402_w29_2_d1 <=  heap_bh402_w29_2;
            heap_bh402_w28_2_d1 <=  heap_bh402_w28_2;
            heap_bh402_w27_2_d1 <=  heap_bh402_w27_2;
            heap_bh402_w27_2_d2 <=  heap_bh402_w27_2_d1;
            heap_bh402_w26_2_d1 <=  heap_bh402_w26_2;
            heap_bh402_w26_2_d2 <=  heap_bh402_w26_2_d1;
            heap_bh402_w25_2_d1 <=  heap_bh402_w25_2;
            heap_bh402_w25_2_d2 <=  heap_bh402_w25_2_d1;
            heap_bh402_w24_2_d1 <=  heap_bh402_w24_2;
            heap_bh402_w24_2_d2 <=  heap_bh402_w24_2_d1;
            heap_bh402_w23_2_d1 <=  heap_bh402_w23_2;
            heap_bh402_w23_2_d2 <=  heap_bh402_w23_2_d1;
            heap_bh402_w22_2_d1 <=  heap_bh402_w22_2;
            heap_bh402_w21_2_d1 <=  heap_bh402_w21_2;
            heap_bh402_w20_2_d1 <=  heap_bh402_w20_2;
            heap_bh402_w19_2_d1 <=  heap_bh402_w19_2;
            heap_bh402_w18_2_d1 <=  heap_bh402_w18_2;
            heap_bh402_w17_2_d1 <=  heap_bh402_w17_2;
            heap_bh402_w16_2_d1 <=  heap_bh402_w16_2;
            heap_bh402_w15_2_d1 <=  heap_bh402_w15_2;
            heap_bh402_w14_2_d1 <=  heap_bh402_w14_2;
            heap_bh402_w13_2_d1 <=  heap_bh402_w13_2;
            heap_bh402_w31_3_d1 <=  heap_bh402_w31_3;
            heap_bh402_w32_2_d1 <=  heap_bh402_w32_2;
            heap_bh402_w33_2_d1 <=  heap_bh402_w33_2;
            heap_bh402_w34_2_d1 <=  heap_bh402_w34_2;
            heap_bh402_w35_2_d1 <=  heap_bh402_w35_2;
            heap_bh402_w36_2_d1 <=  heap_bh402_w36_2;
            heap_bh402_w37_2_d1 <=  heap_bh402_w37_2;
            heap_bh402_w38_2_d1 <=  heap_bh402_w38_2;
            heap_bh402_w39_2_d1 <=  heap_bh402_w39_2;
            heap_bh402_w39_2_d2 <=  heap_bh402_w39_2_d1;
            heap_bh402_w40_2_d1 <=  heap_bh402_w40_2;
            heap_bh402_w40_2_d2 <=  heap_bh402_w40_2_d1;
            heap_bh402_w41_2_d1 <=  heap_bh402_w41_2;
            heap_bh402_w41_2_d2 <=  heap_bh402_w41_2_d1;
            heap_bh402_w42_2_d1 <=  heap_bh402_w42_2;
            heap_bh402_w42_2_d2 <=  heap_bh402_w42_2_d1;
            heap_bh402_w43_2_d1 <=  heap_bh402_w43_2;
            heap_bh402_w43_2_d2 <=  heap_bh402_w43_2_d1;
            heap_bh402_w44_2_d1 <=  heap_bh402_w44_2;
            heap_bh402_w44_2_d2 <=  heap_bh402_w44_2_d1;
            heap_bh402_w45_2_d1 <=  heap_bh402_w45_2;
            heap_bh402_w45_2_d2 <=  heap_bh402_w45_2_d1;
            heap_bh402_w46_2_d1 <=  heap_bh402_w46_2;
            heap_bh402_w46_2_d2 <=  heap_bh402_w46_2_d1;
            heap_bh402_w47_2_d1 <=  heap_bh402_w47_2;
            heap_bh402_w47_2_d2 <=  heap_bh402_w47_2_d1;
            heap_bh402_w47_2_d3 <=  heap_bh402_w47_2_d2;
            heap_bh402_w48_2_d1 <=  heap_bh402_w48_2;
            heap_bh402_w48_2_d2 <=  heap_bh402_w48_2_d1;
            heap_bh402_w48_2_d3 <=  heap_bh402_w48_2_d2;
            heap_bh402_w49_2_d1 <=  heap_bh402_w49_2;
            heap_bh402_w49_2_d2 <=  heap_bh402_w49_2_d1;
            heap_bh402_w49_2_d3 <=  heap_bh402_w49_2_d2;
            heap_bh402_w50_2_d1 <=  heap_bh402_w50_2;
            heap_bh402_w50_2_d2 <=  heap_bh402_w50_2_d1;
            heap_bh402_w50_2_d3 <=  heap_bh402_w50_2_d2;
            heap_bh402_w51_2_d1 <=  heap_bh402_w51_2;
            heap_bh402_w51_2_d2 <=  heap_bh402_w51_2_d1;
            heap_bh402_w51_2_d3 <=  heap_bh402_w51_2_d2;
            heap_bh402_w52_2_d1 <=  heap_bh402_w52_2;
            heap_bh402_w52_2_d2 <=  heap_bh402_w52_2_d1;
            heap_bh402_w52_2_d3 <=  heap_bh402_w52_2_d2;
            heap_bh402_w53_2_d1 <=  heap_bh402_w53_2;
            heap_bh402_w53_2_d2 <=  heap_bh402_w53_2_d1;
            heap_bh402_w53_2_d3 <=  heap_bh402_w53_2_d2;
            heap_bh402_w54_2_d1 <=  heap_bh402_w54_2;
            heap_bh402_w54_2_d2 <=  heap_bh402_w54_2_d1;
            heap_bh402_w54_2_d3 <=  heap_bh402_w54_2_d2;
            heap_bh402_w55_2_d1 <=  heap_bh402_w55_2;
            heap_bh402_w55_2_d2 <=  heap_bh402_w55_2_d1;
            heap_bh402_w55_2_d3 <=  heap_bh402_w55_2_d2;
            heap_bh402_w56_2_d1 <=  heap_bh402_w56_2;
            heap_bh402_w56_2_d2 <=  heap_bh402_w56_2_d1;
            heap_bh402_w56_2_d3 <=  heap_bh402_w56_2_d2;
            heap_bh402_w58_1_d1 <=  heap_bh402_w58_1;
            heap_bh402_w58_1_d2 <=  heap_bh402_w58_1_d1;
            heap_bh402_w58_1_d3 <=  heap_bh402_w58_1_d2;
            heap_bh402_w59_1_d1 <=  heap_bh402_w59_1;
            heap_bh402_w59_1_d2 <=  heap_bh402_w59_1_d1;
            heap_bh402_w59_1_d3 <=  heap_bh402_w59_1_d2;
            heap_bh402_w60_1_d1 <=  heap_bh402_w60_1;
            heap_bh402_w60_1_d2 <=  heap_bh402_w60_1_d1;
            heap_bh402_w60_1_d3 <=  heap_bh402_w60_1_d2;
            heap_bh402_w61_1_d1 <=  heap_bh402_w61_1;
            heap_bh402_w61_1_d2 <=  heap_bh402_w61_1_d1;
            heap_bh402_w61_1_d3 <=  heap_bh402_w61_1_d2;
            heap_bh402_w62_1_d1 <=  heap_bh402_w62_1;
            heap_bh402_w62_1_d2 <=  heap_bh402_w62_1_d1;
            heap_bh402_w62_1_d3 <=  heap_bh402_w62_1_d2;
            heap_bh402_w63_1_d1 <=  heap_bh402_w63_1;
            heap_bh402_w63_1_d2 <=  heap_bh402_w63_1_d1;
            heap_bh402_w63_1_d3 <=  heap_bh402_w63_1_d2;
            heap_bh402_w63_1_d4 <=  heap_bh402_w63_1_d3;
            heap_bh402_w64_1_d1 <=  heap_bh402_w64_1;
            heap_bh402_w64_1_d2 <=  heap_bh402_w64_1_d1;
            heap_bh402_w64_1_d3 <=  heap_bh402_w64_1_d2;
            heap_bh402_w64_1_d4 <=  heap_bh402_w64_1_d3;
            heap_bh402_w65_1_d1 <=  heap_bh402_w65_1;
            heap_bh402_w65_1_d2 <=  heap_bh402_w65_1_d1;
            heap_bh402_w65_1_d3 <=  heap_bh402_w65_1_d2;
            heap_bh402_w65_1_d4 <=  heap_bh402_w65_1_d3;
            heap_bh402_w0_11_d1 <=  heap_bh402_w0_11;
            heap_bh402_w5_7_d1 <=  heap_bh402_w5_7;
            heap_bh402_w7_6_d1 <=  heap_bh402_w7_6;
            heap_bh402_w5_8_d1 <=  heap_bh402_w5_8;
            heap_bh402_w9_6_d1 <=  heap_bh402_w9_6;
            heap_bh402_w10_4_d1 <=  heap_bh402_w10_4;
            heap_bh402_w0_12_d1 <=  heap_bh402_w0_12;
            heap_bh402_w2_13_d1 <=  heap_bh402_w2_13;
            heap_bh402_w3_12_d1 <=  heap_bh402_w3_12;
            heap_bh402_w4_10_d1 <=  heap_bh402_w4_10;
            heap_bh402_w4_11_d1 <=  heap_bh402_w4_11;
            heap_bh402_w5_10_d1 <=  heap_bh402_w5_10;
            heap_bh402_w6_11_d1 <=  heap_bh402_w6_11;
            heap_bh402_w6_12_d1 <=  heap_bh402_w6_12;
            heap_bh402_w7_9_d1 <=  heap_bh402_w7_9;
            heap_bh402_w8_9_d1 <=  heap_bh402_w8_9;
            heap_bh402_w8_10_d1 <=  heap_bh402_w8_10;
            heap_bh402_w9_9_d1 <=  heap_bh402_w9_9;
            heap_bh402_w10_5_d1 <=  heap_bh402_w10_5;
            heap_bh402_w11_5_d1 <=  heap_bh402_w11_5;
            heap_bh402_w12_4_d1 <=  heap_bh402_w12_4;
            heap_bh402_w13_3_d1 <=  heap_bh402_w13_3;
            heap_bh402_w1_12_d1 <=  heap_bh402_w1_12;
            heap_bh402_w2_14_d1 <=  heap_bh402_w2_14;
            heap_bh402_w3_13_d1 <=  heap_bh402_w3_13;
            heap_bh402_w3_14_d1 <=  heap_bh402_w3_14;
            heap_bh402_w4_12_d1 <=  heap_bh402_w4_12;
            heap_bh402_w5_11_d1 <=  heap_bh402_w5_11;
            heap_bh402_w5_11_d2 <=  heap_bh402_w5_11_d1;
            heap_bh402_w0_13_d1 <=  heap_bh402_w0_13;
            heap_bh402_w0_13_d2 <=  heap_bh402_w0_13_d1;
            heap_bh402_w0_13_d3 <=  heap_bh402_w0_13_d2;
            heap_bh402_w0_13_d4 <=  heap_bh402_w0_13_d3;
            heap_bh402_w0_13_d5 <=  heap_bh402_w0_13_d4;
            heap_bh402_w1_13_d1 <=  heap_bh402_w1_13;
            heap_bh402_w1_13_d2 <=  heap_bh402_w1_13_d1;
            heap_bh402_w1_13_d3 <=  heap_bh402_w1_13_d2;
            heap_bh402_w1_13_d4 <=  heap_bh402_w1_13_d3;
            heap_bh402_w1_13_d5 <=  heap_bh402_w1_13_d4;
            heap_bh402_w2_15_d1 <=  heap_bh402_w2_15;
            heap_bh402_w2_15_d2 <=  heap_bh402_w2_15_d1;
            heap_bh402_w2_16_d1 <=  heap_bh402_w2_16;
            heap_bh402_w2_16_d2 <=  heap_bh402_w2_16_d1;
            heap_bh402_w3_16_d1 <=  heap_bh402_w3_16;
            heap_bh402_w3_16_d2 <=  heap_bh402_w3_16_d1;
            heap_bh402_w4_14_d1 <=  heap_bh402_w4_14;
            heap_bh402_w4_14_d2 <=  heap_bh402_w4_14_d1;
            heap_bh402_w4_15_d1 <=  heap_bh402_w4_15;
            heap_bh402_w4_15_d2 <=  heap_bh402_w4_15_d1;
            heap_bh402_w5_13_d1 <=  heap_bh402_w5_13;
            heap_bh402_w5_13_d2 <=  heap_bh402_w5_13_d1;
            heap_bh402_w10_7_d1 <=  heap_bh402_w10_7;
            heap_bh402_w10_7_d2 <=  heap_bh402_w10_7_d1;
            heap_bh402_w31_4_d1 <=  heap_bh402_w31_4;
            heap_bh402_w31_4_d2 <=  heap_bh402_w31_4_d1;
            heap_bh402_w32_3_d1 <=  heap_bh402_w32_3;
            heap_bh402_w32_3_d2 <=  heap_bh402_w32_3_d1;
            heap_bh402_w8_12_d1 <=  heap_bh402_w8_12;
            heap_bh402_w8_12_d2 <=  heap_bh402_w8_12_d1;
            heap_bh402_w9_12_d1 <=  heap_bh402_w9_12;
            heap_bh402_w9_12_d2 <=  heap_bh402_w9_12_d1;
            heap_bh402_w10_8_d1 <=  heap_bh402_w10_8;
            heap_bh402_w10_8_d2 <=  heap_bh402_w10_8_d1;
            heap_bh402_w15_3_d1 <=  heap_bh402_w15_3;
            heap_bh402_w15_3_d2 <=  heap_bh402_w15_3_d1;
            heap_bh402_w33_4_d1 <=  heap_bh402_w33_4;
            heap_bh402_w33_4_d2 <=  heap_bh402_w33_4_d1;
            heap_bh402_w34_3_d1 <=  heap_bh402_w34_3;
            heap_bh402_w34_3_d2 <=  heap_bh402_w34_3_d1;
            heap_bh402_w6_15_d1 <=  heap_bh402_w6_15;
            heap_bh402_w6_15_d2 <=  heap_bh402_w6_15_d1;
            heap_bh402_w7_12_d1 <=  heap_bh402_w7_12;
            heap_bh402_w7_12_d2 <=  heap_bh402_w7_12_d1;
            heap_bh402_w8_13_d1 <=  heap_bh402_w8_13;
            heap_bh402_w8_13_d2 <=  heap_bh402_w8_13_d1;
            heap_bh402_w11_8_d1 <=  heap_bh402_w11_8;
            heap_bh402_w11_8_d2 <=  heap_bh402_w11_8_d1;
            heap_bh402_w12_7_d1 <=  heap_bh402_w12_7;
            heap_bh402_w12_7_d2 <=  heap_bh402_w12_7_d1;
            heap_bh402_w16_4_d1 <=  heap_bh402_w16_4;
            heap_bh402_w16_4_d2 <=  heap_bh402_w16_4_d1;
            heap_bh402_w17_3_d1 <=  heap_bh402_w17_3;
            heap_bh402_w17_3_d2 <=  heap_bh402_w17_3_d1;
            heap_bh402_w18_3_d1 <=  heap_bh402_w18_3;
            heap_bh402_w35_4_d1 <=  heap_bh402_w35_4;
            heap_bh402_w35_4_d2 <=  heap_bh402_w35_4_d1;
            heap_bh402_w36_3_d1 <=  heap_bh402_w36_3;
            heap_bh402_w36_3_d2 <=  heap_bh402_w36_3_d1;
            heap_bh402_w13_6_d1 <=  heap_bh402_w13_6;
            heap_bh402_w13_6_d2 <=  heap_bh402_w13_6_d1;
            heap_bh402_w14_5_d1 <=  heap_bh402_w14_5;
            heap_bh402_w14_5_d2 <=  heap_bh402_w14_5_d1;
            heap_bh402_w15_4_d1 <=  heap_bh402_w15_4;
            heap_bh402_w15_4_d2 <=  heap_bh402_w15_4_d1;
            heap_bh402_w18_4_d1 <=  heap_bh402_w18_4;
            heap_bh402_w19_3_d1 <=  heap_bh402_w19_3;
            heap_bh402_w37_4_d1 <=  heap_bh402_w37_4;
            heap_bh402_w37_4_d2 <=  heap_bh402_w37_4_d1;
            heap_bh402_w38_3_d1 <=  heap_bh402_w38_3;
            heap_bh402_w38_3_d2 <=  heap_bh402_w38_3_d1;
            heap_bh402_w38_3_d3 <=  heap_bh402_w38_3_d2;
            heap_bh402_w39_3_d1 <=  heap_bh402_w39_3;
            heap_bh402_w20_4_d1 <=  heap_bh402_w20_4;
            heap_bh402_w21_3_d1 <=  heap_bh402_w21_3;
            heap_bh402_w39_4_d1 <=  heap_bh402_w39_4;
            heap_bh402_w39_4_d2 <=  heap_bh402_w39_4_d1;
            heap_bh402_w40_3_d1 <=  heap_bh402_w40_3;
            heap_bh402_w40_3_d2 <=  heap_bh402_w40_3_d1;
            heap_bh402_w22_4_d1 <=  heap_bh402_w22_4;
            heap_bh402_w23_3_d1 <=  heap_bh402_w23_3;
            heap_bh402_w23_3_d2 <=  heap_bh402_w23_3_d1;
            heap_bh402_w41_4_d1 <=  heap_bh402_w41_4;
            heap_bh402_w41_4_d2 <=  heap_bh402_w41_4_d1;
            heap_bh402_w42_3_d1 <=  heap_bh402_w42_3;
            heap_bh402_w42_3_d2 <=  heap_bh402_w42_3_d1;
            heap_bh402_w24_4_d1 <=  heap_bh402_w24_4;
            heap_bh402_w24_4_d2 <=  heap_bh402_w24_4_d1;
            heap_bh402_w25_3_d1 <=  heap_bh402_w25_3;
            heap_bh402_w25_3_d2 <=  heap_bh402_w25_3_d1;
            heap_bh402_w26_3_d1 <=  heap_bh402_w26_3;
            heap_bh402_w43_4_d1 <=  heap_bh402_w43_4;
            heap_bh402_w43_4_d2 <=  heap_bh402_w43_4_d1;
            heap_bh402_w44_3_d1 <=  heap_bh402_w44_3;
            heap_bh402_w44_3_d2 <=  heap_bh402_w44_3_d1;
            heap_bh402_w26_4_d1 <=  heap_bh402_w26_4;
            heap_bh402_w27_3_d1 <=  heap_bh402_w27_3;
            heap_bh402_w45_4_d1 <=  heap_bh402_w45_4;
            heap_bh402_w45_4_d2 <=  heap_bh402_w45_4_d1;
            heap_bh402_w46_3_d1 <=  heap_bh402_w46_3;
            heap_bh402_w46_3_d2 <=  heap_bh402_w46_3_d1;
            heap_bh402_w46_3_d3 <=  heap_bh402_w46_3_d2;
            heap_bh402_w47_3_d1 <=  heap_bh402_w47_3;
            heap_bh402_w28_4_d1 <=  heap_bh402_w28_4;
            heap_bh402_w47_4_d1 <=  heap_bh402_w47_4;
            heap_bh402_w47_4_d2 <=  heap_bh402_w47_4_d1;
            heap_bh402_w2_17_d1 <=  heap_bh402_w2_17;
            heap_bh402_w2_17_d2 <=  heap_bh402_w2_17_d1;
            heap_bh402_w2_17_d3 <=  heap_bh402_w2_17_d2;
            heap_bh402_w3_17_d1 <=  heap_bh402_w3_17;
            heap_bh402_w3_17_d2 <=  heap_bh402_w3_17_d1;
            heap_bh402_w3_17_d3 <=  heap_bh402_w3_17_d2;
            heap_bh402_w4_16_d1 <=  heap_bh402_w4_16;
            heap_bh402_w4_16_d2 <=  heap_bh402_w4_16_d1;
            heap_bh402_w4_16_d3 <=  heap_bh402_w4_16_d2;
            heap_bh402_w4_17_d1 <=  heap_bh402_w4_17;
            heap_bh402_w4_17_d2 <=  heap_bh402_w4_17_d1;
            heap_bh402_w4_17_d3 <=  heap_bh402_w4_17_d2;
            heap_bh402_w5_14_d1 <=  heap_bh402_w5_14;
            heap_bh402_w5_14_d2 <=  heap_bh402_w5_14_d1;
            heap_bh402_w5_14_d3 <=  heap_bh402_w5_14_d2;
            heap_bh402_w8_14_d1 <=  heap_bh402_w8_14;
            heap_bh402_w8_14_d2 <=  heap_bh402_w8_14_d1;
            heap_bh402_w8_14_d3 <=  heap_bh402_w8_14_d2;
            heap_bh402_w9_13_d1 <=  heap_bh402_w9_13;
            heap_bh402_w9_13_d2 <=  heap_bh402_w9_13_d1;
            heap_bh402_w9_13_d3 <=  heap_bh402_w9_13_d2;
            heap_bh402_w10_9_d1 <=  heap_bh402_w10_9;
            heap_bh402_w10_9_d2 <=  heap_bh402_w10_9_d1;
            heap_bh402_w10_9_d3 <=  heap_bh402_w10_9_d2;
            heap_bh402_w10_10_d1 <=  heap_bh402_w10_10;
            heap_bh402_w10_10_d2 <=  heap_bh402_w10_10_d1;
            heap_bh402_w10_10_d3 <=  heap_bh402_w10_10_d2;
            heap_bh402_w11_9_d1 <=  heap_bh402_w11_9;
            heap_bh402_w11_9_d2 <=  heap_bh402_w11_9_d1;
            heap_bh402_w11_9_d3 <=  heap_bh402_w11_9_d2;
            heap_bh402_w16_5_d1 <=  heap_bh402_w16_5;
            heap_bh402_w16_5_d2 <=  heap_bh402_w16_5_d1;
            heap_bh402_w16_5_d3 <=  heap_bh402_w16_5_d2;
            heap_bh402_w31_5_d1 <=  heap_bh402_w31_5;
            heap_bh402_w31_5_d2 <=  heap_bh402_w31_5_d1;
            heap_bh402_w31_5_d3 <=  heap_bh402_w31_5_d2;
            heap_bh402_w54_3_d1 <=  heap_bh402_w54_3;
            heap_bh402_w55_3_d1 <=  heap_bh402_w55_3;
            heap_bh402_w55_4_d1 <=  heap_bh402_w55_4;
            heap_bh402_w56_3_d1 <=  heap_bh402_w56_3;
            heap_bh402_w6_17_d1 <=  heap_bh402_w6_17;
            heap_bh402_w6_17_d2 <=  heap_bh402_w6_17_d1;
            heap_bh402_w6_17_d3 <=  heap_bh402_w6_17_d2;
            heap_bh402_w7_13_d1 <=  heap_bh402_w7_13;
            heap_bh402_w7_13_d2 <=  heap_bh402_w7_13_d1;
            heap_bh402_w7_13_d3 <=  heap_bh402_w7_13_d2;
            heap_bh402_w8_15_d1 <=  heap_bh402_w8_15;
            heap_bh402_w8_15_d2 <=  heap_bh402_w8_15_d1;
            heap_bh402_w8_15_d3 <=  heap_bh402_w8_15_d2;
            heap_bh402_w12_9_d1 <=  heap_bh402_w12_9;
            heap_bh402_w12_9_d2 <=  heap_bh402_w12_9_d1;
            heap_bh402_w12_9_d3 <=  heap_bh402_w12_9_d2;
            heap_bh402_w13_7_d1 <=  heap_bh402_w13_7;
            heap_bh402_w13_7_d2 <=  heap_bh402_w13_7_d1;
            heap_bh402_w13_7_d3 <=  heap_bh402_w13_7_d2;
            heap_bh402_w17_5_d1 <=  heap_bh402_w17_5;
            heap_bh402_w17_5_d2 <=  heap_bh402_w17_5_d1;
            heap_bh402_w17_5_d3 <=  heap_bh402_w17_5_d2;
            heap_bh402_w18_5_d1 <=  heap_bh402_w18_5;
            heap_bh402_w18_5_d2 <=  heap_bh402_w18_5_d1;
            heap_bh402_w18_5_d3 <=  heap_bh402_w18_5_d2;
            heap_bh402_w29_4_d1 <=  heap_bh402_w29_4;
            heap_bh402_w29_4_d2 <=  heap_bh402_w29_4_d1;
            heap_bh402_w29_4_d3 <=  heap_bh402_w29_4_d2;
            heap_bh402_w30_5_d1 <=  heap_bh402_w30_5;
            heap_bh402_w30_5_d2 <=  heap_bh402_w30_5_d1;
            heap_bh402_w30_5_d3 <=  heap_bh402_w30_5_d2;
            heap_bh402_w31_6_d1 <=  heap_bh402_w31_6;
            heap_bh402_w31_6_d2 <=  heap_bh402_w31_6_d1;
            heap_bh402_w31_6_d3 <=  heap_bh402_w31_6_d2;
            heap_bh402_w32_5_d1 <=  heap_bh402_w32_5;
            heap_bh402_w32_5_d2 <=  heap_bh402_w32_5_d1;
            heap_bh402_w32_5_d3 <=  heap_bh402_w32_5_d2;
            heap_bh402_w33_5_d1 <=  heap_bh402_w33_5;
            heap_bh402_w33_5_d2 <=  heap_bh402_w33_5_d1;
            heap_bh402_w33_5_d3 <=  heap_bh402_w33_5_d2;
            heap_bh402_w48_4_d1 <=  heap_bh402_w48_4;
            heap_bh402_w48_4_d2 <=  heap_bh402_w48_4_d1;
            heap_bh402_w48_4_d3 <=  heap_bh402_w48_4_d2;
            heap_bh402_w49_5_d1 <=  heap_bh402_w49_5;
            heap_bh402_w49_5_d2 <=  heap_bh402_w49_5_d1;
            heap_bh402_w49_5_d3 <=  heap_bh402_w49_5_d2;
            heap_bh402_w57_3_d1 <=  heap_bh402_w57_3;
            heap_bh402_w58_2_d1 <=  heap_bh402_w58_2;
            heap_bh402_w58_2_d2 <=  heap_bh402_w58_2_d1;
            heap_bh402_w58_2_d3 <=  heap_bh402_w58_2_d2;
            heap_bh402_w19_5_d1 <=  heap_bh402_w19_5;
            heap_bh402_w19_5_d2 <=  heap_bh402_w19_5_d1;
            heap_bh402_w19_5_d3 <=  heap_bh402_w19_5_d2;
            heap_bh402_w20_5_d1 <=  heap_bh402_w20_5;
            heap_bh402_w20_5_d2 <=  heap_bh402_w20_5_d1;
            heap_bh402_w20_5_d3 <=  heap_bh402_w20_5_d2;
            heap_bh402_w34_5_d1 <=  heap_bh402_w34_5;
            heap_bh402_w34_5_d2 <=  heap_bh402_w34_5_d1;
            heap_bh402_w34_5_d3 <=  heap_bh402_w34_5_d2;
            heap_bh402_w35_5_d1 <=  heap_bh402_w35_5;
            heap_bh402_w35_5_d2 <=  heap_bh402_w35_5_d1;
            heap_bh402_w35_5_d3 <=  heap_bh402_w35_5_d2;
            heap_bh402_w50_5_d1 <=  heap_bh402_w50_5;
            heap_bh402_w50_5_d2 <=  heap_bh402_w50_5_d1;
            heap_bh402_w50_5_d3 <=  heap_bh402_w50_5_d2;
            heap_bh402_w51_5_d1 <=  heap_bh402_w51_5;
            heap_bh402_w51_5_d2 <=  heap_bh402_w51_5_d1;
            heap_bh402_w51_5_d3 <=  heap_bh402_w51_5_d2;
            heap_bh402_w59_3_d1 <=  heap_bh402_w59_3;
            heap_bh402_w59_3_d2 <=  heap_bh402_w59_3_d1;
            heap_bh402_w59_3_d3 <=  heap_bh402_w59_3_d2;
            heap_bh402_w60_2_d1 <=  heap_bh402_w60_2;
            heap_bh402_w60_2_d2 <=  heap_bh402_w60_2_d1;
            heap_bh402_w60_2_d3 <=  heap_bh402_w60_2_d2;
            heap_bh402_w14_7_d1 <=  heap_bh402_w14_7;
            heap_bh402_w14_7_d2 <=  heap_bh402_w14_7_d1;
            heap_bh402_w14_7_d3 <=  heap_bh402_w14_7_d2;
            heap_bh402_w15_6_d1 <=  heap_bh402_w15_6;
            heap_bh402_w15_6_d2 <=  heap_bh402_w15_6_d1;
            heap_bh402_w15_6_d3 <=  heap_bh402_w15_6_d2;
            heap_bh402_w16_6_d1 <=  heap_bh402_w16_6;
            heap_bh402_w16_6_d2 <=  heap_bh402_w16_6_d1;
            heap_bh402_w16_6_d3 <=  heap_bh402_w16_6_d2;
            heap_bh402_w21_5_d1 <=  heap_bh402_w21_5;
            heap_bh402_w21_5_d2 <=  heap_bh402_w21_5_d1;
            heap_bh402_w21_5_d3 <=  heap_bh402_w21_5_d2;
            heap_bh402_w22_5_d1 <=  heap_bh402_w22_5;
            heap_bh402_w22_5_d2 <=  heap_bh402_w22_5_d1;
            heap_bh402_w22_5_d3 <=  heap_bh402_w22_5_d2;
            heap_bh402_w23_4_d1 <=  heap_bh402_w23_4;
            heap_bh402_w36_5_d1 <=  heap_bh402_w36_5;
            heap_bh402_w36_5_d2 <=  heap_bh402_w36_5_d1;
            heap_bh402_w36_5_d3 <=  heap_bh402_w36_5_d2;
            heap_bh402_w37_5_d1 <=  heap_bh402_w37_5;
            heap_bh402_w37_5_d2 <=  heap_bh402_w37_5_d1;
            heap_bh402_w37_5_d3 <=  heap_bh402_w37_5_d2;
            heap_bh402_w38_4_d1 <=  heap_bh402_w38_4;
            heap_bh402_w52_5_d1 <=  heap_bh402_w52_5;
            heap_bh402_w52_5_d2 <=  heap_bh402_w52_5_d1;
            heap_bh402_w52_5_d3 <=  heap_bh402_w52_5_d2;
            heap_bh402_w53_5_d1 <=  heap_bh402_w53_5;
            heap_bh402_w53_5_d2 <=  heap_bh402_w53_5_d1;
            heap_bh402_w53_5_d3 <=  heap_bh402_w53_5_d2;
            heap_bh402_w54_4_d1 <=  heap_bh402_w54_4;
            heap_bh402_w61_3_d1 <=  heap_bh402_w61_3;
            heap_bh402_w61_3_d2 <=  heap_bh402_w61_3_d1;
            heap_bh402_w61_3_d3 <=  heap_bh402_w61_3_d2;
            heap_bh402_w62_2_d1 <=  heap_bh402_w62_2;
            heap_bh402_w62_2_d2 <=  heap_bh402_w62_2_d1;
            heap_bh402_w62_2_d3 <=  heap_bh402_w62_2_d2;
            heap_bh402_w63_2_d1 <=  heap_bh402_w63_2;
            heap_bh402_w23_5_d1 <=  heap_bh402_w23_5;
            heap_bh402_w23_5_d2 <=  heap_bh402_w23_5_d1;
            heap_bh402_w24_5_d1 <=  heap_bh402_w24_5;
            heap_bh402_w24_5_d2 <=  heap_bh402_w24_5_d1;
            heap_bh402_w38_5_d1 <=  heap_bh402_w38_5;
            heap_bh402_w38_5_d2 <=  heap_bh402_w38_5_d1;
            heap_bh402_w39_5_d1 <=  heap_bh402_w39_5;
            heap_bh402_w39_5_d2 <=  heap_bh402_w39_5_d1;
            heap_bh402_w54_5_d1 <=  heap_bh402_w54_5;
            heap_bh402_w54_5_d2 <=  heap_bh402_w54_5_d1;
            heap_bh402_w55_5_d1 <=  heap_bh402_w55_5;
            heap_bh402_w55_5_d2 <=  heap_bh402_w55_5_d1;
            heap_bh402_w63_3_d1 <=  heap_bh402_w63_3;
            heap_bh402_w63_3_d2 <=  heap_bh402_w63_3_d1;
            heap_bh402_w64_2_d1 <=  heap_bh402_w64_2;
            heap_bh402_w64_2_d2 <=  heap_bh402_w64_2_d1;
            heap_bh402_w25_5_d1 <=  heap_bh402_w25_5;
            heap_bh402_w25_5_d2 <=  heap_bh402_w25_5_d1;
            heap_bh402_w26_5_d1 <=  heap_bh402_w26_5;
            heap_bh402_w26_5_d2 <=  heap_bh402_w26_5_d1;
            heap_bh402_w40_5_d1 <=  heap_bh402_w40_5;
            heap_bh402_w40_5_d2 <=  heap_bh402_w40_5_d1;
            heap_bh402_w41_5_d1 <=  heap_bh402_w41_5;
            heap_bh402_w41_5_d2 <=  heap_bh402_w41_5_d1;
            heap_bh402_w56_5_d1 <=  heap_bh402_w56_5;
            heap_bh402_w56_5_d2 <=  heap_bh402_w56_5_d1;
            heap_bh402_w57_4_d1 <=  heap_bh402_w57_4;
            heap_bh402_w57_4_d2 <=  heap_bh402_w57_4_d1;
            heap_bh402_w58_3_d1 <=  heap_bh402_w58_3;
            heap_bh402_w58_3_d2 <=  heap_bh402_w58_3_d1;
            heap_bh402_w65_3_d1 <=  heap_bh402_w65_3;
            heap_bh402_w65_3_d2 <=  heap_bh402_w65_3_d1;
            heap_bh402_w42_5_d1 <=  heap_bh402_w42_5;
            heap_bh402_w42_5_d2 <=  heap_bh402_w42_5_d1;
            heap_bh402_w43_5_d1 <=  heap_bh402_w43_5;
            heap_bh402_w43_5_d2 <=  heap_bh402_w43_5_d1;
            heap_bh402_w27_5_d1 <=  heap_bh402_w27_5;
            heap_bh402_w27_5_d2 <=  heap_bh402_w27_5_d1;
            heap_bh402_w28_5_d1 <=  heap_bh402_w28_5;
            heap_bh402_w28_5_d2 <=  heap_bh402_w28_5_d1;
            heap_bh402_w29_5_d1 <=  heap_bh402_w29_5;
            heap_bh402_w29_5_d2 <=  heap_bh402_w29_5_d1;
            heap_bh402_w44_5_d1 <=  heap_bh402_w44_5;
            heap_bh402_w44_5_d2 <=  heap_bh402_w44_5_d1;
            heap_bh402_w45_5_d1 <=  heap_bh402_w45_5;
            heap_bh402_w45_5_d2 <=  heap_bh402_w45_5_d1;
            heap_bh402_w46_4_d1 <=  heap_bh402_w46_4;
            heap_bh402_w46_5_d1 <=  heap_bh402_w46_5;
            heap_bh402_w47_5_d1 <=  heap_bh402_w47_5;
            heap_bh402_w48_5_d1 <=  heap_bh402_w48_5;
         end if;
      end process;
   XX_m404 <= Y ;
   YY_m404 <= X ;
   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=10 lsbY=13 msbX=23 msbY=26
   Xp_m404b406 <= XX_m404(22 downto 10) & "00";
   Yp_m404b406 <= YY_m404(25 downto 13) & "00";
   x_m404b406_0 <= Xp_m404b406(2 downto 0);
   x_m404b406_1 <= Xp_m404b406(5 downto 3);
   x_m404b406_2 <= Xp_m404b406(8 downto 6);
   x_m404b406_3 <= Xp_m404b406(11 downto 9);
   x_m404b406_4 <= Xp_m404b406(14 downto 12);
   y_m404b406_0 <= Yp_m404b406(2 downto 0);
   y_m404b406_1 <= Yp_m404b406(5 downto 3);
   y_m404b406_2 <= Yp_m404b406(8 downto 6);
   y_m404b406_3 <= Yp_m404b406(11 downto 9);
   y_m404b406_4 <= Yp_m404b406(14 downto 12);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X4_406_m404 <= y_m404b406_0 & x_m404b406_4;
   PP_m404_406X4Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X4_406_m404,
                 Y => PP406X4Y0_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_0 <= PP406X4Y0_m404(5); -- cycle= 0 cp= 5.656e-10

   -- Partial product row number 1
   Y1X3_406_m404 <= y_m404b406_1 & x_m404b406_3;
   PP_m404_406X3Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X3_406_m404,
                 Y => PP406X3Y1_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_1 <= PP406X3Y1_m404(5); -- cycle= 0 cp= 5.656e-10

   Y1X4_406_m404 <= y_m404b406_1 & x_m404b406_4;
   PP_m404_406X4Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X4_406_m404,
                 Y => PP406X4Y1_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_2 <= PP406X4Y1_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w1_0 <= PP406X4Y1_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_0 <= PP406X4Y1_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_0 <= PP406X4Y1_m404(5); -- cycle= 0 cp= 5.656e-10

   -- Partial product row number 2
   Y2X2_406_m404 <= y_m404b406_2 & x_m404b406_2;
   PP_m404_406X2Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X2_406_m404,
                 Y => PP406X2Y2_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_3 <= PP406X2Y2_m404(5); -- cycle= 0 cp= 5.656e-10

   Y2X3_406_m404 <= y_m404b406_2 & x_m404b406_3;
   PP_m404_406X3Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X3_406_m404,
                 Y => PP406X3Y2_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_4 <= PP406X3Y2_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w1_1 <= PP406X3Y2_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_1 <= PP406X3Y2_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_1 <= PP406X3Y2_m404(5); -- cycle= 0 cp= 5.656e-10

   Y2X4_406_m404 <= y_m404b406_2 & x_m404b406_4;
   PP_m404_406X4Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X4_406_m404,
                 Y => PP406X4Y2_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w1_2 <= PP406X4Y2_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_2 <= PP406X4Y2_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_2 <= PP406X4Y2_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w4_0 <= PP406X4Y2_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w5_0 <= PP406X4Y2_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w6_0 <= PP406X4Y2_m404(5); -- cycle= 0 cp= 5.656e-10

   -- Partial product row number 3
   Y3X1_406_m404 <= y_m404b406_3 & x_m404b406_1;
   PP_m404_406X1Y3_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X1_406_m404,
                 Y => PP406X1Y3_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_5 <= PP406X1Y3_m404(5); -- cycle= 0 cp= 5.656e-10

   Y3X2_406_m404 <= y_m404b406_3 & x_m404b406_2;
   PP_m404_406X2Y3_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X2_406_m404,
                 Y => PP406X2Y3_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_6 <= PP406X2Y3_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w1_3 <= PP406X2Y3_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_3 <= PP406X2Y3_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_3 <= PP406X2Y3_m404(5); -- cycle= 0 cp= 5.656e-10

   Y3X3_406_m404 <= y_m404b406_3 & x_m404b406_3;
   PP_m404_406X3Y3_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X3_406_m404,
                 Y => PP406X3Y3_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w1_4 <= PP406X3Y3_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_4 <= PP406X3Y3_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_4 <= PP406X3Y3_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w4_1 <= PP406X3Y3_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w5_1 <= PP406X3Y3_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w6_1 <= PP406X3Y3_m404(5); -- cycle= 0 cp= 5.656e-10

   Y3X4_406_m404 <= y_m404b406_3 & x_m404b406_4;
   PP_m404_406X4Y3_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X4_406_m404,
                 Y => PP406X4Y3_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w4_2 <= PP406X4Y3_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w5_2 <= PP406X4Y3_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w6_2 <= PP406X4Y3_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w7_0 <= PP406X4Y3_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w8_0 <= PP406X4Y3_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w9_0 <= PP406X4Y3_m404(5); -- cycle= 0 cp= 5.656e-10

   -- Partial product row number 4
   Y4X0_406_m404 <= y_m404b406_4 & x_m404b406_0;
   PP_m404_406X0Y4_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y4X0_406_m404,
                 Y => PP406X0Y4_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_7 <= PP406X0Y4_m404(5); -- cycle= 0 cp= 5.656e-10

   Y4X1_406_m404 <= y_m404b406_4 & x_m404b406_1;
   PP_m404_406X1Y4_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y4X1_406_m404,
                 Y => PP406X1Y4_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w0_8 <= PP406X1Y4_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w1_5 <= PP406X1Y4_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_5 <= PP406X1Y4_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_5 <= PP406X1Y4_m404(5); -- cycle= 0 cp= 5.656e-10

   Y4X2_406_m404 <= y_m404b406_4 & x_m404b406_2;
   PP_m404_406X2Y4_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y4X2_406_m404,
                 Y => PP406X2Y4_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w1_6 <= PP406X2Y4_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w2_6 <= PP406X2Y4_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w3_6 <= PP406X2Y4_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w4_3 <= PP406X2Y4_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w5_3 <= PP406X2Y4_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w6_3 <= PP406X2Y4_m404(5); -- cycle= 0 cp= 5.656e-10

   Y4X3_406_m404 <= y_m404b406_4 & x_m404b406_3;
   PP_m404_406X3Y4_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y4X3_406_m404,
                 Y => PP406X3Y4_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w4_4 <= PP406X3Y4_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w5_4 <= PP406X3Y4_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w6_4 <= PP406X3Y4_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w7_1 <= PP406X3Y4_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w8_1 <= PP406X3Y4_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w9_1 <= PP406X3Y4_m404(5); -- cycle= 0 cp= 5.656e-10

   Y4X4_406_m404 <= y_m404b406_4 & x_m404b406_4;
   PP_m404_406X4Y4_Tbl: SmallMultTableP3x3r6XuYu_F400_uid408  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y4X4_406_m404,
                 Y => PP406X4Y4_m404);
   -- Adding the relevant bits to the heap of bits
   heap_bh402_w7_2 <= PP406X4Y4_m404(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w8_2 <= PP406X4Y4_m404(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w9_2 <= PP406X4Y4_m404(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w10_0 <= PP406X4Y4_m404(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w11_0 <= PP406X4Y4_m404(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w12_0 <= PP406X4Y4_m404(5); -- cycle= 0 cp= 5.656e-10

   heap_bh402_w7_3 <= A(0); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w8_3 <= A(1); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w9_3 <= A(2); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w10_1 <= A(3); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w11_1 <= A(4); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w12_1 <= A(5); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w13_0 <= A(6); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w14_0 <= A(7); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w15_0 <= A(8); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w16_0 <= A(9); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w17_0 <= A(10); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w18_0 <= A(11); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w19_0 <= A(12); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w20_0 <= A(13); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w21_0 <= A(14); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w22_0 <= A(15); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w23_0 <= A(16); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w24_0 <= A(17); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w25_0 <= A(18); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w26_0 <= A(19); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w27_0 <= A(20); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w28_0 <= A(21); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w29_0 <= A(22); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w30_0 <= A(23); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w31_0 <= A(24); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w32_0 <= A(25); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w33_0 <= A(26); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w34_0 <= A(27); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w35_0 <= A(28); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w36_0 <= A(29); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w37_0 <= A(30); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w38_0 <= A(31); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w39_0 <= A(32); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w40_0 <= A(33); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w41_0 <= A(34); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w42_0 <= A(35); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w43_0 <= A(36); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w44_0 <= A(37); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w45_0 <= A(38); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w46_0 <= A(39); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w47_0 <= A(40); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w48_0 <= A(41); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w49_0 <= A(42); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w50_0 <= A(43); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w51_0 <= A(44); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w52_0 <= A(45); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w53_0 <= A(46); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w54_0 <= A(47); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w55_0 <= A(48); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w56_0 <= A(49); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w57_0 <= A(50); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w58_0 <= A(51); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w59_0 <= A(52); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w60_0 <= A(53); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w61_0 <= A(54); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w62_0 <= A(55); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w63_0 <= A(56); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w64_0 <= A(57); -- cycle= 0 cp= 5.656e-10
   heap_bh402_w65_0 <= A(58); -- cycle= 0 cp= 5.656e-10
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------
   DSP_bh402_ch1_0 <= std_logic_vector(signed("0" & XX_m404(22 downto 0) & "0") * signed("" & YY_m404(43 downto 26) & ""));
   heap_bh402_w31_1 <= not( DSP_bh402_ch1_0(42) ); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w30_1 <= DSP_bh402_ch1_0(41); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w29_1 <= DSP_bh402_ch1_0(40); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w28_1 <= DSP_bh402_ch1_0(39); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w27_1 <= DSP_bh402_ch1_0(38); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w26_1 <= DSP_bh402_ch1_0(37); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w25_1 <= DSP_bh402_ch1_0(36); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w24_1 <= DSP_bh402_ch1_0(35); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w23_1 <= DSP_bh402_ch1_0(34); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w22_1 <= DSP_bh402_ch1_0(33); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w21_1 <= DSP_bh402_ch1_0(32); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w20_1 <= DSP_bh402_ch1_0(31); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w19_1 <= DSP_bh402_ch1_0(30); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w18_1 <= DSP_bh402_ch1_0(29); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w17_1 <= DSP_bh402_ch1_0(28); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w16_1 <= DSP_bh402_ch1_0(27); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w15_1 <= DSP_bh402_ch1_0(26); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w14_1 <= DSP_bh402_ch1_0(25); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w13_1 <= DSP_bh402_ch1_0(24); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w12_2 <= DSP_bh402_ch1_0(23); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w11_2 <= DSP_bh402_ch1_0(22); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w10_2 <= DSP_bh402_ch1_0(21); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w9_4 <= DSP_bh402_ch1_0(20); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w8_4 <= DSP_bh402_ch1_0(19); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w7_4 <= DSP_bh402_ch1_0(18); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w6_5 <= DSP_bh402_ch1_0(17); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w5_5 <= DSP_bh402_ch1_0(16); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w4_5 <= DSP_bh402_ch1_0(15); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w3_7 <= DSP_bh402_ch1_0(14); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w2_7 <= DSP_bh402_ch1_0(13); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w1_7 <= DSP_bh402_ch1_0(12); -- cycle= 0 cp= 2.387e-09
   heap_bh402_w0_9 <= DSP_bh402_ch1_0(11); -- cycle= 0 cp= 2.387e-09
   ----------------Synchro barrier, entering cycle 0----------------
   DSP_bh402_ch3_0 <= std_logic_vector(signed("" & XX_m404(47 downto 23) & "") * signed("0" & YY_m404(8 downto 0) & "00000000"));
   DSP_bh402_root3_1 <= std_logic_vector(signed("" & XX_m404(47 downto 23) & "") * signed("0" & YY_m404(25 downto 9) & ""));
   ----------------Synchro barrier, entering cycle 1----------------
   DSP_bh402_ch3_1<= std_logic_vector(signed(DSP_bh402_root3_1_d1) +  signed( DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) & DSP_bh402_ch3_0_d1(42) &   DSP_bh402_ch3_0_d1(42 downto 17) ));
   DSP_bh402_root3_2 <= std_logic_vector(signed("" & XX_m404_d1(47 downto 23) & "") * signed("" & YY_m404_d1(43 downto 26) & ""));
   ----------------Synchro barrier, entering cycle 2----------------
   DSP_bh402_ch3_2<= std_logic_vector(signed(DSP_bh402_root3_2_d1) +  signed( DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) & DSP_bh402_ch3_1_d1(43) &   DSP_bh402_ch3_1_d1(43 downto 17) ));
   heap_bh402_w12_3 <= DSP_bh402_ch3_1_d1(16); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w11_3 <= DSP_bh402_ch3_1_d1(15); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w10_3 <= DSP_bh402_ch3_1_d1(14); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w9_5 <= DSP_bh402_ch3_1_d1(13); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w8_5 <= DSP_bh402_ch3_1_d1(12); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w7_5 <= DSP_bh402_ch3_1_d1(11); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w6_6 <= DSP_bh402_ch3_1_d1(10); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w5_6 <= DSP_bh402_ch3_1_d1(9); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w4_6 <= DSP_bh402_ch3_1_d1(8); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w3_8 <= DSP_bh402_ch3_1_d1(7); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w2_8 <= DSP_bh402_ch3_1_d1(6); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w1_8 <= DSP_bh402_ch3_1_d1(5); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w0_10 <= DSP_bh402_ch3_1_d1(4); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w57_1 <= not( DSP_bh402_ch3_2(44) ); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w56_1 <= DSP_bh402_ch3_2(43); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w55_1 <= DSP_bh402_ch3_2(42); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w54_1 <= DSP_bh402_ch3_2(41); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w53_1 <= DSP_bh402_ch3_2(40); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w52_1 <= DSP_bh402_ch3_2(39); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w51_1 <= DSP_bh402_ch3_2(38); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w50_1 <= DSP_bh402_ch3_2(37); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w49_1 <= DSP_bh402_ch3_2(36); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w48_1 <= DSP_bh402_ch3_2(35); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w47_1 <= DSP_bh402_ch3_2(34); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w46_1 <= DSP_bh402_ch3_2(33); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w45_1 <= DSP_bh402_ch3_2(32); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w44_1 <= DSP_bh402_ch3_2(31); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w43_1 <= DSP_bh402_ch3_2(30); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w42_1 <= DSP_bh402_ch3_2(29); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w41_1 <= DSP_bh402_ch3_2(28); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w40_1 <= DSP_bh402_ch3_2(27); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w39_1 <= DSP_bh402_ch3_2(26); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w38_1 <= DSP_bh402_ch3_2(25); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w37_1 <= DSP_bh402_ch3_2(24); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w36_1 <= DSP_bh402_ch3_2(23); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w35_1 <= DSP_bh402_ch3_2(22); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w34_1 <= DSP_bh402_ch3_2(21); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w33_1 <= DSP_bh402_ch3_2(20); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w32_1 <= DSP_bh402_ch3_2(19); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w31_2 <= DSP_bh402_ch3_2(18); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w30_2 <= DSP_bh402_ch3_2(17); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w29_2 <= DSP_bh402_ch3_2(16); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w28_2 <= DSP_bh402_ch3_2(15); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w27_2 <= DSP_bh402_ch3_2(14); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w26_2 <= DSP_bh402_ch3_2(13); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w25_2 <= DSP_bh402_ch3_2(12); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w24_2 <= DSP_bh402_ch3_2(11); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w23_2 <= DSP_bh402_ch3_2(10); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w22_2 <= DSP_bh402_ch3_2(9); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w21_2 <= DSP_bh402_ch3_2(8); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w20_2 <= DSP_bh402_ch3_2(7); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w19_2 <= DSP_bh402_ch3_2(6); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w18_2 <= DSP_bh402_ch3_2(5); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w17_2 <= DSP_bh402_ch3_2(4); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w16_2 <= DSP_bh402_ch3_2(3); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w15_2 <= DSP_bh402_ch3_2(2); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w14_2 <= DSP_bh402_ch3_2(1); -- cycle= 2 cp= 2.161e-09
   heap_bh402_w13_2 <= DSP_bh402_ch3_2(0); -- cycle= 2 cp= 2.161e-09
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh402_w6_7 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w31_3 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w32_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w33_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w34_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w35_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w36_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w37_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w38_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w39_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w40_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w41_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w42_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w43_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w44_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w45_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w46_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w47_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w48_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w49_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w50_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w51_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w52_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w53_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w54_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w55_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w56_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w58_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w59_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w60_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w61_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w62_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w63_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w64_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh402_w65_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_0_0 <= heap_bh402_w0_8 & heap_bh402_w0_7 & heap_bh402_w0_6 & heap_bh402_w0_5 & heap_bh402_w0_4 & heap_bh402_w0_3;
      Compressor_bh402_0: Compressor_6_3
      port map ( R => CompressorOut_bh402_0_0,
                 X0 => CompressorIn_bh402_0_0);
   heap_bh402_w0_11 <= CompressorOut_bh402_0_0(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w1_9 <= CompressorOut_bh402_0_0(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w2_9 <= CompressorOut_bh402_0_0(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_1_1 <= heap_bh402_w1_6 & heap_bh402_w1_5 & heap_bh402_w1_4 & heap_bh402_w1_3 & heap_bh402_w1_2 & heap_bh402_w1_1;
      Compressor_bh402_1: Compressor_6_3
      port map ( R => CompressorOut_bh402_1_1,
                 X0 => CompressorIn_bh402_1_1);
   heap_bh402_w1_10 <= CompressorOut_bh402_1_1(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w2_10 <= CompressorOut_bh402_1_1(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w3_9 <= CompressorOut_bh402_1_1(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_2_2 <= heap_bh402_w2_6 & heap_bh402_w2_5 & heap_bh402_w2_4 & heap_bh402_w2_3 & heap_bh402_w2_2 & heap_bh402_w2_1;
      Compressor_bh402_2: Compressor_6_3
      port map ( R => CompressorOut_bh402_2_2,
                 X0 => CompressorIn_bh402_2_2);
   heap_bh402_w2_11 <= CompressorOut_bh402_2_2(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w3_10 <= CompressorOut_bh402_2_2(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w4_7 <= CompressorOut_bh402_2_2(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_3_3 <= heap_bh402_w3_6 & heap_bh402_w3_5 & heap_bh402_w3_4 & heap_bh402_w3_3 & heap_bh402_w3_2 & heap_bh402_w3_1;
      Compressor_bh402_3: Compressor_6_3
      port map ( R => CompressorOut_bh402_3_3,
                 X0 => CompressorIn_bh402_3_3);
   heap_bh402_w3_11 <= CompressorOut_bh402_3_3(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w4_8 <= CompressorOut_bh402_3_3(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w5_7 <= CompressorOut_bh402_3_3(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_4_4 <= heap_bh402_w6_7 & heap_bh402_w6_4 & heap_bh402_w6_3 & heap_bh402_w6_2 & heap_bh402_w6_1 & heap_bh402_w6_0;
      Compressor_bh402_4: Compressor_6_3
      port map ( R => CompressorOut_bh402_4_4,
                 X0 => CompressorIn_bh402_4_4);
   heap_bh402_w6_8 <= CompressorOut_bh402_4_4(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w7_6 <= CompressorOut_bh402_4_4(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w8_6 <= CompressorOut_bh402_4_4(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_5_5 <= heap_bh402_w4_4 & heap_bh402_w4_3 & heap_bh402_w4_2 & heap_bh402_w4_1;
   CompressorIn_bh402_5_6(0) <= heap_bh402_w5_4;
      Compressor_bh402_5: Compressor_14_3
      port map ( R => CompressorOut_bh402_5_5,
                 X0 => CompressorIn_bh402_5_5,
                 X1 => CompressorIn_bh402_5_6);
   heap_bh402_w4_9 <= CompressorOut_bh402_5_5(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w5_8 <= CompressorOut_bh402_5_5(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w6_9 <= CompressorOut_bh402_5_5(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_6_7 <= heap_bh402_w7_3 & heap_bh402_w7_2 & heap_bh402_w7_1 & heap_bh402_w7_0;
   CompressorIn_bh402_6_8(0) <= heap_bh402_w8_3;
      Compressor_bh402_6: Compressor_14_3
      port map ( R => CompressorOut_bh402_6_6,
                 X0 => CompressorIn_bh402_6_7,
                 X1 => CompressorIn_bh402_6_8);
   heap_bh402_w7_7 <= CompressorOut_bh402_6_6(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w8_7 <= CompressorOut_bh402_6_6(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w9_6 <= CompressorOut_bh402_6_6(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_7_9 <= heap_bh402_w9_3 & heap_bh402_w9_2 & heap_bh402_w9_1 & heap_bh402_w9_0;
   CompressorIn_bh402_7_10(0) <= heap_bh402_w10_1;
      Compressor_bh402_7: Compressor_14_3
      port map ( R => CompressorOut_bh402_7_7,
                 X0 => CompressorIn_bh402_7_9,
                 X1 => CompressorIn_bh402_7_10);
   heap_bh402_w9_7 <= CompressorOut_bh402_7_7(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w10_4 <= CompressorOut_bh402_7_7(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w11_4 <= CompressorOut_bh402_7_7(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_8_11 <= heap_bh402_w5_3 & heap_bh402_w5_2 & heap_bh402_w5_1 & heap_bh402_w5_0;
      Compressor_bh402_8: Compressor_4_3
      port map ( R => CompressorOut_bh402_8_8,
                 X0 => CompressorIn_bh402_8_11);
   heap_bh402_w5_9 <= CompressorOut_bh402_8_8(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w6_10 <= CompressorOut_bh402_8_8(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w7_8 <= CompressorOut_bh402_8_8(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_9_12 <= heap_bh402_w0_2 & heap_bh402_w0_1 & heap_bh402_w0_0;
   CompressorIn_bh402_9_13(0) <= heap_bh402_w1_0;
      Compressor_bh402_9: Compressor_13_3
      port map ( R => CompressorOut_bh402_9_9,
                 X0 => CompressorIn_bh402_9_12,
                 X1 => CompressorIn_bh402_9_13);
   heap_bh402_w0_12 <= CompressorOut_bh402_9_9(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w1_11 <= CompressorOut_bh402_9_9(1); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w2_12 <= CompressorOut_bh402_9_9(2); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_10_14 <= heap_bh402_w8_2 & heap_bh402_w8_1 & heap_bh402_w8_0;
      Compressor_bh402_10: Compressor_3_2
      port map ( R => CompressorOut_bh402_10_10,
                 X0 => CompressorIn_bh402_10_14);
   heap_bh402_w8_8 <= CompressorOut_bh402_10_10(0); -- cycle= 0 cp= 1.09632e-09
   heap_bh402_w9_8 <= CompressorOut_bh402_10_10(1); -- cycle= 0 cp= 1.09632e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_11_15 <= heap_bh402_w2_0 & heap_bh402_w2_12 & heap_bh402_w2_11 & heap_bh402_w2_10;
   CompressorIn_bh402_11_16(0) <= heap_bh402_w3_0;
      Compressor_bh402_11: Compressor_14_3
      port map ( R => CompressorOut_bh402_11_11,
                 X0 => CompressorIn_bh402_11_15,
                 X1 => CompressorIn_bh402_11_16);
   heap_bh402_w2_13 <= CompressorOut_bh402_11_11(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w3_12 <= CompressorOut_bh402_11_11(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w4_10 <= CompressorOut_bh402_11_11(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_12_17 <= heap_bh402_w4_0 & heap_bh402_w4_9 & heap_bh402_w4_8 & heap_bh402_w4_7;
   CompressorIn_bh402_12_18(0) <= heap_bh402_w5_9;
      Compressor_bh402_12: Compressor_14_3
      port map ( R => CompressorOut_bh402_12_12,
                 X0 => CompressorIn_bh402_12_17,
                 X1 => CompressorIn_bh402_12_18);
   heap_bh402_w4_11 <= CompressorOut_bh402_12_12(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w5_10 <= CompressorOut_bh402_12_12(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w6_11 <= CompressorOut_bh402_12_12(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_13_19 <= heap_bh402_w6_10 & heap_bh402_w6_9 & heap_bh402_w6_8;
   CompressorIn_bh402_13_20 <= heap_bh402_w7_8 & heap_bh402_w7_7;
      Compressor_bh402_13: Compressor_23_3
      port map ( R => CompressorOut_bh402_13_13,
                 X0 => CompressorIn_bh402_13_19,
                 X1 => CompressorIn_bh402_13_20);
   heap_bh402_w6_12 <= CompressorOut_bh402_13_13(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w7_9 <= CompressorOut_bh402_13_13(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w8_9 <= CompressorOut_bh402_13_13(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_14_21 <= heap_bh402_w8_8 & heap_bh402_w8_7 & heap_bh402_w8_6;
   CompressorIn_bh402_14_22 <= heap_bh402_w9_8 & heap_bh402_w9_7;
      Compressor_bh402_14: Compressor_23_3
      port map ( R => CompressorOut_bh402_14_14,
                 X0 => CompressorIn_bh402_14_21,
                 X1 => CompressorIn_bh402_14_22);
   heap_bh402_w8_10 <= CompressorOut_bh402_14_14(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w9_9 <= CompressorOut_bh402_14_14(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w10_5 <= CompressorOut_bh402_14_14(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_15_23 <= heap_bh402_w11_1 & heap_bh402_w11_0 & heap_bh402_w11_4;
   CompressorIn_bh402_15_24 <= heap_bh402_w12_1 & heap_bh402_w12_0;
      Compressor_bh402_15: Compressor_23_3
      port map ( R => CompressorOut_bh402_15_15,
                 X0 => CompressorIn_bh402_15_23,
                 X1 => CompressorIn_bh402_15_24);
   heap_bh402_w11_5 <= CompressorOut_bh402_15_15(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w12_4 <= CompressorOut_bh402_15_15(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w13_3 <= CompressorOut_bh402_15_15(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_16_25 <= heap_bh402_w1_11 & heap_bh402_w1_10 & heap_bh402_w1_9;
   CompressorIn_bh402_16_26(0) <= heap_bh402_w2_9;
      Compressor_bh402_16: Compressor_13_3
      port map ( R => CompressorOut_bh402_16_16,
                 X0 => CompressorIn_bh402_16_25,
                 X1 => CompressorIn_bh402_16_26);
   heap_bh402_w1_12 <= CompressorOut_bh402_16_16(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w2_14 <= CompressorOut_bh402_16_16(1); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w3_13 <= CompressorOut_bh402_16_16(2); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh402_17_27 <= heap_bh402_w3_11 & heap_bh402_w3_10 & heap_bh402_w3_9;
      Compressor_bh402_17: Compressor_3_2
      port map ( R => CompressorOut_bh402_17_17,
                 X0 => CompressorIn_bh402_17_27);
   heap_bh402_w3_14 <= CompressorOut_bh402_17_17(0); -- cycle= 0 cp= 1.62704e-09
   heap_bh402_w4_12 <= CompressorOut_bh402_17_17(1); -- cycle= 0 cp= 1.62704e-09

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_18_28 <= heap_bh402_w3_14_d1 & heap_bh402_w3_13_d1 & heap_bh402_w3_12_d1;
   CompressorIn_bh402_18_29 <= heap_bh402_w4_12_d1 & heap_bh402_w4_11_d1;
      Compressor_bh402_18: Compressor_23_3
      port map ( R => CompressorOut_bh402_18_18,
                 X0 => CompressorIn_bh402_18_28,
                 X1 => CompressorIn_bh402_18_29);
   heap_bh402_w3_15 <= CompressorOut_bh402_18_18(0); -- cycle= 1 cp= 0
   heap_bh402_w4_13 <= CompressorOut_bh402_18_18(1); -- cycle= 1 cp= 0
   heap_bh402_w5_11 <= CompressorOut_bh402_18_18(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_19_30 <= heap_bh402_w5_8_d1 & heap_bh402_w5_7_d1 & heap_bh402_w5_10_d1;
   CompressorIn_bh402_19_31 <= heap_bh402_w6_12_d1 & heap_bh402_w6_11_d1;
      Compressor_bh402_19: Compressor_23_3
      port map ( R => CompressorOut_bh402_19_19,
                 X0 => CompressorIn_bh402_19_30,
                 X1 => CompressorIn_bh402_19_31);
   heap_bh402_w5_12 <= CompressorOut_bh402_19_19(0); -- cycle= 1 cp= 0
   heap_bh402_w6_13 <= CompressorOut_bh402_19_19(1); -- cycle= 1 cp= 0
   heap_bh402_w7_10 <= CompressorOut_bh402_19_19(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_20_32 <= heap_bh402_w10_0_d1 & heap_bh402_w10_4_d1 & heap_bh402_w10_5_d1;
   CompressorIn_bh402_20_33(0) <= heap_bh402_w11_5_d1;
      Compressor_bh402_20: Compressor_13_3
      port map ( R => CompressorOut_bh402_20_20,
                 X0 => CompressorIn_bh402_20_32,
                 X1 => CompressorIn_bh402_20_33);
   heap_bh402_w10_6 <= CompressorOut_bh402_20_20(0); -- cycle= 1 cp= 0
   heap_bh402_w11_6 <= CompressorOut_bh402_20_20(1); -- cycle= 1 cp= 0
   heap_bh402_w12_5 <= CompressorOut_bh402_20_20(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_21_34 <= heap_bh402_w7_6_d1 & heap_bh402_w7_9_d1 & heap_bh402_w7_4_d1 & heap_bh402_w7_10;
   CompressorIn_bh402_21_35(0) <= heap_bh402_w8_10_d1;
      Compressor_bh402_21: Compressor_14_3
      port map ( R => CompressorOut_bh402_21_21,
                 X0 => CompressorIn_bh402_21_34,
                 X1 => CompressorIn_bh402_21_35);
   heap_bh402_w7_11 <= CompressorOut_bh402_21_21(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w8_11 <= CompressorOut_bh402_21_21(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w9_10 <= CompressorOut_bh402_21_21(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_22_36 <= heap_bh402_w0_12_d1 & heap_bh402_w0_11_d1 & heap_bh402_w0_9_d1;
   CompressorIn_bh402_22_37 <= heap_bh402_w1_12_d1 & heap_bh402_w1_7_d1;
      Compressor_bh402_22: Compressor_23_3
      port map ( R => CompressorOut_bh402_22_22,
                 X0 => CompressorIn_bh402_22_36,
                 X1 => CompressorIn_bh402_22_37);
   heap_bh402_w0_13 <= CompressorOut_bh402_22_22(0); -- cycle= 1 cp= 0
   heap_bh402_w1_13 <= CompressorOut_bh402_22_22(1); -- cycle= 1 cp= 0
   heap_bh402_w2_15 <= CompressorOut_bh402_22_22(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_23_38 <= heap_bh402_w2_14_d1 & heap_bh402_w2_13_d1 & heap_bh402_w2_7_d1;
   CompressorIn_bh402_23_39 <= heap_bh402_w3_7_d1 & heap_bh402_w3_15;
      Compressor_bh402_23: Compressor_23_3
      port map ( R => CompressorOut_bh402_23_23,
                 X0 => CompressorIn_bh402_23_38,
                 X1 => CompressorIn_bh402_23_39);
   heap_bh402_w2_16 <= CompressorOut_bh402_23_23(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w3_16 <= CompressorOut_bh402_23_23(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w4_14 <= CompressorOut_bh402_23_23(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_24_40 <= heap_bh402_w4_10_d1 & heap_bh402_w4_5_d1 & heap_bh402_w4_13;
   CompressorIn_bh402_24_41 <= heap_bh402_w5_5_d1 & heap_bh402_w5_12;
      Compressor_bh402_24: Compressor_23_3
      port map ( R => CompressorOut_bh402_24_24,
                 X0 => CompressorIn_bh402_24_40,
                 X1 => CompressorIn_bh402_24_41);
   heap_bh402_w4_15 <= CompressorOut_bh402_24_24(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w5_13 <= CompressorOut_bh402_24_24(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w6_14 <= CompressorOut_bh402_24_24(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_25_42 <= heap_bh402_w9_6_d1 & heap_bh402_w9_9_d1 & heap_bh402_w9_4_d1;
   CompressorIn_bh402_25_43 <= heap_bh402_w10_2_d1 & heap_bh402_w10_6;
      Compressor_bh402_25: Compressor_23_3
      port map ( R => CompressorOut_bh402_25_25,
                 X0 => CompressorIn_bh402_25_42,
                 X1 => CompressorIn_bh402_25_43);
   heap_bh402_w9_11 <= CompressorOut_bh402_25_25(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w10_7 <= CompressorOut_bh402_25_25(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w11_7 <= CompressorOut_bh402_25_25(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_26_44 <= heap_bh402_w12_4_d1 & heap_bh402_w12_2_d1 & heap_bh402_w12_5;
   CompressorIn_bh402_26_45 <= heap_bh402_w13_0_d1 & heap_bh402_w13_3_d1;
      Compressor_bh402_26: Compressor_23_3
      port map ( R => CompressorOut_bh402_26_26,
                 X0 => CompressorIn_bh402_26_44,
                 X1 => CompressorIn_bh402_26_45);
   heap_bh402_w12_6 <= CompressorOut_bh402_26_26(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w13_4 <= CompressorOut_bh402_26_26(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w14_3 <= CompressorOut_bh402_26_26(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_27_46 <= heap_bh402_w31_3_d1 & heap_bh402_w31_0_d1 & heap_bh402_w31_1_d1;
   CompressorIn_bh402_27_47 <= heap_bh402_w32_2_d1 & heap_bh402_w32_0_d1;
      Compressor_bh402_27: Compressor_23_3
      port map ( R => CompressorOut_bh402_27_27,
                 X0 => CompressorIn_bh402_27_46,
                 X1 => CompressorIn_bh402_27_47);
   heap_bh402_w31_4 <= CompressorOut_bh402_27_27(0); -- cycle= 1 cp= 0
   heap_bh402_w32_3 <= CompressorOut_bh402_27_27(1); -- cycle= 1 cp= 0
   heap_bh402_w33_3 <= CompressorOut_bh402_27_27(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_28_48 <= heap_bh402_w8_9_d1 & heap_bh402_w8_4_d1 & heap_bh402_w8_11;
   CompressorIn_bh402_28_49 <= heap_bh402_w9_11 & heap_bh402_w9_10;
      Compressor_bh402_28: Compressor_23_3
      port map ( R => CompressorOut_bh402_28_28,
                 X0 => CompressorIn_bh402_28_48,
                 X1 => CompressorIn_bh402_28_49);
   heap_bh402_w8_12 <= CompressorOut_bh402_28_28(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w9_12 <= CompressorOut_bh402_28_28(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w10_8 <= CompressorOut_bh402_28_28(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_29_50 <= heap_bh402_w14_0_d1 & heap_bh402_w14_1_d1 & heap_bh402_w14_3;
   CompressorIn_bh402_29_51 <= heap_bh402_w15_0_d1 & heap_bh402_w15_1_d1;
      Compressor_bh402_29: Compressor_23_3
      port map ( R => CompressorOut_bh402_29_29,
                 X0 => CompressorIn_bh402_29_50,
                 X1 => CompressorIn_bh402_29_51);
   heap_bh402_w14_4 <= CompressorOut_bh402_29_29(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w15_3 <= CompressorOut_bh402_29_29(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w16_3 <= CompressorOut_bh402_29_29(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_30_52 <= heap_bh402_w33_2_d1 & heap_bh402_w33_0_d1 & heap_bh402_w33_3;
   CompressorIn_bh402_30_53 <= heap_bh402_w34_2_d1 & heap_bh402_w34_0_d1;
      Compressor_bh402_30: Compressor_23_3
      port map ( R => CompressorOut_bh402_30_30,
                 X0 => CompressorIn_bh402_30_52,
                 X1 => CompressorIn_bh402_30_53);
   heap_bh402_w33_4 <= CompressorOut_bh402_30_30(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w34_3 <= CompressorOut_bh402_30_30(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh402_w35_3 <= CompressorOut_bh402_30_30(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_31_54 <= heap_bh402_w6_5_d1 & heap_bh402_w6_13 & heap_bh402_w6_14;
   CompressorIn_bh402_31_55(0) <= heap_bh402_w7_11;
      Compressor_bh402_31: Compressor_13_3
      port map ( R => CompressorOut_bh402_31_31,
                 X0 => CompressorIn_bh402_31_54,
                 X1 => CompressorIn_bh402_31_55);
   heap_bh402_w6_15 <= CompressorOut_bh402_31_31(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w7_12 <= CompressorOut_bh402_31_31(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w8_13 <= CompressorOut_bh402_31_31(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_32_56 <= heap_bh402_w11_2_d1 & heap_bh402_w11_6 & heap_bh402_w11_7;
   CompressorIn_bh402_32_57(0) <= heap_bh402_w12_6;
      Compressor_bh402_32: Compressor_13_3
      port map ( R => CompressorOut_bh402_32_32,
                 X0 => CompressorIn_bh402_32_56,
                 X1 => CompressorIn_bh402_32_57);
   heap_bh402_w11_8 <= CompressorOut_bh402_32_32(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w12_7 <= CompressorOut_bh402_32_32(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w13_5 <= CompressorOut_bh402_32_32(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_33_58 <= heap_bh402_w16_0_d1 & heap_bh402_w16_1_d1 & heap_bh402_w16_3;
   CompressorIn_bh402_33_59 <= heap_bh402_w17_0_d1 & heap_bh402_w17_1_d1;
      Compressor_bh402_33: Compressor_23_3
      port map ( R => CompressorOut_bh402_33_33,
                 X0 => CompressorIn_bh402_33_58,
                 X1 => CompressorIn_bh402_33_59);
   heap_bh402_w16_4 <= CompressorOut_bh402_33_33(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w17_3 <= CompressorOut_bh402_33_33(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w18_3 <= CompressorOut_bh402_33_33(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_34_60 <= heap_bh402_w35_2_d1 & heap_bh402_w35_0_d1 & heap_bh402_w35_3;
   CompressorIn_bh402_34_61 <= heap_bh402_w36_2_d1 & heap_bh402_w36_0_d1;
      Compressor_bh402_34: Compressor_23_3
      port map ( R => CompressorOut_bh402_34_34,
                 X0 => CompressorIn_bh402_34_60,
                 X1 => CompressorIn_bh402_34_61);
   heap_bh402_w35_4 <= CompressorOut_bh402_34_34(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w36_3 <= CompressorOut_bh402_34_34(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh402_w37_3 <= CompressorOut_bh402_34_34(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_35_62 <= heap_bh402_w13_1_d1 & heap_bh402_w13_4 & heap_bh402_w13_5;
   CompressorIn_bh402_35_63(0) <= heap_bh402_w14_4;
      Compressor_bh402_35: Compressor_13_3
      port map ( R => CompressorOut_bh402_35_35,
                 X0 => CompressorIn_bh402_35_62,
                 X1 => CompressorIn_bh402_35_63);
   heap_bh402_w13_6 <= CompressorOut_bh402_35_35(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w14_5 <= CompressorOut_bh402_35_35(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w15_4 <= CompressorOut_bh402_35_35(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_36_64 <= heap_bh402_w18_0_d2 & heap_bh402_w18_1_d2 & heap_bh402_w18_3_d1;
   CompressorIn_bh402_36_65 <= heap_bh402_w19_0_d2 & heap_bh402_w19_1_d2;
      Compressor_bh402_36: Compressor_23_3
      port map ( R => CompressorOut_bh402_36_36,
                 X0 => CompressorIn_bh402_36_64,
                 X1 => CompressorIn_bh402_36_65);
   heap_bh402_w18_4 <= CompressorOut_bh402_36_36(0); -- cycle= 2 cp= 0
   heap_bh402_w19_3 <= CompressorOut_bh402_36_36(1); -- cycle= 2 cp= 0
   heap_bh402_w20_3 <= CompressorOut_bh402_36_36(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh402_37_66 <= heap_bh402_w37_2_d1 & heap_bh402_w37_0_d1 & heap_bh402_w37_3;
   CompressorIn_bh402_37_67 <= heap_bh402_w38_2_d1 & heap_bh402_w38_0_d1;
      Compressor_bh402_37: Compressor_23_3
      port map ( R => CompressorOut_bh402_37_37,
                 X0 => CompressorIn_bh402_37_66,
                 X1 => CompressorIn_bh402_37_67);
   heap_bh402_w37_4 <= CompressorOut_bh402_37_37(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w38_3 <= CompressorOut_bh402_37_37(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh402_w39_3 <= CompressorOut_bh402_37_37(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_38_68 <= heap_bh402_w20_0_d2 & heap_bh402_w20_1_d2 & heap_bh402_w20_3;
   CompressorIn_bh402_38_69 <= heap_bh402_w21_0_d2 & heap_bh402_w21_1_d2;
      Compressor_bh402_38: Compressor_23_3
      port map ( R => CompressorOut_bh402_38_38,
                 X0 => CompressorIn_bh402_38_68,
                 X1 => CompressorIn_bh402_38_69);
   heap_bh402_w20_4 <= CompressorOut_bh402_38_38(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh402_w21_3 <= CompressorOut_bh402_38_38(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh402_w22_3 <= CompressorOut_bh402_38_38(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_39_70 <= heap_bh402_w39_2_d2 & heap_bh402_w39_0_d2 & heap_bh402_w39_3_d1;
   CompressorIn_bh402_39_71 <= heap_bh402_w40_2_d2 & heap_bh402_w40_0_d2;
      Compressor_bh402_39: Compressor_23_3
      port map ( R => CompressorOut_bh402_39_39,
                 X0 => CompressorIn_bh402_39_70,
                 X1 => CompressorIn_bh402_39_71);
   heap_bh402_w39_4 <= CompressorOut_bh402_39_39(0); -- cycle= 2 cp= 0
   heap_bh402_w40_3 <= CompressorOut_bh402_39_39(1); -- cycle= 2 cp= 0
   heap_bh402_w41_3 <= CompressorOut_bh402_39_39(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_40_72 <= heap_bh402_w22_0_d2 & heap_bh402_w22_1_d2 & heap_bh402_w22_3;
   CompressorIn_bh402_40_73 <= heap_bh402_w23_0_d2 & heap_bh402_w23_1_d2;
      Compressor_bh402_40: Compressor_23_3
      port map ( R => CompressorOut_bh402_40_40,
                 X0 => CompressorIn_bh402_40_72,
                 X1 => CompressorIn_bh402_40_73);
   heap_bh402_w22_4 <= CompressorOut_bh402_40_40(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh402_w23_3 <= CompressorOut_bh402_40_40(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh402_w24_3 <= CompressorOut_bh402_40_40(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_41_74 <= heap_bh402_w41_2_d2 & heap_bh402_w41_0_d2 & heap_bh402_w41_3;
   CompressorIn_bh402_41_75 <= heap_bh402_w42_2_d2 & heap_bh402_w42_0_d2;
      Compressor_bh402_41: Compressor_23_3
      port map ( R => CompressorOut_bh402_41_41,
                 X0 => CompressorIn_bh402_41_74,
                 X1 => CompressorIn_bh402_41_75);
   heap_bh402_w41_4 <= CompressorOut_bh402_41_41(0); -- cycle= 2 cp= 5.3072e-10
   heap_bh402_w42_3 <= CompressorOut_bh402_41_41(1); -- cycle= 2 cp= 5.3072e-10
   heap_bh402_w43_3 <= CompressorOut_bh402_41_41(2); -- cycle= 2 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_42_76 <= heap_bh402_w24_0_d2 & heap_bh402_w24_1_d2 & heap_bh402_w24_3;
   CompressorIn_bh402_42_77 <= heap_bh402_w25_0_d2 & heap_bh402_w25_1_d2;
      Compressor_bh402_42: Compressor_23_3
      port map ( R => CompressorOut_bh402_42_42,
                 X0 => CompressorIn_bh402_42_76,
                 X1 => CompressorIn_bh402_42_77);
   heap_bh402_w24_4 <= CompressorOut_bh402_42_42(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh402_w25_3 <= CompressorOut_bh402_42_42(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh402_w26_3 <= CompressorOut_bh402_42_42(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_43_78 <= heap_bh402_w43_2_d2 & heap_bh402_w43_0_d2 & heap_bh402_w43_3;
   CompressorIn_bh402_43_79 <= heap_bh402_w44_2_d2 & heap_bh402_w44_0_d2;
      Compressor_bh402_43: Compressor_23_3
      port map ( R => CompressorOut_bh402_43_43,
                 X0 => CompressorIn_bh402_43_78,
                 X1 => CompressorIn_bh402_43_79);
   heap_bh402_w43_4 <= CompressorOut_bh402_43_43(0); -- cycle= 2 cp= 1.06144e-09
   heap_bh402_w44_3 <= CompressorOut_bh402_43_43(1); -- cycle= 2 cp= 1.06144e-09
   heap_bh402_w45_3 <= CompressorOut_bh402_43_43(2); -- cycle= 2 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_44_80 <= heap_bh402_w26_0_d3 & heap_bh402_w26_1_d3 & heap_bh402_w26_3_d1;
   CompressorIn_bh402_44_81 <= heap_bh402_w27_0_d3 & heap_bh402_w27_1_d3;
      Compressor_bh402_44: Compressor_23_3
      port map ( R => CompressorOut_bh402_44_44,
                 X0 => CompressorIn_bh402_44_80,
                 X1 => CompressorIn_bh402_44_81);
   heap_bh402_w26_4 <= CompressorOut_bh402_44_44(0); -- cycle= 3 cp= 0
   heap_bh402_w27_3 <= CompressorOut_bh402_44_44(1); -- cycle= 3 cp= 0
   heap_bh402_w28_3 <= CompressorOut_bh402_44_44(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh402_45_82 <= heap_bh402_w45_2_d2 & heap_bh402_w45_0_d2 & heap_bh402_w45_3;
   CompressorIn_bh402_45_83 <= heap_bh402_w46_2_d2 & heap_bh402_w46_0_d2;
      Compressor_bh402_45: Compressor_23_3
      port map ( R => CompressorOut_bh402_45_45,
                 X0 => CompressorIn_bh402_45_82,
                 X1 => CompressorIn_bh402_45_83);
   heap_bh402_w45_4 <= CompressorOut_bh402_45_45(0); -- cycle= 2 cp= 1.59216e-09
   heap_bh402_w46_3 <= CompressorOut_bh402_45_45(1); -- cycle= 2 cp= 1.59216e-09
   heap_bh402_w47_3 <= CompressorOut_bh402_45_45(2); -- cycle= 2 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_46_84 <= heap_bh402_w28_0_d3 & heap_bh402_w28_1_d3 & heap_bh402_w28_2_d1 & heap_bh402_w28_3;
   CompressorIn_bh402_46_85(0) <= heap_bh402_w29_0_d3;
      Compressor_bh402_46: Compressor_14_3
      port map ( R => CompressorOut_bh402_46_46,
                 X0 => CompressorIn_bh402_46_84,
                 X1 => CompressorIn_bh402_46_85);
   heap_bh402_w28_4 <= CompressorOut_bh402_46_46(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w29_3 <= CompressorOut_bh402_46_46(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w30_3 <= CompressorOut_bh402_46_46(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_47_86 <= heap_bh402_w47_2_d3 & heap_bh402_w47_0_d3 & heap_bh402_w47_3_d1 & heap_bh402_w47_1_d1;
   CompressorIn_bh402_47_87(0) <= heap_bh402_w48_2_d3;
      Compressor_bh402_47: Compressor_14_3
      port map ( R => CompressorOut_bh402_47_47,
                 X0 => CompressorIn_bh402_47_86,
                 X1 => CompressorIn_bh402_47_87);
   heap_bh402_w47_4 <= CompressorOut_bh402_47_47(0); -- cycle= 3 cp= 0
   heap_bh402_w48_3 <= CompressorOut_bh402_47_47(1); -- cycle= 3 cp= 0
   heap_bh402_w49_3 <= CompressorOut_bh402_47_47(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_48_88 <= heap_bh402_w2_15_d2 & heap_bh402_w2_16_d2 & heap_bh402_w2_8_d1;
   CompressorIn_bh402_48_89 <= heap_bh402_w3_16_d2 & heap_bh402_w3_8_d1;
      Compressor_bh402_48: Compressor_23_3
      port map ( R => CompressorOut_bh402_48_48,
                 X0 => CompressorIn_bh402_48_88,
                 X1 => CompressorIn_bh402_48_89);
   heap_bh402_w2_17 <= CompressorOut_bh402_48_48(0); -- cycle= 3 cp= 0
   heap_bh402_w3_17 <= CompressorOut_bh402_48_48(1); -- cycle= 3 cp= 0
   heap_bh402_w4_16 <= CompressorOut_bh402_48_48(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_49_90 <= heap_bh402_w4_15_d2 & heap_bh402_w4_14_d2 & heap_bh402_w4_6_d1;
   CompressorIn_bh402_49_91 <= heap_bh402_w5_11_d2 & heap_bh402_w5_13_d2;
      Compressor_bh402_49: Compressor_23_3
      port map ( R => CompressorOut_bh402_49_49,
                 X0 => CompressorIn_bh402_49_90,
                 X1 => CompressorIn_bh402_49_91);
   heap_bh402_w4_17 <= CompressorOut_bh402_49_49(0); -- cycle= 3 cp= 0
   heap_bh402_w5_14 <= CompressorOut_bh402_49_49(1); -- cycle= 3 cp= 0
   heap_bh402_w6_16 <= CompressorOut_bh402_49_49(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_50_92 <= heap_bh402_w8_13_d2 & heap_bh402_w8_12_d2 & heap_bh402_w8_5_d1;
   CompressorIn_bh402_50_93 <= heap_bh402_w9_12_d2 & heap_bh402_w9_5_d1;
      Compressor_bh402_50: Compressor_23_3
      port map ( R => CompressorOut_bh402_50_50,
                 X0 => CompressorIn_bh402_50_92,
                 X1 => CompressorIn_bh402_50_93);
   heap_bh402_w8_14 <= CompressorOut_bh402_50_50(0); -- cycle= 3 cp= 0
   heap_bh402_w9_13 <= CompressorOut_bh402_50_50(1); -- cycle= 3 cp= 0
   heap_bh402_w10_9 <= CompressorOut_bh402_50_50(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_51_94 <= heap_bh402_w10_7_d2 & heap_bh402_w10_8_d2 & heap_bh402_w10_3_d1;
   CompressorIn_bh402_51_95 <= heap_bh402_w11_8_d2 & heap_bh402_w11_3_d1;
      Compressor_bh402_51: Compressor_23_3
      port map ( R => CompressorOut_bh402_51_51,
                 X0 => CompressorIn_bh402_51_94,
                 X1 => CompressorIn_bh402_51_95);
   heap_bh402_w10_10 <= CompressorOut_bh402_51_51(0); -- cycle= 3 cp= 0
   heap_bh402_w11_9 <= CompressorOut_bh402_51_51(1); -- cycle= 3 cp= 0
   heap_bh402_w12_8 <= CompressorOut_bh402_51_51(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_52_96 <= heap_bh402_w15_3_d2 & heap_bh402_w15_4_d2 & heap_bh402_w15_2_d1;
   CompressorIn_bh402_52_97 <= heap_bh402_w16_4_d2 & heap_bh402_w16_2_d1;
      Compressor_bh402_52: Compressor_23_3
      port map ( R => CompressorOut_bh402_52_52,
                 X0 => CompressorIn_bh402_52_96,
                 X1 => CompressorIn_bh402_52_97);
   heap_bh402_w15_5 <= CompressorOut_bh402_52_52(0); -- cycle= 3 cp= 0
   heap_bh402_w16_5 <= CompressorOut_bh402_52_52(1); -- cycle= 3 cp= 0
   heap_bh402_w17_4 <= CompressorOut_bh402_52_52(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_53_98 <= heap_bh402_w30_0_d3 & heap_bh402_w30_1_d3 & heap_bh402_w30_2_d1;
   CompressorIn_bh402_53_99 <= heap_bh402_w31_4_d2 & heap_bh402_w31_2_d1;
      Compressor_bh402_53: Compressor_23_3
      port map ( R => CompressorOut_bh402_53_53,
                 X0 => CompressorIn_bh402_53_98,
                 X1 => CompressorIn_bh402_53_99);
   heap_bh402_w30_4 <= CompressorOut_bh402_53_53(0); -- cycle= 3 cp= 0
   heap_bh402_w31_5 <= CompressorOut_bh402_53_53(1); -- cycle= 3 cp= 0
   heap_bh402_w32_4 <= CompressorOut_bh402_53_53(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_54_100 <= heap_bh402_w49_2_d3 & heap_bh402_w49_0_d3 & heap_bh402_w49_1_d1;
   CompressorIn_bh402_54_101 <= heap_bh402_w50_2_d3 & heap_bh402_w50_0_d3;
      Compressor_bh402_54: Compressor_23_3
      port map ( R => CompressorOut_bh402_54_54,
                 X0 => CompressorIn_bh402_54_100,
                 X1 => CompressorIn_bh402_54_101);
   heap_bh402_w49_4 <= CompressorOut_bh402_54_54(0); -- cycle= 3 cp= 0
   heap_bh402_w50_3 <= CompressorOut_bh402_54_54(1); -- cycle= 3 cp= 0
   heap_bh402_w51_3 <= CompressorOut_bh402_54_54(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_55_102 <= heap_bh402_w51_2_d3 & heap_bh402_w51_0_d3 & heap_bh402_w51_1_d1;
   CompressorIn_bh402_55_103 <= heap_bh402_w52_2_d3 & heap_bh402_w52_0_d3;
      Compressor_bh402_55: Compressor_23_3
      port map ( R => CompressorOut_bh402_55_55,
                 X0 => CompressorIn_bh402_55_102,
                 X1 => CompressorIn_bh402_55_103);
   heap_bh402_w51_4 <= CompressorOut_bh402_55_55(0); -- cycle= 3 cp= 0
   heap_bh402_w52_3 <= CompressorOut_bh402_55_55(1); -- cycle= 3 cp= 0
   heap_bh402_w53_3 <= CompressorOut_bh402_55_55(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_56_104 <= heap_bh402_w53_2_d3 & heap_bh402_w53_0_d3 & heap_bh402_w53_1_d1;
   CompressorIn_bh402_56_105 <= heap_bh402_w54_2_d3 & heap_bh402_w54_0_d3;
      Compressor_bh402_56: Compressor_23_3
      port map ( R => CompressorOut_bh402_56_56,
                 X0 => CompressorIn_bh402_56_104,
                 X1 => CompressorIn_bh402_56_105);
   heap_bh402_w53_4 <= CompressorOut_bh402_56_56(0); -- cycle= 3 cp= 0
   heap_bh402_w54_3 <= CompressorOut_bh402_56_56(1); -- cycle= 3 cp= 0
   heap_bh402_w55_3 <= CompressorOut_bh402_56_56(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_57_106 <= heap_bh402_w55_2_d3 & heap_bh402_w55_0_d3 & heap_bh402_w55_1_d1;
   CompressorIn_bh402_57_107 <= heap_bh402_w56_2_d3 & heap_bh402_w56_0_d3;
      Compressor_bh402_57: Compressor_23_3
      port map ( R => CompressorOut_bh402_57_57,
                 X0 => CompressorIn_bh402_57_106,
                 X1 => CompressorIn_bh402_57_107);
   heap_bh402_w55_4 <= CompressorOut_bh402_57_57(0); -- cycle= 3 cp= 0
   heap_bh402_w56_3 <= CompressorOut_bh402_57_57(1); -- cycle= 3 cp= 0
   heap_bh402_w57_2 <= CompressorOut_bh402_57_57(2); -- cycle= 3 cp= 0

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_58_108 <= heap_bh402_w6_15_d2 & heap_bh402_w6_6_d1 & heap_bh402_w6_16;
   CompressorIn_bh402_58_109 <= heap_bh402_w7_12_d2 & heap_bh402_w7_5_d1;
      Compressor_bh402_58: Compressor_23_3
      port map ( R => CompressorOut_bh402_58_58,
                 X0 => CompressorIn_bh402_58_108,
                 X1 => CompressorIn_bh402_58_109);
   heap_bh402_w6_17 <= CompressorOut_bh402_58_58(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w7_13 <= CompressorOut_bh402_58_58(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w8_15 <= CompressorOut_bh402_58_58(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_59_110 <= heap_bh402_w12_7_d2 & heap_bh402_w12_3_d1 & heap_bh402_w12_8;
   CompressorIn_bh402_59_111 <= heap_bh402_w13_6_d2 & heap_bh402_w13_2_d1;
      Compressor_bh402_59: Compressor_23_3
      port map ( R => CompressorOut_bh402_59_59,
                 X0 => CompressorIn_bh402_59_110,
                 X1 => CompressorIn_bh402_59_111);
   heap_bh402_w12_9 <= CompressorOut_bh402_59_59(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w13_7 <= CompressorOut_bh402_59_59(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w14_6 <= CompressorOut_bh402_59_59(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_60_112 <= heap_bh402_w17_3_d2 & heap_bh402_w17_2_d1 & heap_bh402_w17_4;
   CompressorIn_bh402_60_113 <= heap_bh402_w18_4_d1 & heap_bh402_w18_2_d1;
      Compressor_bh402_60: Compressor_23_3
      port map ( R => CompressorOut_bh402_60_60,
                 X0 => CompressorIn_bh402_60_112,
                 X1 => CompressorIn_bh402_60_113);
   heap_bh402_w17_5 <= CompressorOut_bh402_60_60(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w18_5 <= CompressorOut_bh402_60_60(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w19_4 <= CompressorOut_bh402_60_60(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_61_114 <= heap_bh402_w29_1_d3 & heap_bh402_w29_2_d1 & heap_bh402_w29_3;
   CompressorIn_bh402_61_115 <= heap_bh402_w30_4 & heap_bh402_w30_3;
      Compressor_bh402_61: Compressor_23_3
      port map ( R => CompressorOut_bh402_61_61,
                 X0 => CompressorIn_bh402_61_114,
                 X1 => CompressorIn_bh402_61_115);
   heap_bh402_w29_4 <= CompressorOut_bh402_61_61(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w30_5 <= CompressorOut_bh402_61_61(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w31_6 <= CompressorOut_bh402_61_61(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_62_116 <= heap_bh402_w32_3_d2 & heap_bh402_w32_1_d1 & heap_bh402_w32_4;
   CompressorIn_bh402_62_117 <= heap_bh402_w33_4_d2 & heap_bh402_w33_1_d1;
      Compressor_bh402_62: Compressor_23_3
      port map ( R => CompressorOut_bh402_62_62,
                 X0 => CompressorIn_bh402_62_116,
                 X1 => CompressorIn_bh402_62_117);
   heap_bh402_w32_5 <= CompressorOut_bh402_62_62(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w33_5 <= CompressorOut_bh402_62_62(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w34_4 <= CompressorOut_bh402_62_62(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_63_118 <= heap_bh402_w48_0_d3 & heap_bh402_w48_1_d1 & heap_bh402_w48_3;
   CompressorIn_bh402_63_119 <= heap_bh402_w49_4 & heap_bh402_w49_3;
      Compressor_bh402_63: Compressor_23_3
      port map ( R => CompressorOut_bh402_63_63,
                 X0 => CompressorIn_bh402_63_118,
                 X1 => CompressorIn_bh402_63_119);
   heap_bh402_w48_4 <= CompressorOut_bh402_63_63(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w49_5 <= CompressorOut_bh402_63_63(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w50_4 <= CompressorOut_bh402_63_63(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_64_120 <= heap_bh402_w57_0_d3 & heap_bh402_w57_1_d1 & heap_bh402_w57_2;
   CompressorIn_bh402_64_121 <= heap_bh402_w58_1_d3 & heap_bh402_w58_0_d3;
      Compressor_bh402_64: Compressor_23_3
      port map ( R => CompressorOut_bh402_64_64,
                 X0 => CompressorIn_bh402_64_120,
                 X1 => CompressorIn_bh402_64_121);
   heap_bh402_w57_3 <= CompressorOut_bh402_64_64(0); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w58_2 <= CompressorOut_bh402_64_64(1); -- cycle= 3 cp= 5.3072e-10
   heap_bh402_w59_2 <= CompressorOut_bh402_64_64(2); -- cycle= 3 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_65_122 <= heap_bh402_w19_3_d1 & heap_bh402_w19_2_d1 & heap_bh402_w19_4;
   CompressorIn_bh402_65_123 <= heap_bh402_w20_4_d1 & heap_bh402_w20_2_d1;
      Compressor_bh402_65: Compressor_23_3
      port map ( R => CompressorOut_bh402_65_65,
                 X0 => CompressorIn_bh402_65_122,
                 X1 => CompressorIn_bh402_65_123);
   heap_bh402_w19_5 <= CompressorOut_bh402_65_65(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w20_5 <= CompressorOut_bh402_65_65(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w21_4 <= CompressorOut_bh402_65_65(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_66_124 <= heap_bh402_w34_3_d2 & heap_bh402_w34_1_d1 & heap_bh402_w34_4;
   CompressorIn_bh402_66_125 <= heap_bh402_w35_4_d2 & heap_bh402_w35_1_d1;
      Compressor_bh402_66: Compressor_23_3
      port map ( R => CompressorOut_bh402_66_66,
                 X0 => CompressorIn_bh402_66_124,
                 X1 => CompressorIn_bh402_66_125);
   heap_bh402_w34_5 <= CompressorOut_bh402_66_66(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w35_5 <= CompressorOut_bh402_66_66(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w36_4 <= CompressorOut_bh402_66_66(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_67_126 <= heap_bh402_w50_1_d1 & heap_bh402_w50_3 & heap_bh402_w50_4;
   CompressorIn_bh402_67_127 <= heap_bh402_w51_4 & heap_bh402_w51_3;
      Compressor_bh402_67: Compressor_23_3
      port map ( R => CompressorOut_bh402_67_67,
                 X0 => CompressorIn_bh402_67_126,
                 X1 => CompressorIn_bh402_67_127);
   heap_bh402_w50_5 <= CompressorOut_bh402_67_67(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w51_5 <= CompressorOut_bh402_67_67(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w52_4 <= CompressorOut_bh402_67_67(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_68_128 <= heap_bh402_w59_1_d3 & heap_bh402_w59_0_d3 & heap_bh402_w59_2;
   CompressorIn_bh402_68_129 <= heap_bh402_w60_1_d3 & heap_bh402_w60_0_d3;
      Compressor_bh402_68: Compressor_23_3
      port map ( R => CompressorOut_bh402_68_68,
                 X0 => CompressorIn_bh402_68_128,
                 X1 => CompressorIn_bh402_68_129);
   heap_bh402_w59_3 <= CompressorOut_bh402_68_68(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w60_2 <= CompressorOut_bh402_68_68(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w61_2 <= CompressorOut_bh402_68_68(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_69_130 <= heap_bh402_w14_5_d2 & heap_bh402_w14_2_d1 & heap_bh402_w14_6;
   CompressorIn_bh402_69_131(0) <= heap_bh402_w15_5;
      Compressor_bh402_69: Compressor_13_3
      port map ( R => CompressorOut_bh402_69_69,
                 X0 => CompressorIn_bh402_69_130,
                 X1 => CompressorIn_bh402_69_131);
   heap_bh402_w14_7 <= CompressorOut_bh402_69_69(0); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w15_6 <= CompressorOut_bh402_69_69(1); -- cycle= 3 cp= 1.06144e-09
   heap_bh402_w16_6 <= CompressorOut_bh402_69_69(2); -- cycle= 3 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_70_132 <= heap_bh402_w21_3_d1 & heap_bh402_w21_2_d1 & heap_bh402_w21_4;
   CompressorIn_bh402_70_133 <= heap_bh402_w22_4_d1 & heap_bh402_w22_2_d1;
      Compressor_bh402_70: Compressor_23_3
      port map ( R => CompressorOut_bh402_70_70,
                 X0 => CompressorIn_bh402_70_132,
                 X1 => CompressorIn_bh402_70_133);
   heap_bh402_w21_5 <= CompressorOut_bh402_70_70(0); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w22_5 <= CompressorOut_bh402_70_70(1); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w23_4 <= CompressorOut_bh402_70_70(2); -- cycle= 3 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_71_134 <= heap_bh402_w36_3_d2 & heap_bh402_w36_1_d1 & heap_bh402_w36_4;
   CompressorIn_bh402_71_135 <= heap_bh402_w37_4_d2 & heap_bh402_w37_1_d1;
      Compressor_bh402_71: Compressor_23_3
      port map ( R => CompressorOut_bh402_71_71,
                 X0 => CompressorIn_bh402_71_134,
                 X1 => CompressorIn_bh402_71_135);
   heap_bh402_w36_5 <= CompressorOut_bh402_71_71(0); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w37_5 <= CompressorOut_bh402_71_71(1); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w38_4 <= CompressorOut_bh402_71_71(2); -- cycle= 3 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_72_136 <= heap_bh402_w52_1_d1 & heap_bh402_w52_3 & heap_bh402_w52_4;
   CompressorIn_bh402_72_137 <= heap_bh402_w53_4 & heap_bh402_w53_3;
      Compressor_bh402_72: Compressor_23_3
      port map ( R => CompressorOut_bh402_72_72,
                 X0 => CompressorIn_bh402_72_136,
                 X1 => CompressorIn_bh402_72_137);
   heap_bh402_w52_5 <= CompressorOut_bh402_72_72(0); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w53_5 <= CompressorOut_bh402_72_72(1); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w54_4 <= CompressorOut_bh402_72_72(2); -- cycle= 3 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   CompressorIn_bh402_73_138 <= heap_bh402_w61_1_d3 & heap_bh402_w61_0_d3 & heap_bh402_w61_2;
   CompressorIn_bh402_73_139 <= heap_bh402_w62_1_d3 & heap_bh402_w62_0_d3;
      Compressor_bh402_73: Compressor_23_3
      port map ( R => CompressorOut_bh402_73_73,
                 X0 => CompressorIn_bh402_73_138,
                 X1 => CompressorIn_bh402_73_139);
   heap_bh402_w61_3 <= CompressorOut_bh402_73_73(0); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w62_2 <= CompressorOut_bh402_73_73(1); -- cycle= 3 cp= 1.59216e-09
   heap_bh402_w63_2 <= CompressorOut_bh402_73_73(2); -- cycle= 3 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 3----------------
   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_74_140 <= heap_bh402_w23_3_d2 & heap_bh402_w23_2_d2 & heap_bh402_w23_4_d1;
   CompressorIn_bh402_74_141 <= heap_bh402_w24_4_d2 & heap_bh402_w24_2_d2;
      Compressor_bh402_74: Compressor_23_3
      port map ( R => CompressorOut_bh402_74_74,
                 X0 => CompressorIn_bh402_74_140,
                 X1 => CompressorIn_bh402_74_141);
   heap_bh402_w23_5 <= CompressorOut_bh402_74_74(0); -- cycle= 4 cp= 0
   heap_bh402_w24_5 <= CompressorOut_bh402_74_74(1); -- cycle= 4 cp= 0
   heap_bh402_w25_4 <= CompressorOut_bh402_74_74(2); -- cycle= 4 cp= 0

   ----------------Synchro barrier, entering cycle 3----------------
   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_75_142 <= heap_bh402_w38_3_d3 & heap_bh402_w38_1_d2 & heap_bh402_w38_4_d1;
   CompressorIn_bh402_75_143 <= heap_bh402_w39_4_d2 & heap_bh402_w39_1_d2;
      Compressor_bh402_75: Compressor_23_3
      port map ( R => CompressorOut_bh402_75_75,
                 X0 => CompressorIn_bh402_75_142,
                 X1 => CompressorIn_bh402_75_143);
   heap_bh402_w38_5 <= CompressorOut_bh402_75_75(0); -- cycle= 4 cp= 0
   heap_bh402_w39_5 <= CompressorOut_bh402_75_75(1); -- cycle= 4 cp= 0
   heap_bh402_w40_4 <= CompressorOut_bh402_75_75(2); -- cycle= 4 cp= 0

   ----------------Synchro barrier, entering cycle 3----------------
   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_76_144 <= heap_bh402_w54_1_d2 & heap_bh402_w54_3_d1 & heap_bh402_w54_4_d1;
   CompressorIn_bh402_76_145 <= heap_bh402_w55_4_d1 & heap_bh402_w55_3_d1;
      Compressor_bh402_76: Compressor_23_3
      port map ( R => CompressorOut_bh402_76_76,
                 X0 => CompressorIn_bh402_76_144,
                 X1 => CompressorIn_bh402_76_145);
   heap_bh402_w54_5 <= CompressorOut_bh402_76_76(0); -- cycle= 4 cp= 0
   heap_bh402_w55_5 <= CompressorOut_bh402_76_76(1); -- cycle= 4 cp= 0
   heap_bh402_w56_4 <= CompressorOut_bh402_76_76(2); -- cycle= 4 cp= 0

   ----------------Synchro barrier, entering cycle 3----------------
   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_77_146 <= heap_bh402_w63_1_d4 & heap_bh402_w63_0_d4 & heap_bh402_w63_2_d1;
   CompressorIn_bh402_77_147 <= heap_bh402_w64_1_d4 & heap_bh402_w64_0_d4;
      Compressor_bh402_77: Compressor_23_3
      port map ( R => CompressorOut_bh402_77_77,
                 X0 => CompressorIn_bh402_77_146,
                 X1 => CompressorIn_bh402_77_147);
   heap_bh402_w63_3 <= CompressorOut_bh402_77_77(0); -- cycle= 4 cp= 0
   heap_bh402_w64_2 <= CompressorOut_bh402_77_77(1); -- cycle= 4 cp= 0
   heap_bh402_w65_2 <= CompressorOut_bh402_77_77(2); -- cycle= 4 cp= 0

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_78_148 <= heap_bh402_w25_3_d2 & heap_bh402_w25_2_d2 & heap_bh402_w25_4;
   CompressorIn_bh402_78_149 <= heap_bh402_w26_2_d2 & heap_bh402_w26_4_d1;
      Compressor_bh402_78: Compressor_23_3
      port map ( R => CompressorOut_bh402_78_78,
                 X0 => CompressorIn_bh402_78_148,
                 X1 => CompressorIn_bh402_78_149);
   heap_bh402_w25_5 <= CompressorOut_bh402_78_78(0); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w26_5 <= CompressorOut_bh402_78_78(1); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w27_4 <= CompressorOut_bh402_78_78(2); -- cycle= 4 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_79_150 <= heap_bh402_w40_3_d2 & heap_bh402_w40_1_d2 & heap_bh402_w40_4;
   CompressorIn_bh402_79_151 <= heap_bh402_w41_4_d2 & heap_bh402_w41_1_d2;
      Compressor_bh402_79: Compressor_23_3
      port map ( R => CompressorOut_bh402_79_79,
                 X0 => CompressorIn_bh402_79_150,
                 X1 => CompressorIn_bh402_79_151);
   heap_bh402_w40_5 <= CompressorOut_bh402_79_79(0); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w41_5 <= CompressorOut_bh402_79_79(1); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w42_4 <= CompressorOut_bh402_79_79(2); -- cycle= 4 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_80_152 <= heap_bh402_w56_1_d2 & heap_bh402_w56_3_d1 & heap_bh402_w56_4;
   CompressorIn_bh402_80_153(0) <= heap_bh402_w57_3_d1;
      Compressor_bh402_80: Compressor_13_3
      port map ( R => CompressorOut_bh402_80_80,
                 X0 => CompressorIn_bh402_80_152,
                 X1 => CompressorIn_bh402_80_153);
   heap_bh402_w56_5 <= CompressorOut_bh402_80_80(0); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w57_4 <= CompressorOut_bh402_80_80(1); -- cycle= 4 cp= 5.3072e-10
   heap_bh402_w58_3 <= CompressorOut_bh402_80_80(2); -- cycle= 4 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_81_154 <= heap_bh402_w65_1_d4 & heap_bh402_w65_0_d4 & heap_bh402_w65_2;
      Compressor_bh402_81: Compressor_3_2
      port map ( R => CompressorOut_bh402_81_81,
                 X0 => CompressorIn_bh402_81_154);
   heap_bh402_w65_3 <= CompressorOut_bh402_81_81(0); -- cycle= 4 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_82_155 <= heap_bh402_w42_3_d2 & heap_bh402_w42_1_d2 & heap_bh402_w42_4;
   CompressorIn_bh402_82_156 <= heap_bh402_w43_4_d2 & heap_bh402_w43_1_d2;
      Compressor_bh402_82: Compressor_23_3
      port map ( R => CompressorOut_bh402_82_82,
                 X0 => CompressorIn_bh402_82_155,
                 X1 => CompressorIn_bh402_82_156);
   heap_bh402_w42_5 <= CompressorOut_bh402_82_82(0); -- cycle= 4 cp= 1.06144e-09
   heap_bh402_w43_5 <= CompressorOut_bh402_82_82(1); -- cycle= 4 cp= 1.06144e-09
   heap_bh402_w44_4 <= CompressorOut_bh402_82_82(2); -- cycle= 4 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_83_157 <= heap_bh402_w27_2_d2 & heap_bh402_w27_3_d1 & heap_bh402_w27_4;
   CompressorIn_bh402_83_158(0) <= heap_bh402_w28_4_d1;
      Compressor_bh402_83: Compressor_13_3
      port map ( R => CompressorOut_bh402_83_83,
                 X0 => CompressorIn_bh402_83_157,
                 X1 => CompressorIn_bh402_83_158);
   heap_bh402_w27_5 <= CompressorOut_bh402_83_83(0); -- cycle= 4 cp= 1.06144e-09
   heap_bh402_w28_5 <= CompressorOut_bh402_83_83(1); -- cycle= 4 cp= 1.06144e-09
   heap_bh402_w29_5 <= CompressorOut_bh402_83_83(2); -- cycle= 4 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 4----------------
   CompressorIn_bh402_84_159 <= heap_bh402_w44_3_d2 & heap_bh402_w44_1_d2 & heap_bh402_w44_4;
   CompressorIn_bh402_84_160 <= heap_bh402_w45_4_d2 & heap_bh402_w45_1_d2;
      Compressor_bh402_84: Compressor_23_3
      port map ( R => CompressorOut_bh402_84_84,
                 X0 => CompressorIn_bh402_84_159,
                 X1 => CompressorIn_bh402_84_160);
   heap_bh402_w44_5 <= CompressorOut_bh402_84_84(0); -- cycle= 4 cp= 1.59216e-09
   heap_bh402_w45_5 <= CompressorOut_bh402_84_84(1); -- cycle= 4 cp= 1.59216e-09
   heap_bh402_w46_4 <= CompressorOut_bh402_84_84(2); -- cycle= 4 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 4----------------
   ----------------Synchro barrier, entering cycle 5----------------
   CompressorIn_bh402_85_161 <= heap_bh402_w46_3_d3 & heap_bh402_w46_1_d3 & heap_bh402_w46_4_d1;
   CompressorIn_bh402_85_162(0) <= heap_bh402_w47_4_d2;
      Compressor_bh402_85: Compressor_13_3
      port map ( R => CompressorOut_bh402_85_85,
                 X0 => CompressorIn_bh402_85_161,
                 X1 => CompressorIn_bh402_85_162);
   heap_bh402_w46_5 <= CompressorOut_bh402_85_85(0); -- cycle= 5 cp= 0
   heap_bh402_w47_5 <= CompressorOut_bh402_85_85(1); -- cycle= 5 cp= 0
   heap_bh402_w48_5 <= CompressorOut_bh402_85_85(2); -- cycle= 5 cp= 0
   ----------------Synchro barrier, entering cycle 5----------------
   ----------------Synchro barrier, entering cycle 6----------------
   finalAdderIn0_bh402 <= "0" & heap_bh402_w65_3_d2 & heap_bh402_w64_2_d2 & heap_bh402_w63_3_d2 & heap_bh402_w62_2_d3 & heap_bh402_w61_3_d3 & heap_bh402_w60_2_d3 & heap_bh402_w59_3_d3 & heap_bh402_w58_2_d3 & heap_bh402_w57_4_d2 & heap_bh402_w56_5_d2 & heap_bh402_w55_5_d2 & heap_bh402_w54_5_d2 & heap_bh402_w53_5_d3 & heap_bh402_w52_5_d3 & heap_bh402_w51_5_d3 & heap_bh402_w50_5_d3 & heap_bh402_w49_5_d3 & heap_bh402_w48_4_d3 & heap_bh402_w47_5_d1 & heap_bh402_w46_5_d1 & heap_bh402_w45_5_d2 & heap_bh402_w44_5_d2 & heap_bh402_w43_5_d2 & heap_bh402_w42_5_d2 & heap_bh402_w41_5_d2 & heap_bh402_w40_5_d2 & heap_bh402_w39_5_d2 & heap_bh402_w38_5_d2 & heap_bh402_w37_5_d3 & heap_bh402_w36_5_d3 & heap_bh402_w35_5_d3 & heap_bh402_w34_5_d3 & heap_bh402_w33_5_d3 & heap_bh402_w32_5_d3 & heap_bh402_w31_5_d3 & heap_bh402_w30_5_d3 & heap_bh402_w29_4_d3 & heap_bh402_w28_5_d2 & heap_bh402_w27_5_d2 & heap_bh402_w26_5_d2 & heap_bh402_w25_5_d2 & heap_bh402_w24_5_d2 & heap_bh402_w23_5_d2 & heap_bh402_w22_5_d3 & heap_bh402_w21_5_d3 & heap_bh402_w20_5_d3 & heap_bh402_w19_5_d3 & heap_bh402_w18_5_d3 & heap_bh402_w17_5_d3 & heap_bh402_w16_5_d3 & heap_bh402_w15_6_d3 & heap_bh402_w14_7_d3 & heap_bh402_w13_7_d3 & heap_bh402_w12_9_d3 & heap_bh402_w11_9_d3 & heap_bh402_w10_10_d3 & heap_bh402_w9_13_d3 & heap_bh402_w8_14_d3 & heap_bh402_w7_13_d3 & heap_bh402_w6_17_d3 & heap_bh402_w5_6_d4 & heap_bh402_w4_17_d3 & heap_bh402_w3_17_d3 & heap_bh402_w2_17_d3 & heap_bh402_w1_13_d5 & heap_bh402_w0_13_d5;
   finalAdderIn1_bh402 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh402_w58_3_d2 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh402_w48_5_d1 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh402_w31_6_d3 & '0' & heap_bh402_w29_5_d2 & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh402_w16_6_d3 & '0' & '0' & '0' & '0' & '0' & heap_bh402_w10_9_d3 & '0' & heap_bh402_w8_15_d3 & '0' & '0' & heap_bh402_w5_14_d3 & heap_bh402_w4_16_d3 & '0' & '0' & heap_bh402_w1_8_d4 & heap_bh402_w0_10_d4;
   finalAdderCin_bh402 <= '0';
      Adder_final402_0: IntAdder_67_f400_uid625  -- pipelineDepth=1 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh402,
                 R => finalAdderOut_bh402,
                 X => finalAdderIn0_bh402,
                 Y => finalAdderIn1_bh402);
   ----------------Synchro barrier, entering cycle 7----------------
   -- concatenate all the compressed chunks
   CompressionResult402 <= finalAdderOut_bh402;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult402(65 downto 7);
end architecture;

--------------------------------------------------------------------------------
--                        FixHornerEvaluator_F400_uid8
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: F. de Dinechin (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 23 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixHornerEvaluator_F400_uid8 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(43 downto 0);
          A0 : in  std_logic_vector(58 downto 0);
          A1 : in  std_logic_vector(47 downto 0);
          A2 : in  std_logic_vector(36 downto 0);
          A3 : in  std_logic_vector(26 downto 0);
          A4 : in  std_logic_vector(17 downto 0);
          R : out  std_logic_vector(53 downto 0)   );
end entity;

architecture arch of FixHornerEvaluator_F400_uid8 is
   component FixMultAdd_18x18p27r27signed_F400_uid10 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(17 downto 0);
             Y : in  std_logic_vector(17 downto 0);
             A : in  std_logic_vector(26 downto 0);
             R : out  std_logic_vector(26 downto 0)   );
   end component;

   component FixMultAdd_27x27p37r37signed_F400_uid43 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(26 downto 0);
             Y : in  std_logic_vector(26 downto 0);
             A : in  std_logic_vector(36 downto 0);
             R : out  std_logic_vector(36 downto 0)   );
   end component;

   component FixMultAdd_37x37p48r48signed_F400_uid244 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(36 downto 0);
             Y : in  std_logic_vector(36 downto 0);
             A : in  std_logic_vector(47 downto 0);
             R : out  std_logic_vector(47 downto 0)   );
   end component;

   component FixMultAdd_44x48p59r59signed_F400_uid401 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(43 downto 0);
             Y : in  std_logic_vector(47 downto 0);
             A : in  std_logic_vector(58 downto 0);
             R : out  std_logic_vector(58 downto 0)   );
   end component;

signal Xs, Xs_d1, Xs_d2, Xs_d3, Xs_d4, Xs_d5, Xs_d6, Xs_d7, Xs_d8, Xs_d9, Xs_d10, Xs_d11, Xs_d12, Xs_d13, Xs_d14, Xs_d15 :  signed(0+43 downto 0);
signal As0, As0_d1, As0_d2, As0_d3, As0_d4, As0_d5, As0_d6, As0_d7, As0_d8, As0_d9, As0_d10, As0_d11, As0_d12, As0_d13, As0_d14, As0_d15 :  signed(1+57 downto 0);
signal As1, As1_d1, As1_d2, As1_d3, As1_d4, As1_d5, As1_d6, As1_d7, As1_d8 :  signed(-10+57 downto 0);
signal As2, As2_d1, As2_d2, As2_d3, As2_d4 :  signed(-21+57 downto 0);
signal As3 :  signed(-31+57 downto 0);
signal As4 :  signed(-40+57 downto 0);
signal Sigma4 :  signed(-40+57 downto 0);
signal XsTrunc3 :  signed(0+17 downto 0);
signal Sigma3_slv :  std_logic_vector(26 downto 0);
signal Sigma3, Sigma3_d1 :  signed(-31+57 downto 0);
signal XsTrunc2 :  signed(0+26 downto 0);
signal Sigma2_slv :  std_logic_vector(36 downto 0);
signal Sigma2, Sigma2_d1 :  signed(-21+57 downto 0);
signal XsTrunc1 :  signed(0+36 downto 0);
signal Sigma1_slv :  std_logic_vector(47 downto 0);
signal Sigma1, Sigma1_d1 :  signed(-10+57 downto 0);
signal XsTrunc0 :  signed(0+43 downto 0);
signal Sigma0_slv :  std_logic_vector(58 downto 0);
signal Sigma0, Sigma0_d1 :  signed(1+57 downto 0);
signal Ys :  signed(1+52 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Xs_d1 <=  Xs;
            Xs_d2 <=  Xs_d1;
            Xs_d3 <=  Xs_d2;
            Xs_d4 <=  Xs_d3;
            Xs_d5 <=  Xs_d4;
            Xs_d6 <=  Xs_d5;
            Xs_d7 <=  Xs_d6;
            Xs_d8 <=  Xs_d7;
            Xs_d9 <=  Xs_d8;
            Xs_d10 <=  Xs_d9;
            Xs_d11 <=  Xs_d10;
            Xs_d12 <=  Xs_d11;
            Xs_d13 <=  Xs_d12;
            Xs_d14 <=  Xs_d13;
            Xs_d15 <=  Xs_d14;
            As0_d1 <=  As0;
            As0_d2 <=  As0_d1;
            As0_d3 <=  As0_d2;
            As0_d4 <=  As0_d3;
            As0_d5 <=  As0_d4;
            As0_d6 <=  As0_d5;
            As0_d7 <=  As0_d6;
            As0_d8 <=  As0_d7;
            As0_d9 <=  As0_d8;
            As0_d10 <=  As0_d9;
            As0_d11 <=  As0_d10;
            As0_d12 <=  As0_d11;
            As0_d13 <=  As0_d12;
            As0_d14 <=  As0_d13;
            As0_d15 <=  As0_d14;
            As1_d1 <=  As1;
            As1_d2 <=  As1_d1;
            As1_d3 <=  As1_d2;
            As1_d4 <=  As1_d3;
            As1_d5 <=  As1_d4;
            As1_d6 <=  As1_d5;
            As1_d7 <=  As1_d6;
            As1_d8 <=  As1_d7;
            As2_d1 <=  As2;
            As2_d2 <=  As2_d1;
            As2_d3 <=  As2_d2;
            As2_d4 <=  As2_d3;
            Sigma3_d1 <=  Sigma3;
            Sigma2_d1 <=  Sigma2;
            Sigma1_d1 <=  Sigma1;
            Sigma0_d1 <=  Sigma0;
         end if;
      end process;
   Xs <= signed(X);
   As0 <= signed(A0);
   As1 <= signed(A1);
   As2 <= signed(A2);
   As3 <= signed(A3);
   As4 <= signed(A4);
   Sigma4 <= As4;
   XsTrunc3 <= Xs(43 downto 26); -- fix resize from (0, -43) to (0, -17)
   Step3: FixMultAdd_18x18p27r27signed_F400_uid10  -- pipelineDepth=3 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As3),
                 R => Sigma3_slv,
                 X => std_logic_vector(XsTrunc3),
                 Y => std_logic_vector(Sigma4));
   ----------------Synchro barrier, entering cycle 3----------------
   Sigma3 <= signed(Sigma3_slv);
   ----------------Synchro barrier, entering cycle 4----------------
   XsTrunc2 <= Xs_d4(43 downto 17); -- fix resize from (0, -43) to (0, -26)
   Step2: FixMultAdd_27x27p37r37signed_F400_uid43  -- pipelineDepth=3 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As2_d4),
                 R => Sigma2_slv,
                 X => std_logic_vector(XsTrunc2),
                 Y => std_logic_vector(Sigma3_d1));
   ----------------Synchro barrier, entering cycle 7----------------
   Sigma2 <= signed(Sigma2_slv);
   ----------------Synchro barrier, entering cycle 8----------------
   XsTrunc1 <= Xs_d8(43 downto 7); -- fix resize from (0, -43) to (0, -36)
   Step1: FixMultAdd_37x37p48r48signed_F400_uid244  -- pipelineDepth=6 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As1_d8),
                 R => Sigma1_slv,
                 X => std_logic_vector(XsTrunc1),
                 Y => std_logic_vector(Sigma2_d1));
   ----------------Synchro barrier, entering cycle 14----------------
   Sigma1 <= signed(Sigma1_slv);
   ----------------Synchro barrier, entering cycle 15----------------
   XsTrunc0 <= Xs_d15(43 downto 0); -- fix resize from (0, -43) to (0, -43)
   Step0: FixMultAdd_44x48p59r59signed_F400_uid401  -- pipelineDepth=7 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As0_d15),
                 R => Sigma0_slv,
                 X => std_logic_vector(XsTrunc0),
                 Y => std_logic_vector(Sigma1_d1));
   ----------------Synchro barrier, entering cycle 22----------------
   Sigma0 <= signed(Sigma0_slv);
   ----------------Synchro barrier, entering cycle 23----------------
   Ys <= Sigma0_d1(58 downto 5); -- fix resize from (1, -57) to (1, -52)
   R <= std_logic_vector(Ys);
end architecture;

--------------------------------------------------------------------------------
--                    FixFunctionByPiecewisePoly_F400_uid2
-- Evaluator for sqrt(1+x) on [0,1) for lsbIn=-52 (wIn=52), msbout=1, lsbOut=-52
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 24 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixFunctionByPiecewisePoly_F400_uid2 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(51 downto 0);
          Y : out  std_logic_vector(53 downto 0)   );
end entity;

architecture arch of FixFunctionByPiecewisePoly_F400_uid2 is
   component GenericTable_8_184_F400_uid4 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(7 downto 0);
             Y : out  std_logic_vector(183 downto 0)   );
   end component;

   component FixHornerEvaluator_F400_uid8 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(43 downto 0);
             A0 : in  std_logic_vector(58 downto 0);
             A1 : in  std_logic_vector(47 downto 0);
             A2 : in  std_logic_vector(36 downto 0);
             A3 : in  std_logic_vector(26 downto 0);
             A4 : in  std_logic_vector(17 downto 0);
             R : out  std_logic_vector(53 downto 0)   );
   end component;

signal A :  std_logic_vector(7 downto 0);
signal Z :  std_logic_vector(43 downto 0);
signal Zs, Zs_d1 :  std_logic_vector(43 downto 0);
signal Coeffs :  std_logic_vector(183 downto 0);
signal A4 :  std_logic_vector(17 downto 0);
signal A3 :  std_logic_vector(26 downto 0);
signal A2 :  std_logic_vector(36 downto 0);
signal A1 :  std_logic_vector(47 downto 0);
signal A0 :  std_logic_vector(58 downto 0);
signal Ys :  std_logic_vector(53 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Zs_d1 <=  Zs;
         end if;
      end process;
   A <= X(51 downto 44);
   Z <= X(43 downto 0);
   Zs <= (not Z(43)) & Z(42 downto 0); -- centering the interval
   coeffTable: GenericTable_8_184_F400_uid4  -- pipelineDepth=1 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => A,
                 Y => Coeffs);

   ----------------Synchro barrier, entering cycle 1----------------
   A4 <= "1" & Coeffs(16 downto 0);
   A3 <= "0" & Coeffs(42 downto 17);
   A2 <= "1" & Coeffs(78 downto 43);
   A1 <= "0" & Coeffs(125 downto 79);
   A0 <= "0" & Coeffs(183 downto 126);
   horner: FixHornerEvaluator_F400_uid8  -- pipelineDepth=23 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A0 => A0,
                 A1 => A1,
                 A2 => A2,
                 A3 => A3,
                 A4 => A4,
                 R => Ys,
                 X => Zs_d1);

   ----------------Synchro barrier, entering cycle 24----------------
   Y <= std_logic_vector(Ys);
end architecture;

