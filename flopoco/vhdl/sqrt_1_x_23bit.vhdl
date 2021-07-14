--------------------------------------------------------------------------------
--                    SmallMultTableP3x3r6XuYu_F400_uid17
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYu_F400_uid17 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYu_F400_uid17 is
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
--                    SmallMultTableP3x3r6XsYu_F400_uid19
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYu_F400_uid19 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYu_F400_uid19 is
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
--                    SmallMultTableP3x3r6XuYs_F400_uid21
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XuYs_F400_uid21 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XuYs_F400_uid21 is
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
--                    SmallMultTableP3x3r6XsYs_F400_uid23
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity SmallMultTableP3x3r6XsYs_F400_uid23 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(5 downto 0)   );
end entity;

architecture arch of SmallMultTableP3x3r6XsYs_F400_uid23 is
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
--                        GenericTable_6_54_F400_uid4
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2007-2012)
--------------------------------------------------------------------------------
library ieee; 
use ieee.std_logic_1164.all;
library work;
entity GenericTable_6_54_F400_uid4 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(5 downto 0);
          Y : out  std_logic_vector(53 downto 0)   );
end entity;

architecture arch of GenericTable_6_54_F400_uid4 is
signal TableOut :  std_logic_vector(53 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
  with X select TableOut <= 
   "100000000111111111000000110111111110000000111000000110" when "000000",
   "100000010111110111000111001111111010000110110000010010" when "000001",
   "100000100111100111011111000111110110010010011000011101" when "000010",
   "100000110111010000010011011111110010100011011000101000" when "000011",
   "100001000110110001101111001111101110111001100000110010" when "000100",
   "100001010110001011111100011111101011010100111000111100" when "000101",
   "100001100101011111000101010111100111110101000001000100" when "000110",
   "100001110100101011010011001111100100011010000001001110" when "000111",
   "100010000011110000101111011111100001000011011001011000" when "001000",
   "100010010010101111100011000111011101110001010001100000" when "001001",
   "100010100001100111110110101111011010100011011001101000" when "001010",
   "100010110000011001110010100111010111011001100001110001" when "001011",
   "100010111111000101011110111111010100010011101001111000" when "001100",
   "100011001101101011000011010111010001010001101010000000" when "001101",
   "100011011100001010100111100111001110010011011010000111" when "001110",
   "100011101010100100010010111111001011011000110010001110" when "001111",
   "100011111000111000001100010111001000100001100010010110" when "010000",
   "100100000111000110011010110111000101101101110010011100" when "010001",
   "100100010101001111000101000111000010111101010010100010" when "010010",
   "100100100011010010010001011111000000001111111010101000" when "010011",
   "100100110001010000000110001110111101100101101010101110" when "010100",
   "100100111111001000101001011110111010111110011010110100" when "010101",
   "100101001100111100000001000110111000011010000010111011" when "010110",
   "100101011010101010010010111110110101111000100010111111" when "010111",
   "100101101000010011100100011110110011011001101011000101" when "011000",
   "100101110101110111111011000110110000111101101011001010" when "011001",
   "100110000011010111011100000110101110100100000011010000" when "011010",
   "100110010000110010001100100110101100001101000011010101" when "011011",
   "100110011110001000010001100110101001111000100011011010" when "011100",
   "100110101011011001101111111110100111100110011011011110" when "011101",
   "100110111000100110101100010110100101010110100011100011" when "011110",
   "100111000101101111001011010110100011001001000011100111" when "011111",
   "100111010010110011010001011110100000111101101011101011" when "100000",
   "100111011111110011000010111110011110110100011011110000" when "100001",
   "100111101100101110100100001110011100101101010011110100" when "100010",
   "100111111001100101111001010110011010101000010011111000" when "100011",
   "101000000110011001000110010110011000100101001011111011" when "100100",
   "101000010011001000001111000110010110100100000011111111" when "100101",
   "101000011111110011010111011110010100100100110100000011" when "100110",
   "101000101100011010100011001110010010100111011100000111" when "100111",
   "101000111000111101110110000110010000101011111100001010" when "101000",
   "101001000101011101010011011110001110110010000100001110" when "101001",
   "101001010001111000111110111110001100111001111100010010" when "101010",
   "101001011110010000111011111110001011000011100100010101" when "101011",
   "101001101010100101001101101110001001001110101100011000" when "101100",
   "101001110110110101110111100110000111011011100100011011" when "101101",
   "101010000011000010111100100110000101101001111100011110" when "101110",
   "101010001111001100011111111110000011111001110100100001" when "101111",
   "101010011011010010100100100110000010001011010100100100" when "110000",
   "101010100111010101001101100110000000011110001100100111" when "110001",
   "101010110011010100011101101101111110110010011100101011" when "110010",
   "101010111111010000010111111101111101001000001100101101" when "110011",
   "101011001011001000111110111101111011011111010100110000" when "110100",
   "101011010110111110010101100101111001110111101100110010" when "110101",
   "101011100010110000011110010101111000010001011100110100" when "110110",
   "101011101110011111011011110101110110101100011100110111" when "110111",
   "101011111010001011010000100101110101001000110100111010" when "111000",
   "101100000101110011111111010101110011100110010100111100" when "111001",
   "101100010001011001101010001101110010000101000100111110" when "111010",
   "101100011100111100010011101101110000100100111101000001" when "111011",
   "101100101000011011111110010101101111000110000101000011" when "111100",
   "101100110011111000101100000101101101101000001101000110" when "111101",
   "101100111111010010011111011101101100001011100101001000" when "111110",
   "101101001010101001011010100101101010101111111101001011" when "111111",
   "------------------------------------------------------" when others;
    Y <= TableOut;
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_25_f400_uid128
--                     (IntAdderClassical_25_F400_uid130)
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

entity IntAdder_25_f400_uid128 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(24 downto 0);
          Y : in  std_logic_vector(24 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(24 downto 0)   );
end entity;

architecture arch of IntAdder_25_f400_uid128 is
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
--                  FixMultAdd_10x10p19r20signed_F400_uid10
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Matei Istoan, 2012-2014
--------------------------------------------------------------------------------
-- Pipeline depth: 2 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixMultAdd_10x10p19r20signed_F400_uid10 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(9 downto 0);
          Y : in  std_logic_vector(9 downto 0);
          A : in  std_logic_vector(18 downto 0);
          R : out  std_logic_vector(19 downto 0)   );
end entity;

architecture arch of FixMultAdd_10x10p19r20signed_F400_uid10 is
   component IntAdder_25_f400_uid128 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(24 downto 0);
             Y : in  std_logic_vector(24 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(24 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYu_F400_uid17 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYu_F400_uid19 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XuYs_F400_uid21 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(5 downto 0)   );
   end component;

   component SmallMultTableP3x3r6XsYs_F400_uid23 is
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

signal XX_m13 :  std_logic_vector(9 downto 0);
signal YY_m13 :  std_logic_vector(9 downto 0);
signal Xp_m13b15 :  std_logic_vector(11 downto 0);
signal Yp_m13b15 :  std_logic_vector(11 downto 0);
signal x_m13b15_0 :  std_logic_vector(2 downto 0);
signal x_m13b15_1 :  std_logic_vector(2 downto 0);
signal x_m13b15_2 :  std_logic_vector(2 downto 0);
signal x_m13b15_3 :  std_logic_vector(2 downto 0);
signal y_m13b15_0 :  std_logic_vector(2 downto 0);
signal y_m13b15_1 :  std_logic_vector(2 downto 0);
signal y_m13b15_2 :  std_logic_vector(2 downto 0);
signal y_m13b15_3 :  std_logic_vector(2 downto 0);
signal Y0X1_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X1Y0_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w0_0, heap_bh11_w0_0_d1, heap_bh11_w0_0_d2 :  std_logic;
signal Y0X2_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X2Y0_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w0_1 :  std_logic;
signal heap_bh11_w1_0 :  std_logic;
signal heap_bh11_w2_0 :  std_logic;
signal heap_bh11_w3_0 :  std_logic;
signal Y0X3_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X3Y0_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w3_1 :  std_logic;
signal heap_bh11_w4_0 :  std_logic;
signal heap_bh11_w5_0 :  std_logic;
signal heap_bh11_w6_0 :  std_logic;
signal Y1X0_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X0Y1_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w0_2 :  std_logic;
signal Y1X1_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X1Y1_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w0_3 :  std_logic;
signal heap_bh11_w1_1 :  std_logic;
signal heap_bh11_w2_1 :  std_logic;
signal heap_bh11_w3_2 :  std_logic;
signal Y1X2_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X2Y1_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w1_2 :  std_logic;
signal heap_bh11_w2_2 :  std_logic;
signal heap_bh11_w3_3 :  std_logic;
signal heap_bh11_w4_1 :  std_logic;
signal heap_bh11_w5_1 :  std_logic;
signal heap_bh11_w6_1 :  std_logic;
signal Y1X3_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X3Y1_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w4_2 :  std_logic;
signal heap_bh11_w5_2 :  std_logic;
signal heap_bh11_w6_2 :  std_logic;
signal heap_bh11_w7_0 :  std_logic;
signal heap_bh11_w8_0 :  std_logic;
signal heap_bh11_w9_0 :  std_logic;
signal Y2X0_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X0Y2_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w0_4 :  std_logic;
signal heap_bh11_w1_3 :  std_logic;
signal heap_bh11_w2_3 :  std_logic;
signal heap_bh11_w3_4 :  std_logic;
signal Y2X1_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X1Y2_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w1_4 :  std_logic;
signal heap_bh11_w2_4 :  std_logic;
signal heap_bh11_w3_5 :  std_logic;
signal heap_bh11_w4_3 :  std_logic;
signal heap_bh11_w5_3 :  std_logic;
signal heap_bh11_w6_3 :  std_logic;
signal Y2X2_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X2Y2_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w4_4 :  std_logic;
signal heap_bh11_w5_4 :  std_logic;
signal heap_bh11_w6_4 :  std_logic;
signal heap_bh11_w7_1 :  std_logic;
signal heap_bh11_w8_1 :  std_logic;
signal heap_bh11_w9_1 :  std_logic;
signal Y2X3_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X3Y2_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w7_2 :  std_logic;
signal heap_bh11_w8_2 :  std_logic;
signal heap_bh11_w9_2 :  std_logic;
signal heap_bh11_w10_0 :  std_logic;
signal heap_bh11_w11_0 :  std_logic;
signal heap_bh11_w12_0 :  std_logic;
signal Y3X0_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X0Y3_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w3_6 :  std_logic;
signal heap_bh11_w4_5 :  std_logic;
signal heap_bh11_w5_5 :  std_logic;
signal heap_bh11_w6_5 :  std_logic;
signal Y3X1_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X1Y3_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w4_6 :  std_logic;
signal heap_bh11_w5_6 :  std_logic;
signal heap_bh11_w6_6 :  std_logic;
signal heap_bh11_w7_3 :  std_logic;
signal heap_bh11_w8_3 :  std_logic;
signal heap_bh11_w9_3 :  std_logic;
signal Y3X2_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X2Y3_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w7_4 :  std_logic;
signal heap_bh11_w8_4 :  std_logic;
signal heap_bh11_w9_4 :  std_logic;
signal heap_bh11_w10_1 :  std_logic;
signal heap_bh11_w11_1 :  std_logic;
signal heap_bh11_w12_1 :  std_logic;
signal Y3X3_15_m13 :  std_logic_vector(5 downto 0);
signal PP15X3Y3_m13 :  std_logic_vector(5 downto 0);
signal heap_bh11_w10_2 :  std_logic;
signal heap_bh11_w11_2 :  std_logic;
signal heap_bh11_w12_2 :  std_logic;
signal heap_bh11_w13_0 :  std_logic;
signal heap_bh11_w14_0 :  std_logic;
signal heap_bh11_w15_0 :  std_logic;
signal heap_bh11_w5_7 :  std_logic;
signal heap_bh11_w6_7 :  std_logic;
signal heap_bh11_w7_5 :  std_logic;
signal heap_bh11_w8_5 :  std_logic;
signal heap_bh11_w9_5 :  std_logic;
signal heap_bh11_w10_3 :  std_logic;
signal heap_bh11_w11_3 :  std_logic;
signal heap_bh11_w12_3 :  std_logic;
signal heap_bh11_w13_1 :  std_logic;
signal heap_bh11_w14_1 :  std_logic;
signal heap_bh11_w15_1 :  std_logic;
signal heap_bh11_w16_0 :  std_logic;
signal heap_bh11_w17_0 :  std_logic;
signal heap_bh11_w18_0, heap_bh11_w18_0_d1 :  std_logic;
signal heap_bh11_w19_0, heap_bh11_w19_0_d1 :  std_logic;
signal heap_bh11_w20_0, heap_bh11_w20_0_d1 :  std_logic;
signal heap_bh11_w21_0, heap_bh11_w21_0_d1 :  std_logic;
signal heap_bh11_w22_0, heap_bh11_w22_0_d1 :  std_logic;
signal heap_bh11_w23_0, heap_bh11_w23_0_d1 :  std_logic;
signal heap_bh11_w3_7 :  std_logic;
signal heap_bh11_w7_6 :  std_logic;
signal heap_bh11_w8_6 :  std_logic;
signal heap_bh11_w9_6 :  std_logic;
signal heap_bh11_w11_4 :  std_logic;
signal heap_bh11_w12_4 :  std_logic;
signal heap_bh11_w14_2 :  std_logic;
signal heap_bh11_w16_1 :  std_logic;
signal heap_bh11_w17_1 :  std_logic;
signal heap_bh11_w18_1, heap_bh11_w18_1_d1 :  std_logic;
signal heap_bh11_w19_1, heap_bh11_w19_1_d1 :  std_logic;
signal heap_bh11_w20_1, heap_bh11_w20_1_d1 :  std_logic;
signal heap_bh11_w21_1, heap_bh11_w21_1_d1 :  std_logic;
signal heap_bh11_w22_1, heap_bh11_w22_1_d1 :  std_logic;
signal heap_bh11_w23_1, heap_bh11_w23_1_d1 :  std_logic;
signal CompressorIn_bh11_0_0 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh11_w3_8 :  std_logic;
signal heap_bh11_w4_7 :  std_logic;
signal heap_bh11_w5_8 :  std_logic;
signal CompressorIn_bh11_1_1 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh11_w4_8 :  std_logic;
signal heap_bh11_w5_9 :  std_logic;
signal heap_bh11_w6_8 :  std_logic;
signal CompressorIn_bh11_2_2 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh11_w5_10 :  std_logic;
signal heap_bh11_w6_9 :  std_logic;
signal heap_bh11_w7_7 :  std_logic;
signal CompressorIn_bh11_3_3 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh11_w6_10 :  std_logic;
signal heap_bh11_w7_8 :  std_logic;
signal heap_bh11_w8_7 :  std_logic;
signal CompressorIn_bh11_4_4 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_4_4 :  std_logic_vector(2 downto 0);
signal heap_bh11_w7_9 :  std_logic;
signal heap_bh11_w8_8 :  std_logic;
signal heap_bh11_w9_7 :  std_logic;
signal CompressorIn_bh11_5_5 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_5_5 :  std_logic_vector(2 downto 0);
signal heap_bh11_w8_9 :  std_logic;
signal heap_bh11_w9_8 :  std_logic;
signal heap_bh11_w10_4, heap_bh11_w10_4_d1 :  std_logic;
signal CompressorIn_bh11_6_6 :  std_logic_vector(5 downto 0);
signal CompressorOut_bh11_6_6 :  std_logic_vector(2 downto 0);
signal heap_bh11_w9_9 :  std_logic;
signal heap_bh11_w10_5 :  std_logic;
signal heap_bh11_w11_5 :  std_logic;
signal CompressorIn_bh11_7_7 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_7_8 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_7_7 :  std_logic_vector(2 downto 0);
signal heap_bh11_w0_5, heap_bh11_w0_5_d1, heap_bh11_w0_5_d2 :  std_logic;
signal heap_bh11_w1_5, heap_bh11_w1_5_d1, heap_bh11_w1_5_d2 :  std_logic;
signal heap_bh11_w2_5 :  std_logic;
signal CompressorIn_bh11_8_9 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_8_10 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_8_8 :  std_logic_vector(2 downto 0);
signal heap_bh11_w1_6, heap_bh11_w1_6_d1, heap_bh11_w1_6_d2 :  std_logic;
signal heap_bh11_w2_6 :  std_logic;
signal heap_bh11_w3_9 :  std_logic;
signal CompressorIn_bh11_9_11 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_9_12 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_9_9 :  std_logic_vector(2 downto 0);
signal heap_bh11_w2_7 :  std_logic;
signal heap_bh11_w3_10 :  std_logic;
signal heap_bh11_w4_9 :  std_logic;
signal CompressorIn_bh11_10_13 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_10_14 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_10_10 :  std_logic_vector(2 downto 0);
signal heap_bh11_w10_6 :  std_logic;
signal heap_bh11_w11_6 :  std_logic;
signal heap_bh11_w12_5, heap_bh11_w12_5_d1 :  std_logic;
signal CompressorIn_bh11_11_15 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_11_16 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_11_11 :  std_logic_vector(2 downto 0);
signal heap_bh11_w11_7 :  std_logic;
signal heap_bh11_w12_6 :  std_logic;
signal heap_bh11_w13_2 :  std_logic;
signal CompressorIn_bh11_12_17 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_12_18 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_12_12 :  std_logic_vector(2 downto 0);
signal heap_bh11_w12_7 :  std_logic;
signal heap_bh11_w13_3 :  std_logic;
signal heap_bh11_w14_3 :  std_logic;
signal CompressorIn_bh11_13_19 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_13_20 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_13_13 :  std_logic_vector(2 downto 0);
signal heap_bh11_w14_4 :  std_logic;
signal heap_bh11_w15_2, heap_bh11_w15_2_d1, heap_bh11_w15_2_d2 :  std_logic;
signal heap_bh11_w16_2 :  std_logic;
signal CompressorIn_bh11_14_21 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_14_22 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_14_14 :  std_logic_vector(2 downto 0);
signal heap_bh11_w3_11, heap_bh11_w3_11_d1, heap_bh11_w3_11_d2 :  std_logic;
signal heap_bh11_w4_10, heap_bh11_w4_10_d1, heap_bh11_w4_10_d2 :  std_logic;
signal heap_bh11_w5_11, heap_bh11_w5_11_d1 :  std_logic;
signal CompressorIn_bh11_15_23 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_15_24 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_15_15 :  std_logic_vector(2 downto 0);
signal heap_bh11_w5_12, heap_bh11_w5_12_d1 :  std_logic;
signal heap_bh11_w6_11, heap_bh11_w6_11_d1, heap_bh11_w6_11_d2 :  std_logic;
signal heap_bh11_w7_10, heap_bh11_w7_10_d1 :  std_logic;
signal CompressorIn_bh11_16_25 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_16_26 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_16_16 :  std_logic_vector(2 downto 0);
signal heap_bh11_w6_12, heap_bh11_w6_12_d1 :  std_logic;
signal heap_bh11_w7_11, heap_bh11_w7_11_d1 :  std_logic;
signal heap_bh11_w8_10, heap_bh11_w8_10_d1, heap_bh11_w8_10_d2 :  std_logic;
signal CompressorIn_bh11_17_27 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh11_17_28 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_17_17 :  std_logic_vector(2 downto 0);
signal heap_bh11_w8_11, heap_bh11_w8_11_d1 :  std_logic;
signal heap_bh11_w9_10, heap_bh11_w9_10_d1 :  std_logic;
signal heap_bh11_w10_7, heap_bh11_w10_7_d1 :  std_logic;
signal CompressorIn_bh11_18_29 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_18_30 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_18_18 :  std_logic_vector(2 downto 0);
signal heap_bh11_w9_11, heap_bh11_w9_11_d1 :  std_logic;
signal heap_bh11_w10_8, heap_bh11_w10_8_d1 :  std_logic;
signal heap_bh11_w11_8, heap_bh11_w11_8_d1 :  std_logic;
signal CompressorIn_bh11_19_31 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_19_32 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_19_19 :  std_logic_vector(2 downto 0);
signal heap_bh11_w11_9, heap_bh11_w11_9_d1 :  std_logic;
signal heap_bh11_w12_8, heap_bh11_w12_8_d1 :  std_logic;
signal heap_bh11_w13_4, heap_bh11_w13_4_d1 :  std_logic;
signal CompressorIn_bh11_20_33 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_20_34 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_20_20 :  std_logic_vector(2 downto 0);
signal heap_bh11_w13_5, heap_bh11_w13_5_d1 :  std_logic;
signal heap_bh11_w14_5, heap_bh11_w14_5_d1, heap_bh11_w14_5_d2 :  std_logic;
signal heap_bh11_w15_3, heap_bh11_w15_3_d1, heap_bh11_w15_3_d2 :  std_logic;
signal CompressorIn_bh11_21_35 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_21_36 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_21_21 :  std_logic_vector(2 downto 0);
signal heap_bh11_w16_3, heap_bh11_w16_3_d1, heap_bh11_w16_3_d2 :  std_logic;
signal heap_bh11_w17_2, heap_bh11_w17_2_d1, heap_bh11_w17_2_d2 :  std_logic;
signal heap_bh11_w18_2, heap_bh11_w18_2_d1 :  std_logic;
signal CompressorIn_bh11_22_37 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_22_38 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_22_22 :  std_logic_vector(2 downto 0);
signal heap_bh11_w4_11, heap_bh11_w4_11_d1, heap_bh11_w4_11_d2 :  std_logic;
signal heap_bh11_w5_13, heap_bh11_w5_13_d1 :  std_logic;
signal heap_bh11_w6_13, heap_bh11_w6_13_d1 :  std_logic;
signal CompressorIn_bh11_23_39 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh11_23_23 :  std_logic_vector(1 downto 0);
signal heap_bh11_w2_8, heap_bh11_w2_8_d1, heap_bh11_w2_8_d2 :  std_logic;
signal heap_bh11_w3_12, heap_bh11_w3_12_d1, heap_bh11_w3_12_d2 :  std_logic;
signal CompressorIn_bh11_24_40 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh11_24_24 :  std_logic_vector(1 downto 0);
signal heap_bh11_w7_12, heap_bh11_w7_12_d1 :  std_logic;
signal heap_bh11_w8_12, heap_bh11_w8_12_d1 :  std_logic;
signal CompressorIn_bh11_25_41 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_25_42 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_25_25 :  std_logic_vector(2 downto 0);
signal heap_bh11_w5_14, heap_bh11_w5_14_d1 :  std_logic;
signal heap_bh11_w6_14, heap_bh11_w6_14_d1 :  std_logic;
signal heap_bh11_w7_13, heap_bh11_w7_13_d1 :  std_logic;
signal CompressorIn_bh11_26_43 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_26_44 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_26_26 :  std_logic_vector(2 downto 0);
signal heap_bh11_w7_14, heap_bh11_w7_14_d1 :  std_logic;
signal heap_bh11_w8_13, heap_bh11_w8_13_d1 :  std_logic;
signal heap_bh11_w9_12 :  std_logic;
signal CompressorIn_bh11_27_45 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_27_46 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_27_27 :  std_logic_vector(2 downto 0);
signal heap_bh11_w10_9 :  std_logic;
signal heap_bh11_w11_10, heap_bh11_w11_10_d1 :  std_logic;
signal heap_bh11_w12_9 :  std_logic;
signal CompressorIn_bh11_28_47 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_28_48 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_28_28 :  std_logic_vector(2 downto 0);
signal heap_bh11_w18_3, heap_bh11_w18_3_d1 :  std_logic;
signal heap_bh11_w19_2, heap_bh11_w19_2_d1 :  std_logic;
signal heap_bh11_w20_2 :  std_logic;
signal CompressorIn_bh11_29_49 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_29_50 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_29_29 :  std_logic_vector(2 downto 0);
signal heap_bh11_w12_10, heap_bh11_w12_10_d1 :  std_logic;
signal heap_bh11_w13_6, heap_bh11_w13_6_d1 :  std_logic;
signal heap_bh11_w14_6, heap_bh11_w14_6_d1 :  std_logic;
signal CompressorIn_bh11_30_51 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_30_52 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_30_30 :  std_logic_vector(2 downto 0);
signal heap_bh11_w20_3, heap_bh11_w20_3_d1 :  std_logic;
signal heap_bh11_w21_2, heap_bh11_w21_2_d1 :  std_logic;
signal heap_bh11_w22_2 :  std_logic;
signal CompressorIn_bh11_31_53 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_31_54 :  std_logic_vector(0 downto 0);
signal CompressorOut_bh11_31_31 :  std_logic_vector(2 downto 0);
signal heap_bh11_w9_13, heap_bh11_w9_13_d1 :  std_logic;
signal heap_bh11_w10_10, heap_bh11_w10_10_d1 :  std_logic;
signal heap_bh11_w11_11, heap_bh11_w11_11_d1 :  std_logic;
signal CompressorIn_bh11_32_55 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh11_32_56 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh11_32_32 :  std_logic_vector(2 downto 0);
signal heap_bh11_w22_3, heap_bh11_w22_3_d1 :  std_logic;
signal heap_bh11_w23_2, heap_bh11_w23_2_d1 :  std_logic;
signal finalAdderIn0_bh11 :  std_logic_vector(24 downto 0);
signal finalAdderIn1_bh11 :  std_logic_vector(24 downto 0);
signal finalAdderCin_bh11 :  std_logic;
signal finalAdderOut_bh11 :  std_logic_vector(24 downto 0);
signal CompressionResult11 :  std_logic_vector(24 downto 0);
attribute rom_extract: string;
attribute rom_style: string;
attribute rom_extract of SmallMultTableP3x3r6XsYs_F400_uid23: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XsYu_F400_uid19: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYs_F400_uid21: component is "yes";
attribute rom_extract of SmallMultTableP3x3r6XuYu_F400_uid17: component is "yes";
attribute rom_style of SmallMultTableP3x3r6XsYs_F400_uid23: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XsYu_F400_uid19: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYs_F400_uid21: component is "distributed";
attribute rom_style of SmallMultTableP3x3r6XuYu_F400_uid17: component is "distributed";
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            heap_bh11_w0_0_d1 <=  heap_bh11_w0_0;
            heap_bh11_w0_0_d2 <=  heap_bh11_w0_0_d1;
            heap_bh11_w18_0_d1 <=  heap_bh11_w18_0;
            heap_bh11_w19_0_d1 <=  heap_bh11_w19_0;
            heap_bh11_w20_0_d1 <=  heap_bh11_w20_0;
            heap_bh11_w21_0_d1 <=  heap_bh11_w21_0;
            heap_bh11_w22_0_d1 <=  heap_bh11_w22_0;
            heap_bh11_w23_0_d1 <=  heap_bh11_w23_0;
            heap_bh11_w18_1_d1 <=  heap_bh11_w18_1;
            heap_bh11_w19_1_d1 <=  heap_bh11_w19_1;
            heap_bh11_w20_1_d1 <=  heap_bh11_w20_1;
            heap_bh11_w21_1_d1 <=  heap_bh11_w21_1;
            heap_bh11_w22_1_d1 <=  heap_bh11_w22_1;
            heap_bh11_w23_1_d1 <=  heap_bh11_w23_1;
            heap_bh11_w10_4_d1 <=  heap_bh11_w10_4;
            heap_bh11_w0_5_d1 <=  heap_bh11_w0_5;
            heap_bh11_w0_5_d2 <=  heap_bh11_w0_5_d1;
            heap_bh11_w1_5_d1 <=  heap_bh11_w1_5;
            heap_bh11_w1_5_d2 <=  heap_bh11_w1_5_d1;
            heap_bh11_w1_6_d1 <=  heap_bh11_w1_6;
            heap_bh11_w1_6_d2 <=  heap_bh11_w1_6_d1;
            heap_bh11_w12_5_d1 <=  heap_bh11_w12_5;
            heap_bh11_w15_2_d1 <=  heap_bh11_w15_2;
            heap_bh11_w15_2_d2 <=  heap_bh11_w15_2_d1;
            heap_bh11_w3_11_d1 <=  heap_bh11_w3_11;
            heap_bh11_w3_11_d2 <=  heap_bh11_w3_11_d1;
            heap_bh11_w4_10_d1 <=  heap_bh11_w4_10;
            heap_bh11_w4_10_d2 <=  heap_bh11_w4_10_d1;
            heap_bh11_w5_11_d1 <=  heap_bh11_w5_11;
            heap_bh11_w5_12_d1 <=  heap_bh11_w5_12;
            heap_bh11_w6_11_d1 <=  heap_bh11_w6_11;
            heap_bh11_w6_11_d2 <=  heap_bh11_w6_11_d1;
            heap_bh11_w7_10_d1 <=  heap_bh11_w7_10;
            heap_bh11_w6_12_d1 <=  heap_bh11_w6_12;
            heap_bh11_w7_11_d1 <=  heap_bh11_w7_11;
            heap_bh11_w8_10_d1 <=  heap_bh11_w8_10;
            heap_bh11_w8_10_d2 <=  heap_bh11_w8_10_d1;
            heap_bh11_w8_11_d1 <=  heap_bh11_w8_11;
            heap_bh11_w9_10_d1 <=  heap_bh11_w9_10;
            heap_bh11_w10_7_d1 <=  heap_bh11_w10_7;
            heap_bh11_w9_11_d1 <=  heap_bh11_w9_11;
            heap_bh11_w10_8_d1 <=  heap_bh11_w10_8;
            heap_bh11_w11_8_d1 <=  heap_bh11_w11_8;
            heap_bh11_w11_9_d1 <=  heap_bh11_w11_9;
            heap_bh11_w12_8_d1 <=  heap_bh11_w12_8;
            heap_bh11_w13_4_d1 <=  heap_bh11_w13_4;
            heap_bh11_w13_5_d1 <=  heap_bh11_w13_5;
            heap_bh11_w14_5_d1 <=  heap_bh11_w14_5;
            heap_bh11_w14_5_d2 <=  heap_bh11_w14_5_d1;
            heap_bh11_w15_3_d1 <=  heap_bh11_w15_3;
            heap_bh11_w15_3_d2 <=  heap_bh11_w15_3_d1;
            heap_bh11_w16_3_d1 <=  heap_bh11_w16_3;
            heap_bh11_w16_3_d2 <=  heap_bh11_w16_3_d1;
            heap_bh11_w17_2_d1 <=  heap_bh11_w17_2;
            heap_bh11_w17_2_d2 <=  heap_bh11_w17_2_d1;
            heap_bh11_w18_2_d1 <=  heap_bh11_w18_2;
            heap_bh11_w4_11_d1 <=  heap_bh11_w4_11;
            heap_bh11_w4_11_d2 <=  heap_bh11_w4_11_d1;
            heap_bh11_w5_13_d1 <=  heap_bh11_w5_13;
            heap_bh11_w6_13_d1 <=  heap_bh11_w6_13;
            heap_bh11_w2_8_d1 <=  heap_bh11_w2_8;
            heap_bh11_w2_8_d2 <=  heap_bh11_w2_8_d1;
            heap_bh11_w3_12_d1 <=  heap_bh11_w3_12;
            heap_bh11_w3_12_d2 <=  heap_bh11_w3_12_d1;
            heap_bh11_w7_12_d1 <=  heap_bh11_w7_12;
            heap_bh11_w8_12_d1 <=  heap_bh11_w8_12;
            heap_bh11_w5_14_d1 <=  heap_bh11_w5_14;
            heap_bh11_w6_14_d1 <=  heap_bh11_w6_14;
            heap_bh11_w7_13_d1 <=  heap_bh11_w7_13;
            heap_bh11_w7_14_d1 <=  heap_bh11_w7_14;
            heap_bh11_w8_13_d1 <=  heap_bh11_w8_13;
            heap_bh11_w11_10_d1 <=  heap_bh11_w11_10;
            heap_bh11_w18_3_d1 <=  heap_bh11_w18_3;
            heap_bh11_w19_2_d1 <=  heap_bh11_w19_2;
            heap_bh11_w12_10_d1 <=  heap_bh11_w12_10;
            heap_bh11_w13_6_d1 <=  heap_bh11_w13_6;
            heap_bh11_w14_6_d1 <=  heap_bh11_w14_6;
            heap_bh11_w20_3_d1 <=  heap_bh11_w20_3;
            heap_bh11_w21_2_d1 <=  heap_bh11_w21_2;
            heap_bh11_w9_13_d1 <=  heap_bh11_w9_13;
            heap_bh11_w10_10_d1 <=  heap_bh11_w10_10;
            heap_bh11_w11_11_d1 <=  heap_bh11_w11_11;
            heap_bh11_w22_3_d1 <=  heap_bh11_w22_3;
            heap_bh11_w23_2_d1 <=  heap_bh11_w23_2;
         end if;
      end process;
   XX_m13 <= X ;
   YY_m13 <= Y ;
   -- code generated by IntMultiplier::buildHeapLogicOnly()
   -- buildheaplogiconly called for lsbX=0 lsbY=0 msbX=10 msbY=10
   Xp_m13b15 <= XX_m13(9 downto 0) & "00";
   Yp_m13b15 <= YY_m13(9 downto 0) & "00";
   x_m13b15_0 <= Xp_m13b15(2 downto 0);
   x_m13b15_1 <= Xp_m13b15(5 downto 3);
   x_m13b15_2 <= Xp_m13b15(8 downto 6);
   x_m13b15_3 <= Xp_m13b15(11 downto 9);
   y_m13b15_0 <= Yp_m13b15(2 downto 0);
   y_m13b15_1 <= Yp_m13b15(5 downto 3);
   y_m13b15_2 <= Yp_m13b15(8 downto 6);
   y_m13b15_3 <= Yp_m13b15(11 downto 9);
   ----------------Synchro barrier, entering cycle 0----------------
   -- Partial product row number 0
   Y0X1_15_m13 <= y_m13b15_0 & x_m13b15_1;
   PP_m13_15X1Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X1_15_m13,
                 Y => PP15X1Y0_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w0_0 <= PP15X1Y0_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y0X2_15_m13 <= y_m13b15_0 & x_m13b15_2;
   PP_m13_15X2Y0_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X2_15_m13,
                 Y => PP15X2Y0_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w0_1 <= PP15X2Y0_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w1_0 <= PP15X2Y0_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w2_0 <= PP15X2Y0_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w3_0 <= PP15X2Y0_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y0X3_15_m13 <= y_m13b15_0 & x_m13b15_3;
   PP_m13_15X3Y0_Tbl: SmallMultTableP3x3r6XsYu_F400_uid19  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y0X3_15_m13,
                 Y => PP15X3Y0_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w3_1 <= PP15X3Y0_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w4_0 <= PP15X3Y0_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_0 <= PP15X3Y0_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_0 <= not PP15X3Y0_m13(5); -- cycle= 0 cp= 5.5688e-10

   -- Partial product row number 1
   Y1X0_15_m13 <= y_m13b15_1 & x_m13b15_0;
   PP_m13_15X0Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X0_15_m13,
                 Y => PP15X0Y1_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w0_2 <= PP15X0Y1_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y1X1_15_m13 <= y_m13b15_1 & x_m13b15_1;
   PP_m13_15X1Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X1_15_m13,
                 Y => PP15X1Y1_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w0_3 <= PP15X1Y1_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w1_1 <= PP15X1Y1_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w2_1 <= PP15X1Y1_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w3_2 <= PP15X1Y1_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y1X2_15_m13 <= y_m13b15_1 & x_m13b15_2;
   PP_m13_15X2Y1_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X2_15_m13,
                 Y => PP15X2Y1_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w1_2 <= PP15X2Y1_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w2_2 <= PP15X2Y1_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w3_3 <= PP15X2Y1_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w4_1 <= PP15X2Y1_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_1 <= PP15X2Y1_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_1 <= PP15X2Y1_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y1X3_15_m13 <= y_m13b15_1 & x_m13b15_3;
   PP_m13_15X3Y1_Tbl: SmallMultTableP3x3r6XsYu_F400_uid19  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y1X3_15_m13,
                 Y => PP15X3Y1_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w4_2 <= PP15X3Y1_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_2 <= PP15X3Y1_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_2 <= PP15X3Y1_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w7_0 <= PP15X3Y1_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_0 <= PP15X3Y1_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_0 <= not PP15X3Y1_m13(5); -- cycle= 0 cp= 5.5688e-10

   -- Partial product row number 2
   Y2X0_15_m13 <= y_m13b15_2 & x_m13b15_0;
   PP_m13_15X0Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X0_15_m13,
                 Y => PP15X0Y2_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w0_4 <= PP15X0Y2_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w1_3 <= PP15X0Y2_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w2_3 <= PP15X0Y2_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w3_4 <= PP15X0Y2_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y2X1_15_m13 <= y_m13b15_2 & x_m13b15_1;
   PP_m13_15X1Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X1_15_m13,
                 Y => PP15X1Y2_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w1_4 <= PP15X1Y2_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w2_4 <= PP15X1Y2_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w3_5 <= PP15X1Y2_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w4_3 <= PP15X1Y2_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_3 <= PP15X1Y2_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_3 <= PP15X1Y2_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y2X2_15_m13 <= y_m13b15_2 & x_m13b15_2;
   PP_m13_15X2Y2_Tbl: SmallMultTableP3x3r6XuYu_F400_uid17  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X2_15_m13,
                 Y => PP15X2Y2_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w4_4 <= PP15X2Y2_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_4 <= PP15X2Y2_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_4 <= PP15X2Y2_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w7_1 <= PP15X2Y2_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_1 <= PP15X2Y2_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_1 <= PP15X2Y2_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y2X3_15_m13 <= y_m13b15_2 & x_m13b15_3;
   PP_m13_15X3Y2_Tbl: SmallMultTableP3x3r6XsYu_F400_uid19  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y2X3_15_m13,
                 Y => PP15X3Y2_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w7_2 <= PP15X3Y2_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_2 <= PP15X3Y2_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_2 <= PP15X3Y2_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w10_0 <= PP15X3Y2_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w11_0 <= PP15X3Y2_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w12_0 <= not PP15X3Y2_m13(5); -- cycle= 0 cp= 5.5688e-10

   -- Partial product row number 3
   Y3X0_15_m13 <= y_m13b15_3 & x_m13b15_0;
   PP_m13_15X0Y3_Tbl: SmallMultTableP3x3r6XuYs_F400_uid21  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X0_15_m13,
                 Y => PP15X0Y3_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w3_6 <= PP15X0Y3_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w4_5 <= PP15X0Y3_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_5 <= PP15X0Y3_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_5 <= not PP15X0Y3_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y3X1_15_m13 <= y_m13b15_3 & x_m13b15_1;
   PP_m13_15X1Y3_Tbl: SmallMultTableP3x3r6XuYs_F400_uid21  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X1_15_m13,
                 Y => PP15X1Y3_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w4_6 <= PP15X1Y3_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w5_6 <= PP15X1Y3_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_6 <= PP15X1Y3_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w7_3 <= PP15X1Y3_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_3 <= PP15X1Y3_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_3 <= not PP15X1Y3_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y3X2_15_m13 <= y_m13b15_3 & x_m13b15_2;
   PP_m13_15X2Y3_Tbl: SmallMultTableP3x3r6XuYs_F400_uid21  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X2_15_m13,
                 Y => PP15X2Y3_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w7_4 <= PP15X2Y3_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_4 <= PP15X2Y3_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_4 <= PP15X2Y3_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w10_1 <= PP15X2Y3_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w11_1 <= PP15X2Y3_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w12_1 <= not PP15X2Y3_m13(5); -- cycle= 0 cp= 5.5688e-10

   Y3X3_15_m13 <= y_m13b15_3 & x_m13b15_3;
   PP_m13_15X3Y3_Tbl: SmallMultTableP3x3r6XsYs_F400_uid23  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => Y3X3_15_m13,
                 Y => PP15X3Y3_m13);
   -- Adding the relevant bits to the heap of bits
   heap_bh11_w10_2 <= PP15X3Y3_m13(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w11_2 <= PP15X3Y3_m13(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w12_2 <= PP15X3Y3_m13(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w13_0 <= PP15X3Y3_m13(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w14_0 <= PP15X3Y3_m13(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w15_0 <= not PP15X3Y3_m13(5); -- cycle= 0 cp= 5.5688e-10

   heap_bh11_w5_7 <= A(0); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w6_7 <= A(1); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w7_5 <= A(2); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w8_5 <= A(3); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w9_5 <= A(4); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w10_3 <= A(5); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w11_3 <= A(6); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w12_3 <= A(7); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w13_1 <= A(8); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w14_1 <= A(9); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w15_1 <= A(10); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w16_0 <= A(11); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w17_0 <= A(12); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w18_0 <= A(13); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w19_0 <= A(14); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w20_0 <= A(15); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w21_0 <= A(16); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w22_0 <= A(17); -- cycle= 0 cp= 5.5688e-10
   heap_bh11_w23_0 <= A(18); -- cycle= 0 cp= 5.5688e-10
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh11_w3_7 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w7_6 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w8_6 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w9_6 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w11_4 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w12_4 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w14_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w16_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w17_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w18_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w19_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w20_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w21_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w22_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh11_w23_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_0_0 <= heap_bh11_w3_7 & heap_bh11_w3_6 & heap_bh11_w3_5 & heap_bh11_w3_4 & heap_bh11_w3_3 & heap_bh11_w3_2;
      Compressor_bh11_0: Compressor_6_3
      port map ( R => CompressorOut_bh11_0_0,
                 X0 => CompressorIn_bh11_0_0);
   heap_bh11_w3_8 <= CompressorOut_bh11_0_0(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w4_7 <= CompressorOut_bh11_0_0(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w5_8 <= CompressorOut_bh11_0_0(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_1_1 <= heap_bh11_w4_6 & heap_bh11_w4_5 & heap_bh11_w4_4 & heap_bh11_w4_3 & heap_bh11_w4_2 & heap_bh11_w4_1;
      Compressor_bh11_1: Compressor_6_3
      port map ( R => CompressorOut_bh11_1_1,
                 X0 => CompressorIn_bh11_1_1);
   heap_bh11_w4_8 <= CompressorOut_bh11_1_1(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w5_9 <= CompressorOut_bh11_1_1(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w6_8 <= CompressorOut_bh11_1_1(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_2_2 <= heap_bh11_w5_7 & heap_bh11_w5_6 & heap_bh11_w5_5 & heap_bh11_w5_4 & heap_bh11_w5_3 & heap_bh11_w5_2;
      Compressor_bh11_2: Compressor_6_3
      port map ( R => CompressorOut_bh11_2_2,
                 X0 => CompressorIn_bh11_2_2);
   heap_bh11_w5_10 <= CompressorOut_bh11_2_2(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w6_9 <= CompressorOut_bh11_2_2(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w7_7 <= CompressorOut_bh11_2_2(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_3_3 <= heap_bh11_w6_7 & heap_bh11_w6_6 & heap_bh11_w6_5 & heap_bh11_w6_4 & heap_bh11_w6_3 & heap_bh11_w6_2;
      Compressor_bh11_3: Compressor_6_3
      port map ( R => CompressorOut_bh11_3_3,
                 X0 => CompressorIn_bh11_3_3);
   heap_bh11_w6_10 <= CompressorOut_bh11_3_3(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w7_8 <= CompressorOut_bh11_3_3(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w8_7 <= CompressorOut_bh11_3_3(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_4_4 <= heap_bh11_w7_6 & heap_bh11_w7_5 & heap_bh11_w7_4 & heap_bh11_w7_3 & heap_bh11_w7_2 & heap_bh11_w7_1;
      Compressor_bh11_4: Compressor_6_3
      port map ( R => CompressorOut_bh11_4_4,
                 X0 => CompressorIn_bh11_4_4);
   heap_bh11_w7_9 <= CompressorOut_bh11_4_4(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w8_8 <= CompressorOut_bh11_4_4(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w9_7 <= CompressorOut_bh11_4_4(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_5_5 <= heap_bh11_w8_6 & heap_bh11_w8_5 & heap_bh11_w8_4 & heap_bh11_w8_3 & heap_bh11_w8_2 & heap_bh11_w8_1;
      Compressor_bh11_5: Compressor_6_3
      port map ( R => CompressorOut_bh11_5_5,
                 X0 => CompressorIn_bh11_5_5);
   heap_bh11_w8_9 <= CompressorOut_bh11_5_5(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w9_8 <= CompressorOut_bh11_5_5(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w10_4 <= CompressorOut_bh11_5_5(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_6_6 <= heap_bh11_w9_6 & heap_bh11_w9_5 & heap_bh11_w9_4 & heap_bh11_w9_3 & heap_bh11_w9_2 & heap_bh11_w9_1;
      Compressor_bh11_6: Compressor_6_3
      port map ( R => CompressorOut_bh11_6_6,
                 X0 => CompressorIn_bh11_6_6);
   heap_bh11_w9_9 <= CompressorOut_bh11_6_6(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w10_5 <= CompressorOut_bh11_6_6(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w11_5 <= CompressorOut_bh11_6_6(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_7_7 <= heap_bh11_w0_4 & heap_bh11_w0_3 & heap_bh11_w0_2 & heap_bh11_w0_1;
   CompressorIn_bh11_7_8(0) <= heap_bh11_w1_4;
      Compressor_bh11_7: Compressor_14_3
      port map ( R => CompressorOut_bh11_7_7,
                 X0 => CompressorIn_bh11_7_7,
                 X1 => CompressorIn_bh11_7_8);
   heap_bh11_w0_5 <= CompressorOut_bh11_7_7(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w1_5 <= CompressorOut_bh11_7_7(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w2_5 <= CompressorOut_bh11_7_7(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_8_9 <= heap_bh11_w1_3 & heap_bh11_w1_2 & heap_bh11_w1_1 & heap_bh11_w1_0;
   CompressorIn_bh11_8_10(0) <= heap_bh11_w2_4;
      Compressor_bh11_8: Compressor_14_3
      port map ( R => CompressorOut_bh11_8_8,
                 X0 => CompressorIn_bh11_8_9,
                 X1 => CompressorIn_bh11_8_10);
   heap_bh11_w1_6 <= CompressorOut_bh11_8_8(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w2_6 <= CompressorOut_bh11_8_8(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w3_9 <= CompressorOut_bh11_8_8(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_9_11 <= heap_bh11_w2_3 & heap_bh11_w2_2 & heap_bh11_w2_1 & heap_bh11_w2_0;
   CompressorIn_bh11_9_12(0) <= heap_bh11_w3_1;
      Compressor_bh11_9: Compressor_14_3
      port map ( R => CompressorOut_bh11_9_9,
                 X0 => CompressorIn_bh11_9_11,
                 X1 => CompressorIn_bh11_9_12);
   heap_bh11_w2_7 <= CompressorOut_bh11_9_9(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w3_10 <= CompressorOut_bh11_9_9(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w4_9 <= CompressorOut_bh11_9_9(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_10_13 <= heap_bh11_w10_3 & heap_bh11_w10_2 & heap_bh11_w10_1 & heap_bh11_w10_0;
   CompressorIn_bh11_10_14(0) <= heap_bh11_w11_4;
      Compressor_bh11_10: Compressor_14_3
      port map ( R => CompressorOut_bh11_10_10,
                 X0 => CompressorIn_bh11_10_13,
                 X1 => CompressorIn_bh11_10_14);
   heap_bh11_w10_6 <= CompressorOut_bh11_10_10(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w11_6 <= CompressorOut_bh11_10_10(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w12_5 <= CompressorOut_bh11_10_10(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_11_15 <= heap_bh11_w11_3 & heap_bh11_w11_2 & heap_bh11_w11_1 & heap_bh11_w11_0;
   CompressorIn_bh11_11_16(0) <= heap_bh11_w12_4;
      Compressor_bh11_11: Compressor_14_3
      port map ( R => CompressorOut_bh11_11_11,
                 X0 => CompressorIn_bh11_11_15,
                 X1 => CompressorIn_bh11_11_16);
   heap_bh11_w11_7 <= CompressorOut_bh11_11_11(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w12_6 <= CompressorOut_bh11_11_11(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w13_2 <= CompressorOut_bh11_11_11(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_12_17 <= heap_bh11_w12_3 & heap_bh11_w12_2 & heap_bh11_w12_1 & heap_bh11_w12_0;
   CompressorIn_bh11_12_18(0) <= heap_bh11_w13_1;
      Compressor_bh11_12: Compressor_14_3
      port map ( R => CompressorOut_bh11_12_12,
                 X0 => CompressorIn_bh11_12_17,
                 X1 => CompressorIn_bh11_12_18);
   heap_bh11_w12_7 <= CompressorOut_bh11_12_12(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w13_3 <= CompressorOut_bh11_12_12(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w14_3 <= CompressorOut_bh11_12_12(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_13_19 <= heap_bh11_w14_2 & heap_bh11_w14_1 & heap_bh11_w14_0;
   CompressorIn_bh11_13_20 <= heap_bh11_w15_1 & heap_bh11_w15_0;
      Compressor_bh11_13: Compressor_23_3
      port map ( R => CompressorOut_bh11_13_13,
                 X0 => CompressorIn_bh11_13_19,
                 X1 => CompressorIn_bh11_13_20);
   heap_bh11_w14_4 <= CompressorOut_bh11_13_13(0); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w15_2 <= CompressorOut_bh11_13_13(1); -- cycle= 0 cp= 1.0876e-09
   heap_bh11_w16_2 <= CompressorOut_bh11_13_13(2); -- cycle= 0 cp= 1.0876e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_14_21 <= heap_bh11_w3_0 & heap_bh11_w3_10 & heap_bh11_w3_9 & heap_bh11_w3_8;
   CompressorIn_bh11_14_22(0) <= heap_bh11_w4_0;
      Compressor_bh11_14: Compressor_14_3
      port map ( R => CompressorOut_bh11_14_14,
                 X0 => CompressorIn_bh11_14_21,
                 X1 => CompressorIn_bh11_14_22);
   heap_bh11_w3_11 <= CompressorOut_bh11_14_14(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w4_10 <= CompressorOut_bh11_14_14(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w5_11 <= CompressorOut_bh11_14_14(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_15_23 <= heap_bh11_w5_1 & heap_bh11_w5_0 & heap_bh11_w5_10 & heap_bh11_w5_9;
   CompressorIn_bh11_15_24(0) <= heap_bh11_w6_1;
      Compressor_bh11_15: Compressor_14_3
      port map ( R => CompressorOut_bh11_15_15,
                 X0 => CompressorIn_bh11_15_23,
                 X1 => CompressorIn_bh11_15_24);
   heap_bh11_w5_12 <= CompressorOut_bh11_15_15(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w6_11 <= CompressorOut_bh11_15_15(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w7_10 <= CompressorOut_bh11_15_15(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_16_25 <= heap_bh11_w6_0 & heap_bh11_w6_10 & heap_bh11_w6_9 & heap_bh11_w6_8;
   CompressorIn_bh11_16_26(0) <= heap_bh11_w7_0;
      Compressor_bh11_16: Compressor_14_3
      port map ( R => CompressorOut_bh11_16_16,
                 X0 => CompressorIn_bh11_16_25,
                 X1 => CompressorIn_bh11_16_26);
   heap_bh11_w6_12 <= CompressorOut_bh11_16_16(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w7_11 <= CompressorOut_bh11_16_16(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w8_10 <= CompressorOut_bh11_16_16(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_17_27 <= heap_bh11_w8_0 & heap_bh11_w8_9 & heap_bh11_w8_8 & heap_bh11_w8_7;
   CompressorIn_bh11_17_28(0) <= heap_bh11_w9_0;
      Compressor_bh11_17: Compressor_14_3
      port map ( R => CompressorOut_bh11_17_17,
                 X0 => CompressorIn_bh11_17_27,
                 X1 => CompressorIn_bh11_17_28);
   heap_bh11_w8_11 <= CompressorOut_bh11_17_17(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w9_10 <= CompressorOut_bh11_17_17(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w10_7 <= CompressorOut_bh11_17_17(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_18_29 <= heap_bh11_w9_9 & heap_bh11_w9_8 & heap_bh11_w9_7;
   CompressorIn_bh11_18_30 <= heap_bh11_w10_6 & heap_bh11_w10_5;
      Compressor_bh11_18: Compressor_23_3
      port map ( R => CompressorOut_bh11_18_18,
                 X0 => CompressorIn_bh11_18_29,
                 X1 => CompressorIn_bh11_18_30);
   heap_bh11_w9_11 <= CompressorOut_bh11_18_18(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w10_8 <= CompressorOut_bh11_18_18(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w11_8 <= CompressorOut_bh11_18_18(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_19_31 <= heap_bh11_w11_7 & heap_bh11_w11_6 & heap_bh11_w11_5;
   CompressorIn_bh11_19_32 <= heap_bh11_w12_7 & heap_bh11_w12_6;
      Compressor_bh11_19: Compressor_23_3
      port map ( R => CompressorOut_bh11_19_19,
                 X0 => CompressorIn_bh11_19_31,
                 X1 => CompressorIn_bh11_19_32);
   heap_bh11_w11_9 <= CompressorOut_bh11_19_19(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w12_8 <= CompressorOut_bh11_19_19(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w13_4 <= CompressorOut_bh11_19_19(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_20_33 <= heap_bh11_w13_0 & heap_bh11_w13_3 & heap_bh11_w13_2;
   CompressorIn_bh11_20_34 <= heap_bh11_w14_4 & heap_bh11_w14_3;
      Compressor_bh11_20: Compressor_23_3
      port map ( R => CompressorOut_bh11_20_20,
                 X0 => CompressorIn_bh11_20_33,
                 X1 => CompressorIn_bh11_20_34);
   heap_bh11_w13_5 <= CompressorOut_bh11_20_20(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w14_5 <= CompressorOut_bh11_20_20(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w15_3 <= CompressorOut_bh11_20_20(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_21_35 <= heap_bh11_w16_1 & heap_bh11_w16_0 & heap_bh11_w16_2;
   CompressorIn_bh11_21_36 <= heap_bh11_w17_1 & heap_bh11_w17_0;
      Compressor_bh11_21: Compressor_23_3
      port map ( R => CompressorOut_bh11_21_21,
                 X0 => CompressorIn_bh11_21_35,
                 X1 => CompressorIn_bh11_21_36);
   heap_bh11_w16_3 <= CompressorOut_bh11_21_21(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w17_2 <= CompressorOut_bh11_21_21(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w18_2 <= CompressorOut_bh11_21_21(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_22_37 <= heap_bh11_w4_9 & heap_bh11_w4_8 & heap_bh11_w4_7;
   CompressorIn_bh11_22_38(0) <= heap_bh11_w5_8;
      Compressor_bh11_22: Compressor_13_3
      port map ( R => CompressorOut_bh11_22_22,
                 X0 => CompressorIn_bh11_22_37,
                 X1 => CompressorIn_bh11_22_38);
   heap_bh11_w4_11 <= CompressorOut_bh11_22_22(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w5_13 <= CompressorOut_bh11_22_22(1); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w6_13 <= CompressorOut_bh11_22_22(2); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_23_39 <= heap_bh11_w2_7 & heap_bh11_w2_6 & heap_bh11_w2_5;
      Compressor_bh11_23: Compressor_3_2
      port map ( R => CompressorOut_bh11_23_23,
                 X0 => CompressorIn_bh11_23_39);
   heap_bh11_w2_8 <= CompressorOut_bh11_23_23(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w3_12 <= CompressorOut_bh11_23_23(1); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   CompressorIn_bh11_24_40 <= heap_bh11_w7_9 & heap_bh11_w7_8 & heap_bh11_w7_7;
      Compressor_bh11_24: Compressor_3_2
      port map ( R => CompressorOut_bh11_24_24,
                 X0 => CompressorIn_bh11_24_40);
   heap_bh11_w7_12 <= CompressorOut_bh11_24_24(0); -- cycle= 0 cp= 1.61832e-09
   heap_bh11_w8_12 <= CompressorOut_bh11_24_24(1); -- cycle= 0 cp= 1.61832e-09

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_25_41 <= heap_bh11_w5_13_d1 & heap_bh11_w5_12_d1 & heap_bh11_w5_11_d1;
   CompressorIn_bh11_25_42 <= heap_bh11_w6_13_d1 & heap_bh11_w6_12_d1;
      Compressor_bh11_25: Compressor_23_3
      port map ( R => CompressorOut_bh11_25_25,
                 X0 => CompressorIn_bh11_25_41,
                 X1 => CompressorIn_bh11_25_42);
   heap_bh11_w5_14 <= CompressorOut_bh11_25_25(0); -- cycle= 1 cp= 0
   heap_bh11_w6_14 <= CompressorOut_bh11_25_25(1); -- cycle= 1 cp= 0
   heap_bh11_w7_13 <= CompressorOut_bh11_25_25(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_26_43 <= heap_bh11_w7_12_d1 & heap_bh11_w7_11_d1 & heap_bh11_w7_10_d1;
   CompressorIn_bh11_26_44 <= heap_bh11_w8_12_d1 & heap_bh11_w8_11_d1;
      Compressor_bh11_26: Compressor_23_3
      port map ( R => CompressorOut_bh11_26_26,
                 X0 => CompressorIn_bh11_26_43,
                 X1 => CompressorIn_bh11_26_44);
   heap_bh11_w7_14 <= CompressorOut_bh11_26_26(0); -- cycle= 1 cp= 0
   heap_bh11_w8_13 <= CompressorOut_bh11_26_26(1); -- cycle= 1 cp= 0
   heap_bh11_w9_12 <= CompressorOut_bh11_26_26(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_27_45 <= heap_bh11_w10_4_d1 & heap_bh11_w10_8_d1 & heap_bh11_w10_7_d1;
   CompressorIn_bh11_27_46 <= heap_bh11_w11_9_d1 & heap_bh11_w11_8_d1;
      Compressor_bh11_27: Compressor_23_3
      port map ( R => CompressorOut_bh11_27_27,
                 X0 => CompressorIn_bh11_27_45,
                 X1 => CompressorIn_bh11_27_46);
   heap_bh11_w10_9 <= CompressorOut_bh11_27_27(0); -- cycle= 1 cp= 0
   heap_bh11_w11_10 <= CompressorOut_bh11_27_27(1); -- cycle= 1 cp= 0
   heap_bh11_w12_9 <= CompressorOut_bh11_27_27(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_28_47 <= heap_bh11_w18_1_d1 & heap_bh11_w18_0_d1 & heap_bh11_w18_2_d1;
   CompressorIn_bh11_28_48 <= heap_bh11_w19_1_d1 & heap_bh11_w19_0_d1;
      Compressor_bh11_28: Compressor_23_3
      port map ( R => CompressorOut_bh11_28_28,
                 X0 => CompressorIn_bh11_28_47,
                 X1 => CompressorIn_bh11_28_48);
   heap_bh11_w18_3 <= CompressorOut_bh11_28_28(0); -- cycle= 1 cp= 0
   heap_bh11_w19_2 <= CompressorOut_bh11_28_28(1); -- cycle= 1 cp= 0
   heap_bh11_w20_2 <= CompressorOut_bh11_28_28(2); -- cycle= 1 cp= 0

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_29_49 <= heap_bh11_w12_5_d1 & heap_bh11_w12_8_d1 & heap_bh11_w12_9;
   CompressorIn_bh11_29_50 <= heap_bh11_w13_5_d1 & heap_bh11_w13_4_d1;
      Compressor_bh11_29: Compressor_23_3
      port map ( R => CompressorOut_bh11_29_29,
                 X0 => CompressorIn_bh11_29_49,
                 X1 => CompressorIn_bh11_29_50);
   heap_bh11_w12_10 <= CompressorOut_bh11_29_29(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w13_6 <= CompressorOut_bh11_29_29(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w14_6 <= CompressorOut_bh11_29_29(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_30_51 <= heap_bh11_w20_1_d1 & heap_bh11_w20_0_d1 & heap_bh11_w20_2;
   CompressorIn_bh11_30_52 <= heap_bh11_w21_1_d1 & heap_bh11_w21_0_d1;
      Compressor_bh11_30: Compressor_23_3
      port map ( R => CompressorOut_bh11_30_30,
                 X0 => CompressorIn_bh11_30_51,
                 X1 => CompressorIn_bh11_30_52);
   heap_bh11_w20_3 <= CompressorOut_bh11_30_30(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w21_2 <= CompressorOut_bh11_30_30(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w22_2 <= CompressorOut_bh11_30_30(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_31_53 <= heap_bh11_w9_11_d1 & heap_bh11_w9_10_d1 & heap_bh11_w9_12;
   CompressorIn_bh11_31_54(0) <= heap_bh11_w10_9;
      Compressor_bh11_31: Compressor_13_3
      port map ( R => CompressorOut_bh11_31_31,
                 X0 => CompressorIn_bh11_31_53,
                 X1 => CompressorIn_bh11_31_54);
   heap_bh11_w9_13 <= CompressorOut_bh11_31_31(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w10_10 <= CompressorOut_bh11_31_31(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh11_w11_11 <= CompressorOut_bh11_31_31(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh11_32_55 <= heap_bh11_w22_1_d1 & heap_bh11_w22_0_d1 & heap_bh11_w22_2;
   CompressorIn_bh11_32_56 <= heap_bh11_w23_1_d1 & heap_bh11_w23_0_d1;
      Compressor_bh11_32: Compressor_23_3
      port map ( R => CompressorOut_bh11_32_32,
                 X0 => CompressorIn_bh11_32_55,
                 X1 => CompressorIn_bh11_32_56);
   heap_bh11_w22_3 <= CompressorOut_bh11_32_32(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh11_w23_2 <= CompressorOut_bh11_32_32(1); -- cycle= 1 cp= 1.06144e-09
   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   finalAdderIn0_bh11 <= "0" & heap_bh11_w23_2_d1 & heap_bh11_w22_3_d1 & heap_bh11_w21_2_d1 & heap_bh11_w20_3_d1 & heap_bh11_w19_2_d1 & heap_bh11_w18_3_d1 & heap_bh11_w17_2_d2 & heap_bh11_w16_3_d2 & heap_bh11_w15_2_d2 & heap_bh11_w14_5_d2 & heap_bh11_w13_6_d1 & heap_bh11_w12_10_d1 & heap_bh11_w11_10_d1 & heap_bh11_w10_10_d1 & heap_bh11_w9_13_d1 & heap_bh11_w8_10_d2 & heap_bh11_w7_14_d1 & heap_bh11_w6_11_d2 & heap_bh11_w5_14_d1 & heap_bh11_w4_11_d2 & heap_bh11_w3_12_d2 & heap_bh11_w2_8_d2 & heap_bh11_w1_6_d2 & heap_bh11_w0_0_d2;
   finalAdderIn1_bh11 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh11_w15_3_d2 & heap_bh11_w14_6_d1 & '0' & '0' & heap_bh11_w11_11_d1 & '0' & '0' & heap_bh11_w8_13_d1 & heap_bh11_w7_13_d1 & heap_bh11_w6_14_d1 & '0' & heap_bh11_w4_10_d2 & heap_bh11_w3_11_d2 & '0' & heap_bh11_w1_5_d2 & heap_bh11_w0_5_d2;
   finalAdderCin_bh11 <= '0';
      Adder_final11_0: IntAdder_25_f400_uid128  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh11,
                 R => finalAdderOut_bh11,
                 X => finalAdderIn0_bh11,
                 Y => finalAdderIn1_bh11);
   -- concatenate all the compressed chunks
   CompressionResult11 <= finalAdderOut_bh11;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult11(23 downto 4);
end architecture;

--------------------------------------------------------------------------------
--                          IntAdder_31_f400_uid161
--                     (IntAdderClassical_31_F400_uid163)
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

entity IntAdder_31_f400_uid161 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(30 downto 0);
          Y : in  std_logic_vector(30 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(30 downto 0)   );
end entity;

architecture arch of IntAdder_31_f400_uid161 is
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
--                  FixMultAdd_17x20p28r29signed_F400_uid138
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

entity FixMultAdd_17x20p28r29signed_F400_uid138 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(16 downto 0);
          Y : in  std_logic_vector(19 downto 0);
          A : in  std_logic_vector(27 downto 0);
          R : out  std_logic_vector(28 downto 0)   );
end entity;

architecture arch of FixMultAdd_17x20p28r29signed_F400_uid138 is
   component IntAdder_31_f400_uid161 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(30 downto 0);
             Y : in  std_logic_vector(30 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(30 downto 0)   );
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

signal XX_m141, XX_m141_d1 :  std_logic_vector(19 downto 0);
signal YY_m141, YY_m141_d1 :  std_logic_vector(16 downto 0);
signal DSP_Res_140 :  std_logic_vector(42 downto 0);
signal heap_bh139_w0_0 :  std_logic;
signal heap_bh139_w1_0 :  std_logic;
signal heap_bh139_w2_0 :  std_logic;
signal heap_bh139_w3_0 :  std_logic;
signal heap_bh139_w4_0, heap_bh139_w4_0_d1, heap_bh139_w4_0_d2 :  std_logic;
signal heap_bh139_w5_0, heap_bh139_w5_0_d1, heap_bh139_w5_0_d2 :  std_logic;
signal heap_bh139_w6_0, heap_bh139_w6_0_d1, heap_bh139_w6_0_d2 :  std_logic;
signal heap_bh139_w7_0, heap_bh139_w7_0_d1, heap_bh139_w7_0_d2 :  std_logic;
signal heap_bh139_w8_0, heap_bh139_w8_0_d1, heap_bh139_w8_0_d2 :  std_logic;
signal heap_bh139_w9_0, heap_bh139_w9_0_d1, heap_bh139_w9_0_d2 :  std_logic;
signal heap_bh139_w10_0, heap_bh139_w10_0_d1, heap_bh139_w10_0_d2 :  std_logic;
signal heap_bh139_w11_0, heap_bh139_w11_0_d1, heap_bh139_w11_0_d2 :  std_logic;
signal heap_bh139_w12_0, heap_bh139_w12_0_d1, heap_bh139_w12_0_d2 :  std_logic;
signal heap_bh139_w13_0, heap_bh139_w13_0_d1, heap_bh139_w13_0_d2 :  std_logic;
signal heap_bh139_w14_0, heap_bh139_w14_0_d1, heap_bh139_w14_0_d2 :  std_logic;
signal heap_bh139_w15_0, heap_bh139_w15_0_d1, heap_bh139_w15_0_d2 :  std_logic;
signal heap_bh139_w16_0, heap_bh139_w16_0_d1, heap_bh139_w16_0_d2 :  std_logic;
signal heap_bh139_w17_0, heap_bh139_w17_0_d1, heap_bh139_w17_0_d2 :  std_logic;
signal heap_bh139_w18_0, heap_bh139_w18_0_d1, heap_bh139_w18_0_d2 :  std_logic;
signal heap_bh139_w19_0, heap_bh139_w19_0_d1, heap_bh139_w19_0_d2 :  std_logic;
signal heap_bh139_w20_0, heap_bh139_w20_0_d1, heap_bh139_w20_0_d2 :  std_logic;
signal heap_bh139_w21_0, heap_bh139_w21_0_d1, heap_bh139_w21_0_d2 :  std_logic;
signal heap_bh139_w22_0, heap_bh139_w22_0_d1, heap_bh139_w22_0_d2 :  std_logic;
signal heap_bh139_w23_0, heap_bh139_w23_0_d1, heap_bh139_w23_0_d2 :  std_logic;
signal heap_bh139_w24_0, heap_bh139_w24_0_d1, heap_bh139_w24_0_d2 :  std_logic;
signal heap_bh139_w25_0 :  std_logic;
signal heap_bh139_w6_1, heap_bh139_w6_1_d1, heap_bh139_w6_1_d2 :  std_logic;
signal heap_bh139_w7_1, heap_bh139_w7_1_d1, heap_bh139_w7_1_d2 :  std_logic;
signal heap_bh139_w8_1, heap_bh139_w8_1_d1, heap_bh139_w8_1_d2 :  std_logic;
signal heap_bh139_w9_1, heap_bh139_w9_1_d1, heap_bh139_w9_1_d2 :  std_logic;
signal heap_bh139_w10_1, heap_bh139_w10_1_d1, heap_bh139_w10_1_d2 :  std_logic;
signal heap_bh139_w11_1, heap_bh139_w11_1_d1, heap_bh139_w11_1_d2 :  std_logic;
signal heap_bh139_w12_1, heap_bh139_w12_1_d1, heap_bh139_w12_1_d2 :  std_logic;
signal heap_bh139_w13_1, heap_bh139_w13_1_d1, heap_bh139_w13_1_d2 :  std_logic;
signal heap_bh139_w14_1, heap_bh139_w14_1_d1, heap_bh139_w14_1_d2 :  std_logic;
signal heap_bh139_w15_1, heap_bh139_w15_1_d1, heap_bh139_w15_1_d2 :  std_logic;
signal heap_bh139_w16_1, heap_bh139_w16_1_d1, heap_bh139_w16_1_d2 :  std_logic;
signal heap_bh139_w17_1, heap_bh139_w17_1_d1, heap_bh139_w17_1_d2 :  std_logic;
signal heap_bh139_w18_1, heap_bh139_w18_1_d1, heap_bh139_w18_1_d2 :  std_logic;
signal heap_bh139_w19_1, heap_bh139_w19_1_d1, heap_bh139_w19_1_d2 :  std_logic;
signal heap_bh139_w20_1, heap_bh139_w20_1_d1, heap_bh139_w20_1_d2 :  std_logic;
signal heap_bh139_w21_1, heap_bh139_w21_1_d1, heap_bh139_w21_1_d2 :  std_logic;
signal heap_bh139_w22_1, heap_bh139_w22_1_d1, heap_bh139_w22_1_d2 :  std_logic;
signal heap_bh139_w23_1, heap_bh139_w23_1_d1, heap_bh139_w23_1_d2 :  std_logic;
signal heap_bh139_w24_1, heap_bh139_w24_1_d1, heap_bh139_w24_1_d2 :  std_logic;
signal heap_bh139_w25_1 :  std_logic;
signal heap_bh139_w26_0 :  std_logic;
signal heap_bh139_w27_0 :  std_logic;
signal heap_bh139_w28_0 :  std_logic;
signal heap_bh139_w29_0 :  std_logic;
signal heap_bh139_w30_0 :  std_logic;
signal heap_bh139_w31_0, heap_bh139_w31_0_d1 :  std_logic;
signal heap_bh139_w32_0, heap_bh139_w32_0_d1 :  std_logic;
signal heap_bh139_w33_0, heap_bh139_w33_0_d1 :  std_logic;
signal heap_bh139_w4_1, heap_bh139_w4_1_d1, heap_bh139_w4_1_d2, heap_bh139_w4_1_d3 :  std_logic;
signal heap_bh139_w25_2, heap_bh139_w25_2_d1 :  std_logic;
signal heap_bh139_w26_1, heap_bh139_w26_1_d1 :  std_logic;
signal heap_bh139_w27_1, heap_bh139_w27_1_d1 :  std_logic;
signal heap_bh139_w28_1, heap_bh139_w28_1_d1 :  std_logic;
signal heap_bh139_w29_1, heap_bh139_w29_1_d1 :  std_logic;
signal heap_bh139_w30_1, heap_bh139_w30_1_d1 :  std_logic;
signal heap_bh139_w31_1, heap_bh139_w31_1_d1, heap_bh139_w31_1_d2 :  std_logic;
signal heap_bh139_w32_1, heap_bh139_w32_1_d1, heap_bh139_w32_1_d2 :  std_logic;
signal heap_bh139_w33_1, heap_bh139_w33_1_d1, heap_bh139_w33_1_d2 :  std_logic;
signal tempR_bh139_0, tempR_bh139_0_d1, tempR_bh139_0_d2 :  std_logic_vector(3 downto 0);
signal CompressorIn_bh139_0_0 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh139_0_1 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh139_0_0 :  std_logic_vector(2 downto 0);
signal heap_bh139_w25_3, heap_bh139_w25_3_d1, heap_bh139_w25_3_d2 :  std_logic;
signal heap_bh139_w26_2, heap_bh139_w26_2_d1, heap_bh139_w26_2_d2 :  std_logic;
signal heap_bh139_w27_2 :  std_logic;
signal CompressorIn_bh139_1_2 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh139_1_3 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh139_1_1 :  std_logic_vector(2 downto 0);
signal heap_bh139_w27_3, heap_bh139_w27_3_d1, heap_bh139_w27_3_d2 :  std_logic;
signal heap_bh139_w28_2, heap_bh139_w28_2_d1, heap_bh139_w28_2_d2 :  std_logic;
signal heap_bh139_w29_2 :  std_logic;
signal CompressorIn_bh139_2_4 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh139_2_5 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh139_2_2 :  std_logic_vector(2 downto 0);
signal heap_bh139_w29_3, heap_bh139_w29_3_d1, heap_bh139_w29_3_d2 :  std_logic;
signal heap_bh139_w30_2, heap_bh139_w30_2_d1, heap_bh139_w30_2_d2 :  std_logic;
signal heap_bh139_w31_2, heap_bh139_w31_2_d1 :  std_logic;
signal CompressorIn_bh139_3_6 :  std_logic_vector(2 downto 0);
signal CompressorIn_bh139_3_7 :  std_logic_vector(1 downto 0);
signal CompressorOut_bh139_3_3 :  std_logic_vector(2 downto 0);
signal heap_bh139_w31_3, heap_bh139_w31_3_d1 :  std_logic;
signal heap_bh139_w32_2, heap_bh139_w32_2_d1 :  std_logic;
signal heap_bh139_w33_2 :  std_logic;
signal CompressorIn_bh139_4_8 :  std_logic_vector(2 downto 0);
signal CompressorOut_bh139_4_4 :  std_logic_vector(1 downto 0);
signal heap_bh139_w33_3, heap_bh139_w33_3_d1 :  std_logic;
signal finalAdderIn0_bh139 :  std_logic_vector(30 downto 0);
signal finalAdderIn1_bh139 :  std_logic_vector(30 downto 0);
signal finalAdderCin_bh139 :  std_logic;
signal finalAdderOut_bh139 :  std_logic_vector(30 downto 0);
signal CompressionResult139 :  std_logic_vector(34 downto 0);
signal A_d1 :  std_logic_vector(27 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            XX_m141_d1 <=  XX_m141;
            YY_m141_d1 <=  YY_m141;
            heap_bh139_w4_0_d1 <=  heap_bh139_w4_0;
            heap_bh139_w4_0_d2 <=  heap_bh139_w4_0_d1;
            heap_bh139_w5_0_d1 <=  heap_bh139_w5_0;
            heap_bh139_w5_0_d2 <=  heap_bh139_w5_0_d1;
            heap_bh139_w6_0_d1 <=  heap_bh139_w6_0;
            heap_bh139_w6_0_d2 <=  heap_bh139_w6_0_d1;
            heap_bh139_w7_0_d1 <=  heap_bh139_w7_0;
            heap_bh139_w7_0_d2 <=  heap_bh139_w7_0_d1;
            heap_bh139_w8_0_d1 <=  heap_bh139_w8_0;
            heap_bh139_w8_0_d2 <=  heap_bh139_w8_0_d1;
            heap_bh139_w9_0_d1 <=  heap_bh139_w9_0;
            heap_bh139_w9_0_d2 <=  heap_bh139_w9_0_d1;
            heap_bh139_w10_0_d1 <=  heap_bh139_w10_0;
            heap_bh139_w10_0_d2 <=  heap_bh139_w10_0_d1;
            heap_bh139_w11_0_d1 <=  heap_bh139_w11_0;
            heap_bh139_w11_0_d2 <=  heap_bh139_w11_0_d1;
            heap_bh139_w12_0_d1 <=  heap_bh139_w12_0;
            heap_bh139_w12_0_d2 <=  heap_bh139_w12_0_d1;
            heap_bh139_w13_0_d1 <=  heap_bh139_w13_0;
            heap_bh139_w13_0_d2 <=  heap_bh139_w13_0_d1;
            heap_bh139_w14_0_d1 <=  heap_bh139_w14_0;
            heap_bh139_w14_0_d2 <=  heap_bh139_w14_0_d1;
            heap_bh139_w15_0_d1 <=  heap_bh139_w15_0;
            heap_bh139_w15_0_d2 <=  heap_bh139_w15_0_d1;
            heap_bh139_w16_0_d1 <=  heap_bh139_w16_0;
            heap_bh139_w16_0_d2 <=  heap_bh139_w16_0_d1;
            heap_bh139_w17_0_d1 <=  heap_bh139_w17_0;
            heap_bh139_w17_0_d2 <=  heap_bh139_w17_0_d1;
            heap_bh139_w18_0_d1 <=  heap_bh139_w18_0;
            heap_bh139_w18_0_d2 <=  heap_bh139_w18_0_d1;
            heap_bh139_w19_0_d1 <=  heap_bh139_w19_0;
            heap_bh139_w19_0_d2 <=  heap_bh139_w19_0_d1;
            heap_bh139_w20_0_d1 <=  heap_bh139_w20_0;
            heap_bh139_w20_0_d2 <=  heap_bh139_w20_0_d1;
            heap_bh139_w21_0_d1 <=  heap_bh139_w21_0;
            heap_bh139_w21_0_d2 <=  heap_bh139_w21_0_d1;
            heap_bh139_w22_0_d1 <=  heap_bh139_w22_0;
            heap_bh139_w22_0_d2 <=  heap_bh139_w22_0_d1;
            heap_bh139_w23_0_d1 <=  heap_bh139_w23_0;
            heap_bh139_w23_0_d2 <=  heap_bh139_w23_0_d1;
            heap_bh139_w24_0_d1 <=  heap_bh139_w24_0;
            heap_bh139_w24_0_d2 <=  heap_bh139_w24_0_d1;
            heap_bh139_w6_1_d1 <=  heap_bh139_w6_1;
            heap_bh139_w6_1_d2 <=  heap_bh139_w6_1_d1;
            heap_bh139_w7_1_d1 <=  heap_bh139_w7_1;
            heap_bh139_w7_1_d2 <=  heap_bh139_w7_1_d1;
            heap_bh139_w8_1_d1 <=  heap_bh139_w8_1;
            heap_bh139_w8_1_d2 <=  heap_bh139_w8_1_d1;
            heap_bh139_w9_1_d1 <=  heap_bh139_w9_1;
            heap_bh139_w9_1_d2 <=  heap_bh139_w9_1_d1;
            heap_bh139_w10_1_d1 <=  heap_bh139_w10_1;
            heap_bh139_w10_1_d2 <=  heap_bh139_w10_1_d1;
            heap_bh139_w11_1_d1 <=  heap_bh139_w11_1;
            heap_bh139_w11_1_d2 <=  heap_bh139_w11_1_d1;
            heap_bh139_w12_1_d1 <=  heap_bh139_w12_1;
            heap_bh139_w12_1_d2 <=  heap_bh139_w12_1_d1;
            heap_bh139_w13_1_d1 <=  heap_bh139_w13_1;
            heap_bh139_w13_1_d2 <=  heap_bh139_w13_1_d1;
            heap_bh139_w14_1_d1 <=  heap_bh139_w14_1;
            heap_bh139_w14_1_d2 <=  heap_bh139_w14_1_d1;
            heap_bh139_w15_1_d1 <=  heap_bh139_w15_1;
            heap_bh139_w15_1_d2 <=  heap_bh139_w15_1_d1;
            heap_bh139_w16_1_d1 <=  heap_bh139_w16_1;
            heap_bh139_w16_1_d2 <=  heap_bh139_w16_1_d1;
            heap_bh139_w17_1_d1 <=  heap_bh139_w17_1;
            heap_bh139_w17_1_d2 <=  heap_bh139_w17_1_d1;
            heap_bh139_w18_1_d1 <=  heap_bh139_w18_1;
            heap_bh139_w18_1_d2 <=  heap_bh139_w18_1_d1;
            heap_bh139_w19_1_d1 <=  heap_bh139_w19_1;
            heap_bh139_w19_1_d2 <=  heap_bh139_w19_1_d1;
            heap_bh139_w20_1_d1 <=  heap_bh139_w20_1;
            heap_bh139_w20_1_d2 <=  heap_bh139_w20_1_d1;
            heap_bh139_w21_1_d1 <=  heap_bh139_w21_1;
            heap_bh139_w21_1_d2 <=  heap_bh139_w21_1_d1;
            heap_bh139_w22_1_d1 <=  heap_bh139_w22_1;
            heap_bh139_w22_1_d2 <=  heap_bh139_w22_1_d1;
            heap_bh139_w23_1_d1 <=  heap_bh139_w23_1;
            heap_bh139_w23_1_d2 <=  heap_bh139_w23_1_d1;
            heap_bh139_w24_1_d1 <=  heap_bh139_w24_1;
            heap_bh139_w24_1_d2 <=  heap_bh139_w24_1_d1;
            heap_bh139_w31_0_d1 <=  heap_bh139_w31_0;
            heap_bh139_w32_0_d1 <=  heap_bh139_w32_0;
            heap_bh139_w33_0_d1 <=  heap_bh139_w33_0;
            heap_bh139_w4_1_d1 <=  heap_bh139_w4_1;
            heap_bh139_w4_1_d2 <=  heap_bh139_w4_1_d1;
            heap_bh139_w4_1_d3 <=  heap_bh139_w4_1_d2;
            heap_bh139_w25_2_d1 <=  heap_bh139_w25_2;
            heap_bh139_w26_1_d1 <=  heap_bh139_w26_1;
            heap_bh139_w27_1_d1 <=  heap_bh139_w27_1;
            heap_bh139_w28_1_d1 <=  heap_bh139_w28_1;
            heap_bh139_w29_1_d1 <=  heap_bh139_w29_1;
            heap_bh139_w30_1_d1 <=  heap_bh139_w30_1;
            heap_bh139_w31_1_d1 <=  heap_bh139_w31_1;
            heap_bh139_w31_1_d2 <=  heap_bh139_w31_1_d1;
            heap_bh139_w32_1_d1 <=  heap_bh139_w32_1;
            heap_bh139_w32_1_d2 <=  heap_bh139_w32_1_d1;
            heap_bh139_w33_1_d1 <=  heap_bh139_w33_1;
            heap_bh139_w33_1_d2 <=  heap_bh139_w33_1_d1;
            tempR_bh139_0_d1 <=  tempR_bh139_0;
            tempR_bh139_0_d2 <=  tempR_bh139_0_d1;
            heap_bh139_w25_3_d1 <=  heap_bh139_w25_3;
            heap_bh139_w25_3_d2 <=  heap_bh139_w25_3_d1;
            heap_bh139_w26_2_d1 <=  heap_bh139_w26_2;
            heap_bh139_w26_2_d2 <=  heap_bh139_w26_2_d1;
            heap_bh139_w27_3_d1 <=  heap_bh139_w27_3;
            heap_bh139_w27_3_d2 <=  heap_bh139_w27_3_d1;
            heap_bh139_w28_2_d1 <=  heap_bh139_w28_2;
            heap_bh139_w28_2_d2 <=  heap_bh139_w28_2_d1;
            heap_bh139_w29_3_d1 <=  heap_bh139_w29_3;
            heap_bh139_w29_3_d2 <=  heap_bh139_w29_3_d1;
            heap_bh139_w30_2_d1 <=  heap_bh139_w30_2;
            heap_bh139_w30_2_d2 <=  heap_bh139_w30_2_d1;
            heap_bh139_w31_2_d1 <=  heap_bh139_w31_2;
            heap_bh139_w31_3_d1 <=  heap_bh139_w31_3;
            heap_bh139_w32_2_d1 <=  heap_bh139_w32_2;
            heap_bh139_w33_3_d1 <=  heap_bh139_w33_3;
            A_d1 <=  A;
         end if;
      end process;
   XX_m141 <= Y ;
   YY_m141 <= X ;
   ----------------Synchro barrier, entering cycle 1----------------
   DSP_Res_140 <=  std_logic_vector(signed'(signed((XX_m141_d1(19) & XX_m141_d1(19) & XX_m141_d1(19) & XX_m141_d1(19) & XX_m141_d1(19)) & XX_m141_d1) * signed((YY_m141_d1(16)) & YY_m141_d1)));
   heap_bh139_w0_0 <= DSP_Res_140(11); -- cycle= 1 cp= 0
   heap_bh139_w1_0 <= DSP_Res_140(12); -- cycle= 1 cp= 0
   heap_bh139_w2_0 <= DSP_Res_140(13); -- cycle= 1 cp= 0
   heap_bh139_w3_0 <= DSP_Res_140(14); -- cycle= 1 cp= 0
   heap_bh139_w4_0 <= DSP_Res_140(15); -- cycle= 1 cp= 0
   heap_bh139_w5_0 <= DSP_Res_140(16); -- cycle= 1 cp= 0
   heap_bh139_w6_0 <= DSP_Res_140(17); -- cycle= 1 cp= 0
   heap_bh139_w7_0 <= DSP_Res_140(18); -- cycle= 1 cp= 0
   heap_bh139_w8_0 <= DSP_Res_140(19); -- cycle= 1 cp= 0
   heap_bh139_w9_0 <= DSP_Res_140(20); -- cycle= 1 cp= 0
   heap_bh139_w10_0 <= DSP_Res_140(21); -- cycle= 1 cp= 0
   heap_bh139_w11_0 <= DSP_Res_140(22); -- cycle= 1 cp= 0
   heap_bh139_w12_0 <= DSP_Res_140(23); -- cycle= 1 cp= 0
   heap_bh139_w13_0 <= DSP_Res_140(24); -- cycle= 1 cp= 0
   heap_bh139_w14_0 <= DSP_Res_140(25); -- cycle= 1 cp= 0
   heap_bh139_w15_0 <= DSP_Res_140(26); -- cycle= 1 cp= 0
   heap_bh139_w16_0 <= DSP_Res_140(27); -- cycle= 1 cp= 0
   heap_bh139_w17_0 <= DSP_Res_140(28); -- cycle= 1 cp= 0
   heap_bh139_w18_0 <= DSP_Res_140(29); -- cycle= 1 cp= 0
   heap_bh139_w19_0 <= DSP_Res_140(30); -- cycle= 1 cp= 0
   heap_bh139_w20_0 <= DSP_Res_140(31); -- cycle= 1 cp= 0
   heap_bh139_w21_0 <= DSP_Res_140(32); -- cycle= 1 cp= 0
   heap_bh139_w22_0 <= DSP_Res_140(33); -- cycle= 1 cp= 0
   heap_bh139_w23_0 <= DSP_Res_140(34); -- cycle= 1 cp= 0
   heap_bh139_w24_0 <= DSP_Res_140(35); -- cycle= 1 cp= 0
   heap_bh139_w25_0 <= not(DSP_Res_140(36)); -- cycle= 1 cp= 0
   heap_bh139_w6_1 <= A_d1(0); -- cycle= 1 cp= 0
   heap_bh139_w7_1 <= A_d1(1); -- cycle= 1 cp= 0
   heap_bh139_w8_1 <= A_d1(2); -- cycle= 1 cp= 0
   heap_bh139_w9_1 <= A_d1(3); -- cycle= 1 cp= 0
   heap_bh139_w10_1 <= A_d1(4); -- cycle= 1 cp= 0
   heap_bh139_w11_1 <= A_d1(5); -- cycle= 1 cp= 0
   heap_bh139_w12_1 <= A_d1(6); -- cycle= 1 cp= 0
   heap_bh139_w13_1 <= A_d1(7); -- cycle= 1 cp= 0
   heap_bh139_w14_1 <= A_d1(8); -- cycle= 1 cp= 0
   heap_bh139_w15_1 <= A_d1(9); -- cycle= 1 cp= 0
   heap_bh139_w16_1 <= A_d1(10); -- cycle= 1 cp= 0
   heap_bh139_w17_1 <= A_d1(11); -- cycle= 1 cp= 0
   heap_bh139_w18_1 <= A_d1(12); -- cycle= 1 cp= 0
   heap_bh139_w19_1 <= A_d1(13); -- cycle= 1 cp= 0
   heap_bh139_w20_1 <= A_d1(14); -- cycle= 1 cp= 0
   heap_bh139_w21_1 <= A_d1(15); -- cycle= 1 cp= 0
   heap_bh139_w22_1 <= A_d1(16); -- cycle= 1 cp= 0
   heap_bh139_w23_1 <= A_d1(17); -- cycle= 1 cp= 0
   heap_bh139_w24_1 <= A_d1(18); -- cycle= 1 cp= 0
   heap_bh139_w25_1 <= A_d1(19); -- cycle= 1 cp= 0
   heap_bh139_w26_0 <= A_d1(20); -- cycle= 1 cp= 0
   heap_bh139_w27_0 <= A_d1(21); -- cycle= 1 cp= 0
   heap_bh139_w28_0 <= A_d1(22); -- cycle= 1 cp= 0
   heap_bh139_w29_0 <= A_d1(23); -- cycle= 1 cp= 0
   heap_bh139_w30_0 <= A_d1(24); -- cycle= 1 cp= 0
   heap_bh139_w31_0 <= A_d1(25); -- cycle= 1 cp= 0
   heap_bh139_w32_0 <= A_d1(26); -- cycle= 1 cp= 0
   heap_bh139_w33_0 <= A_d1(27); -- cycle= 1 cp= 0
   
   -- Beginning of code generated by BitHeap::generateCompressorVHDL
   -- code generated by BitHeap::generateSupertileVHDL()
   ----------------Synchro barrier, entering cycle 0----------------

   -- Adding the constant bits
   heap_bh139_w4_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w25_2 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w26_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w27_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w28_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w29_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w30_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w31_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w32_1 <= '1'; -- cycle= 0 cp= 0
   heap_bh139_w33_1 <= '1'; -- cycle= 0 cp= 0

   ----------------Synchro barrier, entering cycle 0----------------
   ----------------Synchro barrier, entering cycle 1----------------
   tempR_bh139_0 <= heap_bh139_w3_0 & heap_bh139_w2_0 & heap_bh139_w1_0 & heap_bh139_w0_0; -- already compressed

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh139_0_0 <= heap_bh139_w25_2_d1 & heap_bh139_w25_1 & heap_bh139_w25_0;
   CompressorIn_bh139_0_1 <= heap_bh139_w26_1_d1 & heap_bh139_w26_0;
      Compressor_bh139_0: Compressor_23_3
      port map ( R => CompressorOut_bh139_0_0,
                 X0 => CompressorIn_bh139_0_0,
                 X1 => CompressorIn_bh139_0_1);
   heap_bh139_w25_3 <= CompressorOut_bh139_0_0(0); -- cycle= 1 cp= 5.3072e-10
   heap_bh139_w26_2 <= CompressorOut_bh139_0_0(1); -- cycle= 1 cp= 5.3072e-10
   heap_bh139_w27_2 <= CompressorOut_bh139_0_0(2); -- cycle= 1 cp= 5.3072e-10

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh139_1_2 <= heap_bh139_w27_1_d1 & heap_bh139_w27_0 & heap_bh139_w27_2;
   CompressorIn_bh139_1_3 <= heap_bh139_w28_1_d1 & heap_bh139_w28_0;
      Compressor_bh139_1: Compressor_23_3
      port map ( R => CompressorOut_bh139_1_1,
                 X0 => CompressorIn_bh139_1_2,
                 X1 => CompressorIn_bh139_1_3);
   heap_bh139_w27_3 <= CompressorOut_bh139_1_1(0); -- cycle= 1 cp= 1.06144e-09
   heap_bh139_w28_2 <= CompressorOut_bh139_1_1(1); -- cycle= 1 cp= 1.06144e-09
   heap_bh139_w29_2 <= CompressorOut_bh139_1_1(2); -- cycle= 1 cp= 1.06144e-09

   ----------------Synchro barrier, entering cycle 1----------------
   CompressorIn_bh139_2_4 <= heap_bh139_w29_1_d1 & heap_bh139_w29_0 & heap_bh139_w29_2;
   CompressorIn_bh139_2_5 <= heap_bh139_w30_1_d1 & heap_bh139_w30_0;
      Compressor_bh139_2: Compressor_23_3
      port map ( R => CompressorOut_bh139_2_2,
                 X0 => CompressorIn_bh139_2_4,
                 X1 => CompressorIn_bh139_2_5);
   heap_bh139_w29_3 <= CompressorOut_bh139_2_2(0); -- cycle= 1 cp= 1.59216e-09
   heap_bh139_w30_2 <= CompressorOut_bh139_2_2(1); -- cycle= 1 cp= 1.59216e-09
   heap_bh139_w31_2 <= CompressorOut_bh139_2_2(2); -- cycle= 1 cp= 1.59216e-09

   ----------------Synchro barrier, entering cycle 1----------------
   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh139_3_6 <= heap_bh139_w31_1_d2 & heap_bh139_w31_0_d1 & heap_bh139_w31_2_d1;
   CompressorIn_bh139_3_7 <= heap_bh139_w32_1_d2 & heap_bh139_w32_0_d1;
      Compressor_bh139_3: Compressor_23_3
      port map ( R => CompressorOut_bh139_3_3,
                 X0 => CompressorIn_bh139_3_6,
                 X1 => CompressorIn_bh139_3_7);
   heap_bh139_w31_3 <= CompressorOut_bh139_3_3(0); -- cycle= 2 cp= 0
   heap_bh139_w32_2 <= CompressorOut_bh139_3_3(1); -- cycle= 2 cp= 0
   heap_bh139_w33_2 <= CompressorOut_bh139_3_3(2); -- cycle= 2 cp= 0

   ----------------Synchro barrier, entering cycle 2----------------
   CompressorIn_bh139_4_8 <= heap_bh139_w33_1_d2 & heap_bh139_w33_0_d1 & heap_bh139_w33_2;
      Compressor_bh139_4: Compressor_3_2
      port map ( R => CompressorOut_bh139_4_4,
                 X0 => CompressorIn_bh139_4_8);
   heap_bh139_w33_3 <= CompressorOut_bh139_4_4(0); -- cycle= 2 cp= 5.3072e-10
   ----------------Synchro barrier, entering cycle 2----------------
   ----------------Synchro barrier, entering cycle 3----------------
   finalAdderIn0_bh139 <= "0" & heap_bh139_w33_3_d1 & heap_bh139_w32_2_d1 & heap_bh139_w31_3_d1 & heap_bh139_w30_2_d2 & heap_bh139_w29_3_d2 & heap_bh139_w28_2_d2 & heap_bh139_w27_3_d2 & heap_bh139_w26_2_d2 & heap_bh139_w25_3_d2 & heap_bh139_w24_1_d2 & heap_bh139_w23_1_d2 & heap_bh139_w22_1_d2 & heap_bh139_w21_1_d2 & heap_bh139_w20_1_d2 & heap_bh139_w19_1_d2 & heap_bh139_w18_1_d2 & heap_bh139_w17_1_d2 & heap_bh139_w16_1_d2 & heap_bh139_w15_1_d2 & heap_bh139_w14_1_d2 & heap_bh139_w13_1_d2 & heap_bh139_w12_1_d2 & heap_bh139_w11_1_d2 & heap_bh139_w10_1_d2 & heap_bh139_w9_1_d2 & heap_bh139_w8_1_d2 & heap_bh139_w7_1_d2 & heap_bh139_w6_1_d2 & heap_bh139_w5_0_d2 & heap_bh139_w4_1_d3;
   finalAdderIn1_bh139 <= "0" & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & '0' & heap_bh139_w24_0_d2 & heap_bh139_w23_0_d2 & heap_bh139_w22_0_d2 & heap_bh139_w21_0_d2 & heap_bh139_w20_0_d2 & heap_bh139_w19_0_d2 & heap_bh139_w18_0_d2 & heap_bh139_w17_0_d2 & heap_bh139_w16_0_d2 & heap_bh139_w15_0_d2 & heap_bh139_w14_0_d2 & heap_bh139_w13_0_d2 & heap_bh139_w12_0_d2 & heap_bh139_w11_0_d2 & heap_bh139_w10_0_d2 & heap_bh139_w9_0_d2 & heap_bh139_w8_0_d2 & heap_bh139_w7_0_d2 & heap_bh139_w6_0_d2 & '0' & heap_bh139_w4_0_d2;
   finalAdderCin_bh139 <= '0';
      Adder_final139_0: IntAdder_31_f400_uid161  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 Cin => finalAdderCin_bh139,
                 R => finalAdderOut_bh139,
                 X => finalAdderIn0_bh139,
                 Y => finalAdderIn1_bh139);
   -- concatenate all the compressed chunks
   CompressionResult139 <= finalAdderOut_bh139 & tempR_bh139_0_d2;
   -- End of code generated by BitHeap::generateCompressorVHDL
   R <= CompressionResult139(33 downto 5);
end architecture;

--------------------------------------------------------------------------------
--                        FixHornerEvaluator_F400_uid8
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: F. de Dinechin (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 7 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FixHornerEvaluator_F400_uid8 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(16 downto 0);
          A0 : in  std_logic_vector(27 downto 0);
          A1 : in  std_logic_vector(18 downto 0);
          A2 : in  std_logic_vector(9 downto 0);
          R : out  std_logic_vector(24 downto 0)   );
end entity;

architecture arch of FixHornerEvaluator_F400_uid8 is
   component FixMultAdd_10x10p19r20signed_F400_uid10 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(9 downto 0);
             Y : in  std_logic_vector(9 downto 0);
             A : in  std_logic_vector(18 downto 0);
             R : out  std_logic_vector(19 downto 0)   );
   end component;

   component FixMultAdd_17x20p28r29signed_F400_uid138 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(16 downto 0);
             Y : in  std_logic_vector(19 downto 0);
             A : in  std_logic_vector(27 downto 0);
             R : out  std_logic_vector(28 downto 0)   );
   end component;

signal Xs, Xs_d1, Xs_d2, Xs_d3 :  signed(0+16 downto 0);
signal As0, As0_d1, As0_d2, As0_d3 :  signed(1+26 downto 0);
signal As1 :  signed(-8+26 downto 0);
signal As2 :  signed(-17+26 downto 0);
signal Sigma2 :  signed(-17+26 downto 0);
signal XsTrunc1 :  signed(0+9 downto 0);
signal Sigma1_slv :  std_logic_vector(19 downto 0);
signal Sigma1, Sigma1_d1 :  signed(-8+27 downto 0);
signal XsTrunc0 :  signed(0+16 downto 0);
signal Sigma0_slv :  std_logic_vector(28 downto 0);
signal Sigma0, Sigma0_d1 :  signed(1+27 downto 0);
signal Ys :  signed(1+23 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
            Xs_d1 <=  Xs;
            Xs_d2 <=  Xs_d1;
            Xs_d3 <=  Xs_d2;
            As0_d1 <=  As0;
            As0_d2 <=  As0_d1;
            As0_d3 <=  As0_d2;
            Sigma1_d1 <=  Sigma1;
            Sigma0_d1 <=  Sigma0;
         end if;
      end process;
   Xs <= signed(X);
   As0 <= signed(A0);
   As1 <= signed(A1);
   As2 <= signed(A2);
   Sigma2 <= As2;
   XsTrunc1 <= Xs(16 downto 7); -- fix resize from (0, -16) to (0, -9)
   Step1: FixMultAdd_10x10p19r20signed_F400_uid10  -- pipelineDepth=2 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As1),
                 R => Sigma1_slv,
                 X => std_logic_vector(XsTrunc1),
                 Y => std_logic_vector(Sigma2));
   ----------------Synchro barrier, entering cycle 2----------------
   Sigma1 <= signed(Sigma1_slv);
   ----------------Synchro barrier, entering cycle 3----------------
   XsTrunc0 <= Xs_d3(16 downto 0); -- fix resize from (0, -16) to (0, -16)
   Step0: FixMultAdd_17x20p28r29signed_F400_uid138  -- pipelineDepth=3 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A => std_logic_vector(As0_d3),
                 R => Sigma0_slv,
                 X => std_logic_vector(XsTrunc0),
                 Y => std_logic_vector(Sigma1_d1));
   ----------------Synchro barrier, entering cycle 6----------------
   Sigma0 <= signed(Sigma0_slv);
   ----------------Synchro barrier, entering cycle 7----------------
   Ys <= Sigma0_d1(28 downto 4); -- fix resize from (1, -27) to (1, -23)
   R <= std_logic_vector(Ys);
end architecture;

--------------------------------------------------------------------------------
--                    FixFunctionByPiecewisePoly_F400_uid2
-- Evaluator for sqrt(1+x) on [0,1) for lsbIn=-23 (wIn=23), msbout=1, lsbOut=-23
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin (2014)
--------------------------------------------------------------------------------
-- Pipeline depth: 7 cycles

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity FixFunctionByPiecewisePoly_F400_uid2 is
   port ( clk, rst : in std_logic;
          X : in  std_logic_vector(22 downto 0);
          Y : out  std_logic_vector(24 downto 0)   );
end entity;

architecture arch of FixFunctionByPiecewisePoly_F400_uid2 is
   component GenericTable_6_54_F400_uid4 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(5 downto 0);
             Y : out  std_logic_vector(53 downto 0)   );
   end component;

   component FixHornerEvaluator_F400_uid8 is
      port ( clk, rst : in std_logic;
             X : in  std_logic_vector(16 downto 0);
             A0 : in  std_logic_vector(27 downto 0);
             A1 : in  std_logic_vector(18 downto 0);
             A2 : in  std_logic_vector(9 downto 0);
             R : out  std_logic_vector(24 downto 0)   );
   end component;

signal A :  std_logic_vector(5 downto 0);
signal Z :  std_logic_vector(16 downto 0);
signal Zs :  std_logic_vector(16 downto 0);
signal Coeffs :  std_logic_vector(53 downto 0);
signal A2 :  std_logic_vector(9 downto 0);
signal A1 :  std_logic_vector(18 downto 0);
signal A0 :  std_logic_vector(27 downto 0);
signal Ys :  std_logic_vector(24 downto 0);
begin
   process(clk)
      begin
         if clk'event and clk = '1' then
         end if;
      end process;
   A <= X(22 downto 17);
   Z <= X(16 downto 0);
   Zs <= (not Z(16)) & Z(15 downto 0); -- centering the interval
   coeffTable: GenericTable_6_54_F400_uid4  -- pipelineDepth=0 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 X => A,
                 Y => Coeffs);

   A2 <= "1" & Coeffs(8 downto 0);
   A1 <= "0" & Coeffs(26 downto 9);
   A0 <= "0" & Coeffs(53 downto 27);
   horner: FixHornerEvaluator_F400_uid8  -- pipelineDepth=7 maxInDelay=0
      port map ( clk  => clk,
                 rst  => rst,
                 A0 => A0,
                 A1 => A1,
                 A2 => A2,
                 R => Ys,
                 X => Zs);

   ----------------Synchro barrier, entering cycle 7----------------
   Y <= std_logic_vector(Ys);
end architecture;

