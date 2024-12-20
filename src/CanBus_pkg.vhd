library ieee;
    use ieee.std_logic_1164.all;
    
package CanBus is
    ------------------------------------------------------------
    -- TYPES
    ------------------------------------------------------------
    type DataBytes is array (7 downto 0) of std_logic_vector(7 downto 0);
    
    type State is (
        STATE_RESET,
        STATE_ERROR_ACTIVE,
        STATE_ERROR_PASSIVE,
        STATE_BUS_OFF
    );
    
    type Frame is record
        Id      : std_logic_vector(28 downto 0);
        Rtr     : std_logic;
        Ide     : std_logic;
        Dlc     : std_logic_vector(3 downto 0);
        Data    : DataBytes;
    end record Frame;

    type Status is record
        State           : State;
        ErrorWarning    : std_logic; -- '0' = (< 97 errors), '1' = (> 96 errors)
        Overflow        : std_logic; -- '0' = (no overflow), '1' = RX FIFO overflow
    end record Status;
    
    ------------------------------------------------------------
    -- FUNCTIONS
    ------------------------------------------------------------
    function is_match(
        constant FRAME : Frame;
        constant FILTER_ID : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_IDE : std_logic := '0' -- '0' = match 11-bit ID, '1' = match 29-bit ID
    ) return boolean;
    function to_std_logic_vector(constant DATA_BYTES : DataBytes) return std_logic_vector;
    function to_DataBytes(constant SLV : std_logic_vector(63 downto 0)) return DataBytes;

    ------------------------------------------------------------
    -- TESTBENCH PROCEDURES
    ------------------------------------------------------------
    procedure FrameToFifo (
        constant Frame : in Frame;
        signal Clock : in std_logic;
        signal FifoReadEnable : in std_logic;
        signal FifoEmpty : out std_logic;
        signal FifoFrame : out Frame
    );
    
    procedure FifoToFrame (
        signal Clock : in std_logic;
        signal FifoFrame : in Frame;
        signal FifoWriteEnable : in std_logic;
        variable Frame : out Frame;
        constant FILTER_ID : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_IDE : std_logic := '0' -- '0' = match 11-bit ID, '1' = match 29-bit ID
    );
end package CanBus;

package body CanBus is
    ------------------------------------------------------------
    -- FUNCTIONS
    ------------------------------------------------------------
    function is_match(
        constant FRAME : Frame;
        constant FILTER_ID : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_IDE : std_logic := '0' -- '0' = match 11-bit ID, '1' = match 29-bit ID
    ) return boolean is
        variable ID_MASK : std_logic_vector(28 downto 0);
    begin
        if FILTER_IDE = '0' then
            ID_MASK := FILTER_MASK and "00000000000000000011111111111";
        else
            ID_MASK := FILTER_MASK;
        end if;
        return (FRAME.Ide = FILTER_IDE) and ((FRAME.Id and ID_MASK) = (FILTER_ID and ID_MASK));
    end function is_match;

    function to_std_logic_vector(constant DATA_BYTES : DataBytes) return std_logic_vector is
        variable Slv : std_logic_vector(63 downto 0);
    begin
        for Byte in 0 to 7 loop
            Slv((Byte + 1) * 8 - 1 downto Byte * 8) := DATA_BYTES(Byte);
        end loop;
        return Slv;
    end function to_std_logic_vector;
    
    function to_DataBytes(constant SLV : std_logic_vector(63 downto 0)) return DataBytes is
        variable Data : DataBytes;
    begin
        for Byte in 0 to 7 loop
            Data(Byte) := SLV((Byte + 1) * 8 - 1 downto Byte * 8);
        end loop;
        return Data;
    end function to_DataBytes;

    ------------------------------------------------------------
    -- TESTBENCH PROCEDURES
    ------------------------------------------------------------
    procedure FrameToFifo (
        constant Frame : in Frame;
        signal Clock : in std_logic;
        signal FifoReadEnable : in std_logic;
        signal FifoEmpty : out std_logic;
        signal FifoFrame : out Frame
    ) is
    begin
        wait until rising_edge(Clock);
        FifoEmpty <= '0';
        wait until FifoReadEnable = '1';
        wait until rising_edge(Clock);
        FifoFrame <= Frame;
        FifoEmpty <= '1';
        wait until rising_edge(Clock);
    end FrameToFifo;

    procedure FifoToFrame (
        signal Clock : in std_logic;
        signal FifoFrame : in Frame;
        signal FifoWriteEnable : in std_logic;
        variable Frame : out Frame;
        constant FILTER_ID : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0');
        constant FILTER_IDE : std_logic := '0' -- '0' = match 11-bit ID, '1' = match 29-bit ID
    ) is
    begin
        loop
            wait until FifoWriteEnable = '1' and rising_edge(Clock);
            if is_match(FifoFrame, FILTER_ID, FILTER_MASK, FILTER_IDE) then
                Frame := FifoFrame;
                exit;
            end if;
        end loop;
    end FifoToFrame;

end package body CanBus;
