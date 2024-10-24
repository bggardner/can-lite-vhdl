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
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0')
    );
end package CanBus;

package body CanBus is
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
        constant FILTER_MASK : std_logic_vector(28 downto 0) := (others => '0')
    ) is
    begin
        loop
            wait until FifoWriteEnable = '1' and rising_edge(Clock);
            if (FifoFrame.Id and FILTER_MASK) = (FILTER_ID and FILTER_MASK) then
                Frame := FifoFrame;
                exit;
            end if;
        end loop;
    end FifoToFrame;

end package body CanBus;