library ieee;
use ieee.std_logic_1164.all;
    
package CanBus is
    ------------------------------------------------------------
    --! TYPES
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
        ErrorWarning    : std_logic; --! '0' = (< 97 errors), '1' = (> 96 errors)
        Overflow        : std_logic; --! '0' = (no overflow), '1' = RX FIFO overflow
    end record Status;
    
end package CanBus;

package body CanBus is

end package body CanBus;