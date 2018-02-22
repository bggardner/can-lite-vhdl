library ieee;
use ieee.std_logic_1164.all;
use work.CanBus;

entity CanLite_tb is
end CanLite_tb;

architecture Behavioral of CanLite_tb is  
    component CanLite is
        generic ( --! Default configuration register values
            FIFO_DEPTH                  : positive;
            BAUD_RATE_PRESCALAR         : positive;
            SYNCHRONIZATION_JUMP_WIDTH  : positive;
            TIME_SEGMENT_1              : positive;
            TIME_SEGMENT_2              : positive;
            TRIPLE_SAMPLING             : boolean
        );
        port (
            AppClock    : in std_logic; --! Application clock
            Reset_n     : in std_logic;
            
            RxFrame     : out CanBus.Frame; --! CAN frame read from RX FIFO buffer
            RxStrobe    : in std_logic; --! High pulse for reading RxFrame from FIFO buffer
            RxFifoEmpty : out std_logic; --! Empty flag of RX FIFO buffer
                    
            TxFrame     : in CanBus.Frame; --! CAN frame to be loaded into TX FIFO buffer
            TxStrobe    : in std_logic; --! High pulse for loading TxFrame into FIFO buffer
            TxFifoFull  : out std_logic; --! High if TxFifo is full
            TxAck       : out std_logic; --! High pulse when a message was successfully transmitted
            
            Status   : out CanBus.Status; --! See CanBus_pkg.vhdl
            
            CanClock    : in std_logic; --! Base clock for CAN timing (24MHz recommended)
            CanRx       : in std_logic; --! RX input from CAN transceiver
            CanTx       : out std_logic --! TX output to CAN transceiver
        );
    end component CanLite;
    
    signal AppClock, CanClock   : std_logic;
    signal Reset_n              : std_logic;
    signal RxFrame, TxFrame     : CanBus.Frame;
    signal RxStrobe, TxStrobe   : std_logic;
    signal TxAck                : std_logic;
    signal Node1Status          : CanBus.Status;
    signal CanRx                : std_logic;
    signal CanTx_Node1, CanTx_Node2 : std_logic;
begin
    Node1 : CanLite
    generic map (
        FIFO_DEPTH => 2,
        BAUD_RATE_PRESCALAR => 1,
        SYNCHRONIZATION_JUMP_WIDTH => 2,
        TIME_SEGMENT_1 => 8,
        TIME_SEGMENT_2 => 3,
        TRIPLE_SAMPLING => true
    )
    port map (
        AppClock => AppClock,
        Reset_n => Reset_n,
        RxFrame => RxFrame,
        RxStrobe => RxStrobe,
        RxFifoEmpty => open,
        TxFrame => TxFrame,
        TxStrobe => TxStrobe,
        TxFifoFull => open,
        TxAck => TxAck,
        Status => Node1Status,
        CanClock => CanClock,
        CanRx => CanRx,
        CanTx => CanTx_Node1
    );
    
    Node2 : CanLite
        generic map (
            FIFO_DEPTH => 2,
            BAUD_RATE_PRESCALAR => 1,
            SYNCHRONIZATION_JUMP_WIDTH => 2,
            TIME_SEGMENT_1 => 8,
            TIME_SEGMENT_2 => 3,
            TRIPLE_SAMPLING => true
        )
        port map (
            AppClock => AppClock,
            Reset_n => Reset_n,
            RxFrame => open,
            RxStrobe => '0',
            RxFifoEmpty => open,
            TxFrame => TxFrame,
            TxStrobe => '0',
            TxFifoFull => open,
            TxAck => open,
            Status => open,
            CanClock => CanClock,
            CanRx => CanRx,
            CanTx => CanTx_Node2
        );

    AppClock <= CanClock;
    CanRx <= CanTx_Node1 and CanTx_Node2;

    process
    begin
        CanClock <= '0';
        wait for 20.833ns;
        CanClock <= '1';
        wait for 20.833ns;
    end process;
    
    process
    begin
        Reset_n <= '0';
        TxStrobe <= '0';
        TxFrame <= (
            Id => (others => '0'),
            Rtr => '0',
            Dlc => (others => '0'),
            Data => (others => (others => '0'))
        );
        RxStrobe <= '0';
        wait for 1us;
        Reset_n <= '1';
        wait until CanBus."/="(Node1Status.State, CanBus.STATE_RESET);
        wait until falling_edge(AppClock);
        TxFrame <= (
            Id => b"11001011001",
            Rtr => '0',
            Dlc => (others => '0'),
            Data => (others => (others => '0'))
        );
        TxStrobe <= '1';
        wait until falling_edge(AppClock);
        TxStrobe <= '0';
        wait until falling_edge(AppClock);
        TxFrame <= (
            Id => b"00000000001",
            Rtr => '0',
            Dlc => b"1111",
            Data => (others => (others => '1'))
        );
        TxStrobe <= '1';
        wait until falling_edge(AppClock);
        TxStrobe <= '0';
        wait;
    end process;

end Behavioral;
