library ieee;
use ieee.std_logic_1164.all;
use work.CanBus;

entity CanLite_tb is
end CanLite_tb;

architecture Behavioral of CanLite_tb is  
    component CanLite is
        generic (
            BAUD_RATE_PRESCALAR         : positive;
            SYNCHRONIZATION_JUMP_WIDTH  : positive;
            TIME_SEGMENT_1              : positive;
            TIME_SEGMENT_2              : positive;
            TRIPLE_SAMPLING             : boolean
        );
        port (
            Clock               : in  std_logic; --! Base clock for CAN timing (24MHz recommended)
            Reset_n             : in  std_logic; --! Active-low reset
            
            RxFrame             : out CanBus.Frame; --! To RX FIFO
            RxFifoWriteEnable   : out std_logic; --! To RX FIFO
            RxFifoFull          : in  std_logic; --! From RX FIFO
                    
            TxFrame             : in  CanBus.Frame; --! From TX FIFO
            TxFifoReadEnable    : out std_logic; --! To TX FIFO
            TxFifoEmpty         : in std_logic; --! From TX FIFO
            TxAck               : out std_logic; --! High pulse when a message was successfully transmitted
            
            Status      : out CanBus.Status; --! See CanBus_pkg.vhdl
            
            CanRx       : in  std_logic; --! RX input from CAN transceiver
            CanTx       : out std_logic --! TX output to CAN transceiver
        );
    end component CanLite;
    
    --! Xilinx FIFO Generator, Independent Clocks
    component VendorFifo is
        port (
            rst : IN STD_LOGIC;
            wr_clk : IN STD_LOGIC;
            rd_clk : IN STD_LOGIC;
            din : IN STD_LOGIC_VECTOR(98 DOWNTO 0);
            wr_en : IN STD_LOGIC;
            rd_en : IN STD_LOGIC;
            dout : OUT STD_LOGIC_VECTOR(98 DOWNTO 0);
            full : OUT STD_LOGIC;
            empty : OUT STD_LOGIC;
            wr_rst_busy : OUT STD_LOGIC;
            rd_rst_busy : OUT STD_LOGIC
        );
    end component VendorFifo;
    
    signal AppClock, CanClock   : std_logic;
    signal Reset, Reset_n       : std_logic;
    signal RxFrame, TxFrame_i, TxFrame_o    : CanBus.Frame;
    signal RxFrameSlv_i, TxFrameSlv_i, TxFrameSlv_o : std_logic_vector(98 downto 0);
    signal RxFifoWriteEnable, TxFifoReadEnable, TxFifoWriteEnable   : std_logic;
    signal RxFifoFull, TxFifoEmpty  : std_logic;
    signal TxFifoReadBusy, TxFifoWriteBusy  : std_logic;
    signal TxAck                : std_logic;
    signal Node1Status          : CanBus.Status;
    signal CanRx                : std_logic;
    signal CanTx_Node1, CanTx_Node2, CanTx_Stimulus : std_logic;
begin
    Node1 : CanLite
    generic map (
        BAUD_RATE_PRESCALAR => 1,
        SYNCHRONIZATION_JUMP_WIDTH => 2,
        TIME_SEGMENT_1 => 8,
        TIME_SEGMENT_2 => 3,
        TRIPLE_SAMPLING => true
    )
    port map (
        Clock => CanClock,
        Reset_n => Reset_n,
        RxFrame => open,
        RxFifoWriteEnable => open,
        RxFifoFull => '0',
        TxFrame => TxFrame_o,
        TxFifoReadEnable => TxFifoReadEnable,
        TxFifoEmpty => TxFifoEmpty,
        TxAck => TxAck,
        Status => Node1Status,
        CanRx => CanRx,
        CanTx => CanTx_Node1
    );
    
    Node2 : CanLite
        generic map (
            BAUD_RATE_PRESCALAR => 1,
            SYNCHRONIZATION_JUMP_WIDTH => 2,
            TIME_SEGMENT_1 => 8,
            TIME_SEGMENT_2 => 3,
            TRIPLE_SAMPLING => true
        )
        port map (
            Clock => CanClock,
            Reset_n => Reset_n,
            RxFrame => RxFrame,
            RxFifoWriteEnable => RxFifoWriteEnable,
            RxFifoFull => RxFifoFull,
            TxFrame => TxFrame_o,
            TxFifoReadEnable => open,
            TxFifoEmpty => '1',
            TxAck => open,
            Status => open,
            CanRx => CanRx,
            CanTx => CanTx_Node2
        );
        
    Node2RxFifo : VendorFifo
        port map (
            rst => Reset,
            wr_clk => CanClock,
            rd_clk => AppClock,
            din => RxFrameSlv_i,
            wr_en => RxFifoWriteEnable,
            rd_en => '0',
            dout => open,
            full => RxFifoFull,
            empty => open,
            wr_rst_busy => open,
            rd_rst_busy => open
        );
        
    Node1TxFifo : VendorFifo
        port map (
            rst => Reset,
            wr_clk => CanClock,
            rd_clk => AppClock,
            din => TxFrameSlv_i,
            wr_en => TxFifoWriteEnable,
            rd_en => TxFifoReadEnable,
            dout => TxFrameSlv_o,
            full => open,
            empty => TxFifoEmpty,
            wr_rst_busy => TxFifoWriteBusy,
            rd_rst_busy => TxFifoReadBusy
        );
        
    AppClock <= CanClock; --! Application clock may be different than CAN base Clock
    Reset <= not Reset_n;
    CanRx <= CanTx_Node1 and CanTx_Node2 and CanTx_Stimulus; --! Virtual CAN bus
    RxFrameSlv_i <=
        RxFrame.Id &
        RxFrame.Rtr &
        RxFrame.Ide &
        RxFrame.Dlc &
        RxFrame.Data(0) &
        RxFrame.Data(1) &
        RxFrame.Data(2) &
        RxFrame.Data(3) &
        RxFrame.Data(4) &
        RxFrame.Data(5) &
        RxFrame.Data(6) &
        RxFrame.Data(7);
    TxFrameSlv_i <=
            TxFrame_i.Id &
            TxFrame_i.Rtr &
            TxFrame_i.Ide &
            TxFrame_i.Dlc &
            TxFrame_i.Data(0) &
            TxFrame_i.Data(1) &
            TxFrame_i.Data(2) &
            TxFrame_i.Data(3) &
            TxFrame_i.Data(4) &
            TxFrame_i.Data(5) &
            TxFrame_i.Data(6) &
            TxFrame_i.Data(7);
    TxFrame_o <= (
        Id => TxFrameSlv_o(98 downto 70),
        Rtr => TxFrameSlv_o(69),
        Ide => TxFrameSlv_o(68),
        Dlc => TxFrameSlv_o(67 downto 64),
        Data => (
            0 => TxFrameSlv_o(63 downto 56),
            1 => TxFrameSlv_o(55 downto 48),
            2 => TxFrameSlv_o(47 downto 40),
            3 => TxFrameSlv_o(39 downto 32),
            4 => TxFrameSlv_o(31 downto 24),
            5 => TxFrameSlv_o(23 downto 16),
            6 => TxFrameSlv_o(15 downto 8),
            7 => TxFrameSlv_o(7 downto 0)
        )
    );

    --! Generate 24MHz clock for CAN base clock
    process
    begin
        CanClock <= '0';
        wait for 20.833ns;
        CanClock <= '1';
        wait for 20.833ns;
    end process;
    
    --! Primary stimulus
    process
    begin
        --! Initialize
        Reset_n <= '0';
        CanTx_Stimulus <= '1';
        TxFifoWriteEnable <= '0';
        TxFrame_i <= (
            Id => (others => '0'),
            Rtr => '0',
            Ide => '0',
            Dlc => (others => '0'),
            Data => (others => (others => '0'))
        );
        wait for 1us;
        Reset_n <= '1';
        wait until CanBus."/="(Node1Status.State, CanBus.STATE_RESET);
        wait until TxFifoReadBusy = '0' and TxFifoWriteBusy = '0';
        
        --! Send 0 byte message
        wait until falling_edge(AppClock);
        TxFrame_i <= (
            Id => b"000000000000000000" & b"11001011001",
            Rtr => '0',
            Ide => '0',
            Dlc => (others => '0'),
            Data => (others => (others => '0'))
        );
        TxFifoWriteEnable <= '1';
        wait until falling_edge(AppClock);
        TxFifoWriteEnable <= '0';
        
        --! Send 8 byte message
        wait until falling_edge(AppClock);
        TxFrame_i <= (
            Id => b"110011001100110011" & b"00000000001",
            Rtr => '0',
            Ide => '1',
            Dlc => b"1111", --! b"1xxx" interpreted as 8 bytes
            Data => (others => (others => '1'))
        );
        TxFifoWriteEnable <= '1';
        wait until falling_edge(AppClock);
        TxFifoWriteEnable <= '0';
        
        --! Test bus off
        wait for 100us;
        CanTx_Stimulus <= '0';
        wait until CanBus."="(Node1Status.State, CanBus.STATE_BUS_OFF);
        CanTx_Stimulus <= '1';
        wait;
    end process;

end Behavioral;
