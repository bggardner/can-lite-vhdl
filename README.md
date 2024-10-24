# can-lite-vhdl
A lightweight Controller Area Network (CAN) controller in VHDL

This code provides a simple FIFO interface to a CAN controller. It was developed for use in conjunction with a CANopen controller (see [canopen-vhdl](https://github.com/bggardner/canopen-vhdl)) on an FPGA with (or using) limited resources, hence the minimal interface and selected status flags.  While it is typical to use this module with an asynchronous (dual clock) FIFO, it is not required.  However, it may be difficult to achieve the appropriate CAN timing with some application clocks, and FIFOs help prevent data loss (overflow) if status bits are not continuously checked.  The values for the `generics` are typical of many CAN controllers, with the following relationships:

* Baud Rate, in bps = (`Clock` frequency, in Hz) / 2 / BAUD_RATE_PRESCALAR / (TIME_SEGMENT_1 + TIME_SEGMENT_2 + 1)
* Sample Point, % of bit time = (TIME_SEGMENT_1 + 1) / (TIME_SEGMENT_1 + TIME_SEGMENT_2 + 1)

Example:
* 24MHz `Clock`, `BAUD_RATE_PRESCALAR` = 1, `TIME_SEGMENT_1` = 8, `TIME_SEGMENT_2` = 3
* Baud Rate: 24000000 / 2 / 1 / (8 + 3 + 1) = 1000000 = 1 Mbps
* Sample Point: (8 + 1) / (8 + 3 + 1) = 75%

`src/CanBus_pkg.vhd` defines some data types to abstract bit ordering and procedures for use in testbenches.

`src/CanLite.vhd` contains the top-level VHDL module (CanLite), as well as the submodules it depends on.

`test/CanLite_tb.vhd` is a simple testbench that uses Xilinx asynchronous FIFOs and connects two nodes on a virtual CAN bus.

Please excuse the mixed coding styles, as this code was adapted multiple times (see `src/CanLite.vhd`).  The top-level file uses the coding style of GRC-PLC-CDD, which is an internal NASA document.
