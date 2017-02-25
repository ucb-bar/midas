module replay;
  reg clock = 1'b0;
  reg reset = 1'b1;
  reg exit = 1'b0;
  reg [64:0] cycles = 0;

`ifdef VCS 
  always #(`CLOCK_PERIOD / 2.0) clock = ~clock;

  reg [1023:0] vcdplusfile = 0;
`endif

/* include compiler generated testbench fragment */
`include `VFRAG

  initial begin
`ifdef VCS
    if ($value$plusargs("waveform=%s", vcdplusfile))
    begin
      $vcdplusfile(vcdplusfile);
      $vcdpluson(0);
      $vcdplusmemon(0);
    end
`endif
    $init_sigs(`TOP_TYPE);
  end

  always @(posedge clock) begin
    if (!reset) cycles <= cycles + 1;
  end

  always @(negedge clock) begin
    $tick(exit);
    if (exit) begin
`ifdef VCS
      $vcdplusclose;
`endif
      $finish;
    end
  end

endmodule
