module hello (input wire clk_in
, input wire rst
, input wire [5:0] data_in
, output reg [3:0] data_out
);

localparam something = 5'b0_11_00;
integer counter;
reg [3:0] memory [90:0][5:0]; 
always @(posedge clk_in or negedge rst) begin
    if(rst == 0) begin
        data_out <= 0;
        counter <= 0;
    end else begin
        if(counter == 0) begin
            data_out[3:0] <= data_in[3:0] + data_in[5:3];
            counter <= something;
        end else begin
            counter <= counter - 1;
        end
    end
end
endmodule