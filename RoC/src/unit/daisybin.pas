unit daisybin;

{$mode objfpc}{$H+}

interface

const
  DAISY_MAX_DEVICES = 8;
  DAISY_HEADER_SIZE = 6;

type
  TDaisyDevice = 0..DAISY_MAX_DEVICES - 1;
  TDaisyDevices = set of TDaisyDevice;

type
  TDaisyFrag = record
    targets: TDaisyDevices;
    data: string;
    inputsize: integer;
  end;


type
  TReceiveProc = procedure(dev: TDaisyDevice; DataSent, DataRecv: string);

  TDaisy = class(Tobject)
    private
      frags: array[0 .. DAISY_MAX_DEVICES - 1] of TDaisyFrag;
      fragcount: integer;
      targ_used: TDaisyDevices;

      cur_packet: string;

      FReceivedPacketProc: TReceiveProc;
      nib_count: integer;
      act_num_daisy_devices: integer;
      daisy_data_in_size, daisy_data_out_size, daisy_broadcast_mode: integer;
      daisy_node_mask: integer;
      final_nib, first_nib_in, first_nib_out: integer;
      RawBuffer: string;
      num_nodes: integer;

      procedure InternalFlush;

    public
      procedure SendData(devs: TDaisyDevices; new_data: string; new_inputsize: integer); overload;
      procedure SendData(dev: TDaisyDevice; data: string; inputsize: integer); overload;
      function Flush: string;

      procedure SetRecvProc(NewReceiveProc: TReceiveProc);
      procedure ProcessRawData(RawData: String);

      constructor Create;
  end;

function DaisyDevCount(devs: TDaisyDevices): integer;

implementation

{ TDaisy }

function DaisyDevCount(devs: TDaisyDevices): integer;
var i: TDaisyDevice;
begin
  result := 0;
  for i := Low(TDaisyDevice) to High(TDaisyDevice) do
    if i in devs then Inc(result);
end;

function DaisyDevMask(devs: TDaisyDevices): integer;
var i: TDaisyDevice;
begin
  result := 0;
  for i := Low(TDaisyDevice) to High(TDaisyDevice) do
    if i in devs then result := result or (1 shl ord(i));
end;

function TDaisy.Flush: string;
begin
  InternalFlush;
  result := cur_packet;
  cur_packet := '';
end;

procedure TDaisy.SendData(devs: TDaisyDevices; new_data: string; new_inputsize: integer);
var count: integer;
begin
  count := DaisyDevCount(devs);
  if count = 0 then exit;

  // treat the broadcast case first
  if count > 1 then begin
    // we can't merge broadcasts anyway, so just flush whatever is in there
    InternalFlush;
    // add just this packet
    with frags[0] do begin
      targets := devs;
      data := new_data;
      inputsize := new_inputsize;
    end;
    fragcount := 1;
    targ_used := devs;
    // and flush it immediately to simplify the merging of regular packets
    InternalFlush;
  end else begin
    // check if the current list needs to be flushed
    if fragcount > 0 then begin
      // if the sizes differ or this device has already been adressed
      if (length(new_data) <> length(frags[0].data)) or
         (new_inputsize <> frags[0].inputsize) or
         (devs <= targ_used) then begin
        InternalFlush;
      end;
    end;
    // add the packet to the list
    with frags[fragcount] do begin
      targets := devs;
      data := new_data;
      inputsize := new_inputsize;
    end;
    Inc(fragcount);
    // mark the device as used
    targ_used := targ_used + devs;
  end;
end;


// General Packet format
// '['D']['C'][I45O45Bn][IO][ma]{dataIn1}{dataIn2}...{}{dataOut1}{dataOut2}...{}'
// 'D', 'C' - start packet sync
// I45 - bits 4:5 of number of input bytes (0-3F)
// O45 - bits 4:5 of number of output bytes (0-3F)
// B - Broadcast: 1:broadcast, 0:not
// n - Node number (0-7)
// I - bits 0:3 of number of input bytes (0-3F)
// O - bits 0:3 of number of output bytes (0-3F)
// ma - Node Mask
// dataInX - I data in bytes
// dataOutX - O data out bytes

// Node Modes
// B (ma=FF)- [Ib][01][02]...[O8]
// B (ma=0) - [Ib]
// A        - [I1][I2]...[I8][01][02]...[O8]

procedure TDaisy.InternalFlush;
var broadcount, osize, isize, i, j: integer;
    pck: string;
    tmp: TDaisyFrag;
begin
  // if there is nothing to do, just return
  if fragcount <= 0 then exit;

  broadcount := DaisyDevCount(frags[0].targets);
  osize := frags[0].inputsize;
  isize := length(frags[0].data);

  pck := 'DC' + chr(((isize shl 2) and $C0) or (osize and $30)) +
         chr(((isize shl 4) and $F0) or (osize and $0F)) +
         chr(DaisyDevMask(targ_used));

  // treat broadcast packets differently
  if broadcount > 1 then begin
    // mark the broadcast flag
    pck[3] := chr(ord(pck[3]) or $08);
    // just place the data there
    pck := pck + frags[0].data;
  end else begin
    // sort the frags
    for i := 0 to fragcount - 2 do begin
      for j := 0 to fragcount - 2 - i do begin
        if DaisyDevMask(frags[j].targets) > DaisyDevMask(frags[j + 1].targets) then begin
          tmp := frags[j];
          frags[j] := frags[j + 1];
          frags[j + 1] := tmp;
        end;
      end;
    end;

    // build the packet
    for i := 0 to fragcount - 1 do
      pck := pck + frags[i].data;
  end;

  // pad with expected input data
  pck := pck + StringOfChar(chr(0), osize * DaisyDevCount(targ_used));

  // add to the current running packet
  cur_packet := cur_packet + pck;

  // clear the low-level data
  fragcount := 0;
  targ_used := [];
end;

procedure TDaisy.SendData(dev: TDaisyDevice; data: string; inputsize: integer);
begin
  SendData([dev], data, inputsize);
end;


constructor TDaisy.Create;
begin
  inherited;
  fragcount := 0;
  targ_used := [];
  cur_packet := '';
  num_nodes := 0;

  FReceivedPacketProc := nil;
  nib_count:=0;
  RawBuffer:='';
end;

procedure TDaisy.ProcessRawData(RawData: String);
var i,cnt: integer;
    c, b, j, offset: integer;
begin

  //if @FReceivedPacketProc = nil then exit;
  if not assigned(FReceivedPacketProc) then exit;

  cnt:=length(RawData);
  for i:=1 to cnt do begin
    c := ord(RawData[i]);
    RawBuffer := RawBuffer + chr(c);
    inc(nib_count);
    case nib_count of
      1: if c <> ord('D') then begin
           nib_count := 0;
           RawBuffer := '';
         end;

      2: if c <> ord('C') then begin
           if c = ord('D') then begin
             nib_count := 1;
             RawBuffer := 'D';
           end else begin
             nib_count := 0;
             RawBuffer := '';
           end;
         end;

      // This byte is the driver identification plus some extra bits, we can use it to know the number of devices
      3: begin
        act_num_daisy_devices := c and $07;
        daisy_data_in_size := (c shr 2) and $30;
        daisy_data_out_size := c and $30;
        daisy_broadcast_mode := c and $08;
      end;

      4: begin  // The rest of the in/out sizes
        daisy_data_in_size := daisy_data_in_size or ((c shr 4) and $0F);
	daisy_data_out_size := daisy_data_out_size or (c and $0F);
      end;

      5: begin // The node mask
        daisy_node_mask := c;

        // Count total number of nodes in the mask
        num_nodes := 0;
        for j := 0 to DAISY_MAX_DEVICES-1 do begin
          b := 1 shl j;
          if (daisy_node_mask and b) <> 0 then inc(num_nodes);
	end;

        // calc the total packet size
        if (daisy_broadcast_mode<>0) then begin // Broadcast
          final_nib := DAISY_HEADER_SIZE + (daisy_data_in_size + num_nodes * daisy_data_out_size) - 1;
        end else begin // All
          final_nib := DAISY_HEADER_SIZE + (num_nodes * (daisy_data_in_size + daisy_data_out_size)) - 1;
        end;
      end;

    else
      if (nib_count = final_nib) then begin  // The packet ends HERE!
        offset := 0;
        for j := 0 to DAISY_MAX_DEVICES-1 do begin
          b := 1 shl j;
          if (daisy_node_mask and b) <> 0 then begin
            if daisy_broadcast_mode<>0 then begin
              first_nib_in := DAISY_HEADER_SIZE;
              first_nib_out := DAISY_HEADER_SIZE + (daisy_data_in_size + offset * daisy_data_out_size);
            end else begin // All
              first_nib_in := DAISY_HEADER_SIZE + (offset * daisy_data_in_size);
              first_nib_out := DAISY_HEADER_SIZE + (num_nodes * daisy_data_in_size + offset * daisy_data_out_size);
            end;

            // call the ReceivedPacketProc for each device targeted in the message
            FReceivedPacketProc(TDaisyDevice(j), copy(RawBuffer,first_nib_in, daisy_data_in_size), copy(RawBuffer,first_nib_out, daisy_data_out_size));

            inc(offset);
	  end;
	end;

        // Reset nib_count to expect another packet
        nib_count := 0;
        RawBuffer:='';
      end;
    end;

    //RawBuffer := RawBuffer + chr(c);
  end;
end;

procedure TDaisy.SetRecvProc(NewReceiveProc: TReceiveProc);
begin
  FReceivedPacketProc := NewReceiveProc;
end;

end.


