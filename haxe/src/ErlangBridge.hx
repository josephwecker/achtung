import flash.events.Event;

class ErlangBridge {
    var socket             :flash.net.Socket;
    var host               :String;
    var port               :UInt;
    var inbound_msg_queue  :flash.Vector<Dynamic>;
    var outbound_msg_queue :flash.Vector<Dynamic>;

    var state              :ErlangBridgeState;
    var read_len           :UInt;

    public function new(host :String, port :Int) {  // TODO: External Event-handlers
        socket = new flash.net.Socket();
        socket.endian = flash.utils.Endian.BIG_ENDIAN;

        socket.addEventListener(flash.events.Event.CLOSE,                       on_close);
        socket.addEventListener(flash.events.Event.CONNECT,                     on_connect);
        socket.addEventListener(flash.events.IOErrorEvent.IO_ERROR,             on_io_error);
        socket.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR, on_security_error);
        socket.addEventListener(flash.events.ProgressEvent.SOCKET_DATA,         on_socket_data);

        state = WAITING_FOR_NEW;
        inbound_msg_queue =  new flash.Vector<Dynamic>();
        outbound_msg_queue = new flash.Vector<Dynamic>();
        trace("Connecting");
        socket.connect(host, port);
    }

    public function quicktest() {
        trace("Sending test request");
        socket.writeUTFBytes("TESTDAT\r\n");
        socket.flush();
    }

    function on_close(_) {
        trace("Socket closed.");
    }

    function on_connect(_) {
        trace("Socket connected.");
        quicktest();
    }

    function on_io_error(e :flash.events.IOErrorEvent) {
        trace("IO Error: "+ e);
    }

    function on_security_error(e :flash.events.SecurityErrorEvent) {
        trace("Security Error: "+ e);
    }

    function on_socket_data(_) {
        trace("Data received.");
        attempt_decode_message();
    }

    function attempt_decode_message() {
        if(state == WAITING_FOR_NEW) {
            if(socket.bytesAvailable < 6) return;
            var ver = socket.readUnsignedByte();
            var type= socket.readUnsignedByte();
            read_len = socket.readUnsignedInt();
            state = WAITING_FOR_BODY;
        }

        if(state == WAITING_FOR_BODY) {
            if(socket.bytesAvailable >= read_len) {
                var raw_dat = new flash.utils.ByteArray();
                socket.readBytes(raw_dat, 0, read_len);
                var msg :Dynamic = ETF.decode(raw_dat);
                trace('[' + Type.typeof(msg)+ '] ' +msg);
                inbound_msg_queue.push(msg);
                var re_enc = ETF.encode(msg);
                var str = '';
                for(i in 0...re_enc.length) str +=
                    StringTools.hex(re_enc.readUnsignedByte(), 1) + ',';
                trace("Rencoded: " + str);
                state = WAITING_FOR_NEW;
                attempt_decode_message();
            }
        }
    }

}

enum ErlangBridgeState {
    WAITING_FOR_NEW;
    WAITING_FOR_BODY;
}
