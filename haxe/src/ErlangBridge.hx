
import de.polygonal.ds.ArrayedQueue;
import flash.events.Event;

class ErlangBridge {
    var socket             :flash.net.Socket;
    var host               :String;
    var port               :UInt;
    var dat_buffer         :flash.utils.ByteArray;
    var inbound_msg_queue  :ArrayedQueue<Dynamic>;
    var outbound_msg_queue :ArrayedQueue<Dynamic>;

    public function new(host :String, port :Int) {  // TODO: External Event-handlers
        socket = new flash.net.Socket();
        socket.endian = flash.utils.Endian.BIG_ENDIAN;

        socket.addEventListener(flash.events.Event.CLOSE,                       on_close);
        socket.addEventListener(flash.events.Event.CONNECT,                     on_connect);
        socket.addEventListener(flash.events.IOErrorEvent.IO_ERROR,             on_io_error);
        socket.addEventListener(flash.events.SecurityErrorEvent.SECURITY_ERROR, on_security_error);
        socket.addEventListener(flash.events.ProgressEvent.SOCKET_DATA,         on_socket_data);

        socket.connect(host, port);
    }

    function on_close(_) {

    }

    function on_connect(_) {
        // TODO: Initialize queues & buffer
    }

    function on_io_error(e :flash.events.IOErrorEvent) {

    }

    function on_security_error(e :flash.events.SecurityErrorEvent) {

    }

    function on_socket_data(_) {

    }

}
