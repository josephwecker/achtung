import flash.utils.ByteArray;

class ETF {
    static public function decode(dat :ByteArray) :Dynamic {
        var identifier_byte = dat.readByte();
        switch(identifier_byte) {
            // Term
            case 131: return(decode(dat));
            // Compressed Term
            case 80:  var len = dat.readUnsignedInt();
                      dat.uncompress();
                      if(dat.length != len) return null;
                      else return decode(dat);
            // Smallint (unsigned byte)
            case 97:  return(dat.readUnsignedByte());
            // Integer
            case 98:  return(dat.readInt());
            // Float (old)
            case 99:  var float_string = dat.readUTFBytes(31);
                      return Std.parseFloat(float_string);
            // Atom (old)
            case 100: var len = dat.readUnsignedShort();
                      return {atom: dat.readUTFBytes(len)};
            // Reference (old)
            case 101: var id = new Array<UInt>();
                      var node = decode(dat);
                      id.push(dat.readUnsignedInt());
                      var ref :ErlRef = {
                          node:     node,
                          id:       id,
                          creation: dat.readUnsignedByte()};
                      return ref;
            // Port
            case 102: var port :ErlPort = {
                          node:     decode(dat),
                          id:       dat.readUnsignedInt(),
                          creation: dat.readUnsignedByte()};
                      return port;
            // PID
            case 103: var pid :ErlPID = {
                          node:     decode(dat),
                          id:       dat.readUnsignedInt(),
                          serial:   dat.readUnsignedInt(),
                          creation: dat.readUnsignedByte()};
                      return pid;
            // Small Tuple
            case 104: var arity = cast(dat.readUnsignedByte(), Int);
                      var tuple = new ErlTuple(arity, true);
                      for(i in 0...arity) tuple[i] = decode(dat);
                      return tuple;
            // Large Tuple
            case 105: var arity = cast(dat.readUnsignedInt(), Int);
                      var tuple = new ErlTuple(arity, true);
                      for(i in 0...arity) tuple[i] = decode(dat);
                      return tuple;
            // Nil / Empty List
            case 106: return null;
            // String
            case 107: var len = dat.readUnsignedShort();
                      return dat.readUTFBytes(len);
            // List
            // TODO: The len cast here for haXe may break big lists!
            case 108: var len = cast(dat.readUnsignedInt(), Int);
                      var list = new ErlList();
                      for(i in 0...len) list.push(decode(dat));
                      var tail = decode(dat);
                      if(tail != null) list.push(tail);
                      return list;
            // Binary
            case 109: var len = dat.readUnsignedInt();
                      var bin = new ByteArray();
                      dat.readBytes(bin, 0, len);
                      return bin;
            // Reference (new)
            case 114: var id_len =   cast(dat.readUnsignedShort(), Int);
                      var node =     decode(dat);
                      var creation = dat.readUnsignedByte();
                      var id = new Array<UInt>();
                      for(i in 0...id_len) id.push(dat.readUnsignedInt());
                      var ref :ErlRef = {
                          node:     node,
                          creation: creation,
                          id:       id};
                      return ref;
            // Small Atom
            case 115: var len = dat.readUnsignedByte();
                      return {atom: dat.readUTFBytes(len)};
            // Function
            // TODO: The num_free cast here for haXe may break big lists!
            case 112: dat.readUnsignedInt(); // Size, which we don't need
                      var arity = dat.readUnsignedByte();
                      var uniq = new ByteArray();
                      dat.readBytes(uniq, 0, 16);
                      var index = dat.readUnsignedInt();
                      var num_free = cast(dat.readUnsignedInt(), Int);
                      var module = decode(dat);
                      var old_index = decode(dat);
                      var old_uniq = decode(dat);
                      var pid = decode(dat);
                      var free_vars = new Array<Dynamic>();
                      for(i in 0...num_free) free_vars.push(decode(dat));
                      var fun :ErlFun = {
                          arity:     arity,
                          uniq:      uniq,
                          index:     index,
                          module:    module,
                          old_index: old_index,
                          old_uniq:  old_uniq,
                          pid:       pid,
                          free_vars: free_vars};
                      return fun;
            // Export
            case 113: var export :ErlExport = {
                          module: decode(dat),
                          fun:    decode(dat),
                          arity:  decode(dat)};
                      return export;
            // Bit Binary
            case 77:  var len = dat.readUnsignedInt();
                      var last_bits = dat.readUnsignedByte();
                      var data = new ByteArray();
                      dat.readBytes(data, 0, len);
                      var bits :ErlBits = {
                          data: dat,
                          last_bits: last_bits};
                      return bits;
            // Float (new)
            case 70:  return dat.readDouble();

            // Dunno...
            default:
                      trace("Unknown data!!- what to do about [" +
                              identifier_byte);
                      return null;
        }
    }

    static public function encode(obj :Dynamic) :ByteArray {
        return null;
    }
}

//class ErlTuple extends flash.Vector<Dynamic> {}
//class ErlList  extends flash.Vector<Dynamic> {}

typedef ErlTuple = flash.Vector<Dynamic>;
typedef ErlList  = flash.Vector<Dynamic>;

typedef ErlAtom = {
    var atom      :String;}

typedef ErlRef = {
    var node      :ErlAtom;
    var id        :Array<UInt>;
    var creation  :UInt;}

typedef ErlPort = {
    var node      :ErlAtom;
    var id        :UInt;
    var creation  :UInt;}

typedef ErlPID = {
    var node      :ErlAtom;
    var id        :UInt;
    var serial    :UInt;
    var creation  :UInt;}

typedef ErlFun = {
    var arity     :UInt;
    var uniq      :ByteArray;
    var index     :UInt;
    var module    :ErlAtom;
    var old_index :UInt;
    var old_uniq  :UInt;
    var pid       :ErlPID;
    var free_vars :Array<Dynamic>;}

typedef ErlExport = {
    var module    :ErlAtom;
    var fun       :ErlAtom;
    var arity     :UInt;}

typedef ErlBits = {
    var data      :ByteArray;
    var last_bits :UInt;}
