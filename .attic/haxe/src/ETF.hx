import flash.utils.ByteArray;
using StringTools;

/**
 * Main Erlang External Term Format encoder / decoder.  Implemented as a
 * singleton so that it's basically static access but fast when recursing the
 * structures.
 *
 *  Type       | Erlang            | HaXe             | Status
 * ------------+-------------------+------------------+-------------
 *  atom       | hello :atom      <|> ':hello:':String | done
 *  null       | [] :list         <|> null :Null       | done
 *  bool       | true/false :atom <|> true/false :Bool | done
 *  smallint   | 123 :smallint    <|> 123 :Int         | done
 *  int        | 1234 :int        <|> 1234 :Int        | done
 *  int        | -1234 :int       <|> -1234 :Int       | done
 *  uint       | 0xFFFFFFFF :int  <|> 0xFFFFFFFF :UInt | nyi
 *  bignum     | 999999999999 :int |                   | no support
 *  double     | 13.123 :float    <|> 13.123 :Float    | done
 *
 * (Structures)
 *  binary     | <<1,2,3>> :bin   <|> :ByteArray       | done
 *  bitstring  | <<1:3>> :bitstr  <|> :ErlBits         | partial
 *  utf-string | "hello" :list    <|> "hello" :String  | done
 *  tuple      | {1, 2, 3} :tuple <|> [1,2,3] :Vector^1| done
 *             |                  <|  [1,2,3] :Array   | nyi
 *  list       | [1,2,3] :list    <|> [1,2,3] :Vector^2| done
 *             |                  <|  :List            | nyi
 *             |                  <|  :FastList        | nyi
 *
 * (Complex Structures)
 *  hash       | [{key,val},...]  <|> :Hash            | nyi
 *  object     | {struct, [{}...]}<|> :Klass (cast)^3  | nyi
 *
 * (Erlang special structures)
 *  reference  | #Ref<0.0.0.1>    <|> :ErlRef          | done
 *  port       | :port-identifier <|> :ErlPort         | done
 *  lambda     | #Fun<...>        <|> :ErlFun          | partial
 *             |                   |  :(function)      | no support
 *  pid        | #PID<0.0.0.1>    <|> :ErlPID          | done
 *  export     | math:pow/2       <|> :ErlExport       | partial
 *
 *  Notes:
 *  ^1: Tuple if the vector has 'fixed' set to true or if just a normal array.
 *  ^2: List only if the vector is not 'fixed.'
 *  ^3: Metadat can be stored as some of the fields, esp. class name, which
 *      will otherwise be anonymous
 *
 */
class ETF {
    static var _instance :ETF;

    static function instance() :ETF {
        if (_instance == null) _instance = new ETF();
        return _instance;
    }

    private function new();

    static public function decode(dat :ByteArray) :Dynamic {
        var res = instance()._decode(dat);
        dat.clear();
        return res;
    }

    function _decode(dat :ByteArray) :Dynamic {
        var identifier_byte = dat.readUnsignedByte();
        switch(identifier_byte) {
            // Term
            case 131: return(_decode(dat));
            // Compressed Term
            case 80:  var len = dat.readUnsignedInt();
                      var newdat = new ByteArray();
                      dat.readBytes(newdat);
                      newdat.uncompress();
                      if(newdat.length != len) return null;
                      else return _decode(newdat);
            // Smallint (unsigned byte)
            case 97:  return(dat.readUnsignedByte());
            // Integer
            case 98:  return(dat.readInt());
            // Float (old)
            case 99:  var float_string = dat.readUTFBytes(31);
                      return Std.parseFloat(float_string);
            // Atom (old)
            case 100: var len = dat.readUnsignedShort();
                      var str = dat.readUTFBytes(len);
                      if(str == 'true') return true;
                      if(str == 'false') return false;
                      return ':' + str + ':';
            // Reference (old)
            case 101: var id = new Array<UInt>();
                      var node = _decode(dat);
                      id.push(dat.readUnsignedInt());
                      return new ErlRef(node, id, dat.readUnsignedByte());
            // Port
            case 102: return new ErlPort(
                              _decode(dat),             // Node
                              dat.readUnsignedInt(),   // ID
                              dat.readUnsignedByte()); // Creation
            // PID
            case 103: return new ErlPID(_decode(dat),   // Node
                              dat.readUnsignedInt(),   // ID
                              dat.readUnsignedInt(),   // Serial
                              dat.readUnsignedByte()); // Creation
            // Small Tuple
            case 104: var arity = cast(dat.readUnsignedByte(), Int);
                      var tuple = new ErlTuple(arity, true);
                      for(i in 0...arity) tuple[i] = _decode(dat);
                      return tuple;
            // Large Tuple
            case 105: var arity = cast(dat.readUnsignedInt(), Int);
                      var tuple = new ErlTuple(arity, true);
                      for(i in 0...arity) tuple[i] = _decode(dat);
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
                      for(i in 0...len) list.push(_decode(dat));
                      var tail = _decode(dat);
                      if(tail != null) list.push(tail);
                      return list;
            // Binary
            case 109: var len = dat.readUnsignedInt();
                      var bin = new ByteArray();
                      dat.readBytes(bin, 0, len);
                      return bin;
            // Reference (new)
            case 114: var id_len =   cast(dat.readUnsignedShort(), Int);
                      var node =     _decode(dat);
                      var creation = dat.readUnsignedByte();
                      var id = new Array<UInt>();
                      for(i in 0...id_len) id.push(dat.readUnsignedInt());
                      return new ErlRef(node, id, creation);
            // Small Atom
            case 115: var len = dat.readUnsignedByte();
                      return ':' + dat.readUTFBytes(len) + ':';
            // Function
            // TODO: The num_free cast here for haXe may break big lists!
            case 112: dat.readUnsignedInt(); // Size, which we don't need
                      var arity = dat.readUnsignedByte();
                      var uniq = new ByteArray();
                      dat.readBytes(uniq, 0, 16);
                      var index = dat.readUnsignedInt();
                      var num_free = cast(dat.readUnsignedInt(), Int);
                      var module = _decode(dat);
                      var old_index = _decode(dat);
                      var old_uniq = _decode(dat);
                      var pid = _decode(dat);
                      var free_vars = new Array<Dynamic>();
                      for(i in 0...num_free) free_vars.push(_decode(dat));
                      return new ErlFun(arity, uniq, index, module, old_index,
                              old_uniq, pid, free_vars);
            // Export
            case 113: return new ErlExport(
                              _decode(dat),   // Module
                              _decode(dat),   // Fun (atom)
                              _decode(dat));  // Arity
            // Bit Binary
            case 77:  var len = dat.readUnsignedInt();
                      var last_bits = dat.readUnsignedByte();
                      var inner_data = new ByteArray();
                      dat.readBytes(inner_data, 0, len);
                      return new ErlBits(inner_data, last_bits);
            // Float (new)
            case 70:  return dat.readDouble();

            // Dunno...
            default:
                      trace("Unknown data!!- what to do about [" +
                              identifier_byte + "]");
                      return null;
        }
    }

    static public function encode(obj :Dynamic, ?allow_compress=true) :ByteArray {
        var enc = new ByteArray();
        instance()._encode(obj, enc);
        enc.position = 0;
        var len = enc.length;
        var enc_res = new ByteArray();
        if(allow_compress && len > 500) {
            var compressed = new ByteArray();
            compressed.writeBytes(enc);
            compressed.compress();
            if(compressed.length < cast((len - 1),UInt)) {
                enc_res.writeByte(131);
                enc_res.writeByte(80);
                enc_res.writeUnsignedInt(len);
                enc_res.writeBytes(compressed);
            } else {
                enc_res.writeByte(131);
                enc.position = 0;
                enc_res.writeBytes(enc);
            }
            compressed.clear();
        } else {
            enc_res.writeByte(131);
            enc_res.writeBytes(enc);
        }
        enc.clear();
        enc_res.position = 0;
        return enc_res;
    }

    function _encode(obj :Dynamic, acc :ByteArray) :Void {
        switch(Type.typeof(obj)) {
            case TNull:
                acc.writeByte(106);
            case TInt:
                var i :Int = obj;
                if(i > 0 && i < 256) {
                    acc.writeByte(97);
                    acc.writeByte(i);
                } else {
                    acc.writeByte(98);
                    acc.writeInt(i);
                }
            case TFloat:
                acc.writeByte(70);
                acc.writeFloat(obj);
            case TBool:
                var b :Bool = obj;
                encode_atom(acc, (b ? 'true' : 'false'));
                encode_atom(acc, (obj ? 'true' : 'false'));
            case TClass(c):
                switch(Type.getClassName(c)) {
                    case 'String':
                        var s :String = obj;
                        if(s.startsWith(':') && s.endsWith(':')) {
                            encode_atom(acc, s);
                        } else {
                            acc.writeByte(107);
                            acc.writeUTF(s);
                        }
                    case '__AS3__.vec.Vector.<Object>':
                        if(obj.fixed) {  // Tuple
                            if(obj.length < 256) {
                                acc.writeByte(104);
                                acc.writeByte(obj.length);
                            } else {
                                acc.writeByte(105);
                                acc.writeUnsignedInt(obj.length);
                            }
                            for(i in 0...obj.length) _encode(obj[i], acc);
                        } else {  // List
                            acc.writeByte(108);
                            acc.writeUnsignedInt(obj.length);
                            for(i in 0...obj.length) _encode(obj[i], acc);
                            acc.writeByte(106);
                        }
                    case 'flash.utils.ByteArray': // Binary
                        obj.position = 0;
                        acc.writeByte(109);
                        acc.writeUnsignedInt(obj.length);
                        acc.writeBytes(obj);
                    case 'ErlRef':
                        acc.writeByte(114);
                        acc.writeShort(obj.id.length);
                        encode_atom(acc, obj.node);
                        acc.writeByte(obj.creation);
                        for(id in cast(obj.id, Array<Dynamic>)) acc.writeUnsignedInt(id);
                    case 'ErlPort':
                        acc.writeByte(102);
                        encode_atom(acc, obj.node);
                        acc.writeUnsignedInt(obj.id);
                        acc.writeByte(obj.creation);
                    case 'ErlPID':
                        acc.writeByte(103);
                        encode_atom(acc, obj.node);
                        acc.writeUnsignedInt(obj.id);
                        acc.writeUnsignedInt(obj.serial);
                        acc.writeByte(obj.creation);
                    case 'ErlFun':
                    case 'ErlExport':
                    case 'ErlBits':

                    default:
                        trace("Class was: >" + Type.getClassName(c) + "<");
                }
            default:
        }
    }

    inline function encode_atom(acc :ByteArray, s :String) {
        if(s.startsWith(':') && s.endsWith(':')) s=s.substr(1,s.length-2);
        if(s.length < 256) {
            acc.writeByte(115);
            acc.writeByte(s.length);
            acc.writeUTFBytes(s);
        } else {
            acc.writeByte(100);
            acc.writeUTF(s);
        }
    }
}

typedef ErlTuple = flash.Vector<Dynamic>;
typedef ErlList  = flash.Vector<Dynamic>;

class ErlRef {
    public var node      :String;
    public var id        :Array<UInt>;
    public var creation  :UInt;

    public function new(node, id, creation) {
        this.node = node;
        this.id = id;
        this.creation = creation;
    }
    public function toString() {return 'ErlRef('+node+'|'+id+'|'+creation+')';}
}

class ErlPort {
    public var node      :String;
    public var id        :UInt;
    public var creation  :UInt;

    public function new(node, id, creation) {
        this.node = node;
        this.id = id;
        this.creation = creation;
    }
    public function toString() {return 'ErlPort('+node+'|'+id+'|'+creation+')';}
}

class ErlPID {
    public var node      :String;
    public var id        :UInt;
    public var serial    :UInt;
    public var creation  :UInt;

    public function new(node, id, serial, creation) {
        this.node = node;
        this.id = id;
        this.serial = serial;
        this.creation = creation;
    }

    public function toString() {
        return 'ErlPID('+node+'|'+id+'|'+serial+'|'+creation+')';
    }
}

class ErlFun {
    public var arity     :UInt;
    public var uniq      :ByteArray;
    public var index     :UInt;
    public var module    :String;
    public var old_index :UInt;
    public var old_uniq  :UInt;
    public var pid       :ErlPID;
    public var free_vars :Array<Dynamic>;

    public function new(a, u, i, m, oi, ou, p, f) {
        arity = a; uniq = u; index = i; module = m;
        old_index = oi; old_uniq = ou; pid = p; free_vars = f;
    }

    public function toString() {
        return 'ErlFun('+arity+'|'+index+'|'+module+'|'+free_vars+')';
    }
}


class ErlExport {
    public var module    :String;
    public var fun       :String;
    public var arity     :UInt;
    public function new(m,f,a) {
        module = m; fun = f; arity = a;
    }
    public function toString() {
        return 'ErlExport('+module+'.'+fun+'/'+arity+')';
    }
}


class ErlBits {
    public var data      :ByteArray;
    public var last_bits :UInt;
    public function new(d, l) {
        data = d; last_bits = l;
    }
    public function toString() {
        return 'ErlBits('+data.length+'|'+last_bits+')';
    }
}
