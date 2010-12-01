package utils;

class Tracer {
    public static function initialize() {
        haxe.Log.trace = tracer_trace;
    }

    public function new();

    static function tracer_trace(v :Dynamic, ?inf :haxe.PosInfos) {
#if (flash9 || flash10)
        untyped __global__["trace"](inf.className+"#"+inf.methodName+"("+inf.lineNumber+"):",v);
#elseif flash
        flash.Lib.trace(inf.className+"#"+inf.methodName+"("+inf.lineNumber+"): "+v);
#end
    }
}
