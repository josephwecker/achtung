class ErlishTest extends flash.display.Sprite {
    var server :ErlangBridge;
    static public function main() {
        utils.Tracer.initialize();
        var main = new ErlishTest();
    }

    public function new() {
        super();
        flash.Lib.current.addChild(this);
        server = new ErlangBridge('localhost', 4114);
        server.quicktest();
    }
}
