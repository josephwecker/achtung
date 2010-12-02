/*import utest.Assert;

class ErlishTest {
    var server :ErlangBridge;

    static public function main() {
        utils.Tracer.initialize();
        utils.Tester.run(new ErlishTest());
    }

    public function new();

    public function setup() {
        server = new ErlangBridge('localhost', 4114);
    }

    public function testJSON() {
        Assert.equals(1, 1, "Oops, those are different.\n.\n.\n.\n.");
    }

    public function testDataTypes() {
        server.quicktest();
    }
}*/

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
