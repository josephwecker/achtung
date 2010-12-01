import utest.Assert;

class ErlishTest {
    static public function main() {
        utils.Tracer.initialize();
        utils.Tester.run(new ErlishTest());
    }

    public function new();

    public function testJSON() {
        Assert.equals(1, 1, "Oops, those are different.\n.\n.\n.\n.");
    }
}
