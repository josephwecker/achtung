package utils;

import utest.Assert;
import utest.Runner;
import utest.ui.Report;
import utest.ui.text.PrintReport;

class Tester {
    static public function run(Object :Dynamic, ?pattern :EReg) {
        var runner = new Runner();
        runner.addCase(Object, 'setup', 'teardown', 'test', pattern);
        Report.create(runner);
        runner.run();
    }
}

