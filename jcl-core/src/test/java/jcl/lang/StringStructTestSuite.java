package jcl.lang;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Test suite for {@link StringStruct} tests.
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({StringStructStringTest.class,
                     StringStructStringEqualityTest.class,
                     StringStructVectorTest.class,
                     StringStructArrayTest.class,
                     StringStructAdjustArrayTest.class,
                     StringStructSequenceTest.class})
public class StringStructTestSuite {
}
