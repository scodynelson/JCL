package jcl.lang;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Test suite for {@link CharacterStruct} tests.
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({CharacterStructCharacterTest.class,
                     CharacterStructCharacterEqualityTest.class,
                     CharacterStructObjectTest.class})
public class CharacterStructTestSuite {
}