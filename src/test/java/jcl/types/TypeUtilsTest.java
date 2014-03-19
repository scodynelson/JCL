package jcl.types;

import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Tests for TypeUtils.
 */
public class TypeUtilsTest {

	/**
	 * Test for "isArrayLispTypeEqual" method.
	 */
	@Test
	public void testIsArrayLispTypeEqual() {
		final boolean result = TypeUtils.isArrayLispTypeEqual(Array.INSTANCE, Array.INSTANCE);
		Assert.assertTrue(result);
	}
}
