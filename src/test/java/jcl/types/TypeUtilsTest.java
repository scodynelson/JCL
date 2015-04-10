package jcl.types;

import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Tests for TypeUtils.
 */
public class TypeUtilsTest {

	/**
	 * Test for "isArrayTypeEqual" method.
	 */
	@Test
	public void testIsArrayLispTypeEqual() {
		final boolean result = TypeUtils.isArrayTypeEqual(ArrayType.INSTANCE, ArrayType.INSTANCE);
		Assert.assertTrue(result);
	}
}
