package jcl.types;

import org.junit.Assert;
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

	/**
	 * Test for "numberCompareTo" method.
	 */
	@Test
	public void testNumberCompareTo() {
		final int result = TypeUtils.numberCompareTo(0, 0);
		Assert.assertEquals(-1, result);
	}
}
