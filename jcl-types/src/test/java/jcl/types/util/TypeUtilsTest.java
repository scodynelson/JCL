package jcl.types.util;

import jcl.types.arrays.Array;
import org.junit.Test;

public class TypeUtilsTest {

	@Test
	public void testIsArrayLispTypeEqual() throws Exception {
		TypeUtils.isArrayLispTypeEqual(Array.INSTANCE, Array.INSTANCE);
	}

	@Test
	public void testNumberCompareTo() throws Exception {
		TypeUtils.numberCompareTo(0, 0);
	}
}
