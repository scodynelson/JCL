package jcl.lists;

import java.util.List;

import jcl.LispStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * The test class for the {@link ConsStruct} object.
 */
public class ConsStructTest {

	/**
	 * Test for 'getCar' method.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetCar() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getCar(), TStruct.INSTANCE, "'car' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'setCar' method.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSetCar() throws Exception {
		final LispStruct value = NILStruct.INSTANCE;

		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		consStruct.setCar(value);

		Assert.assertEquals(consStruct.getCar(), value, "'car' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'getCdr' method.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetCdr() throws Exception {
		final ConsStruct consStruct = new ConsStruct(NILStruct.INSTANCE, TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getCdr(), TStruct.INSTANCE, "'cdr' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'setCdr' method.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSetCdr() throws Exception {
		final LispStruct value = NILStruct.INSTANCE;

		final ConsStruct consStruct = new ConsStruct(NILStruct.INSTANCE, TStruct.INSTANCE);
		consStruct.setCdr(value);

		Assert.assertEquals(consStruct.getCdr(), value, "'cdr' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'getFirst' method.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetFirst() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getCar(), TStruct.INSTANCE, "'first' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'isDotted' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testIsDotted_cdrListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		Assert.assertFalse(consStruct.isDotted(), "ConsStruct expected to not be dotted.");
	}

	/**
	 * Test for 'isDotted' method where the cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testIsDotted_cdrNotListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(NILStruct.INSTANCE, TStruct.INSTANCE);
		Assert.assertTrue(consStruct.isDotted(), "ConsStruct expected to be dotted.");
	}

	/**
	 * Test for 'isCircular' method where the car of the ConsStruct is circular.
	 *
	 * @throws Exception if any error occurs
	 */
//	@Test
//	public void testIsCircular_true_car() throws Exception {
//
//		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
//		final ConsStruct innerConsStruct = new ConsStruct(TStruct.INSTANCE, consStruct);
//		consStruct.setCar(innerConsStruct);
//
//		Assert.assertTrue(consStruct.isCircular(), "ConsStruct expected to be circular.");
//	}

	/**
	 * Test for 'isCircular' method where the cdr of the ConsStruct is circular.
	 *
	 * @throws Exception if any error occurs
	 */
//	@Test
//	public void testIsCircular_true_cdr() throws Exception {
//
//		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
//		final ConsStruct innerConsStruct = new ConsStruct(TStruct.INSTANCE, consStruct);
//		consStruct.setCdr(innerConsStruct);
//
//		Assert.assertTrue(consStruct.isCircular(), "ConsStruct expected to be circular.");
//	}

	/**
	 * Test for 'isCircular' method where the ConsStruct is not circular.
	 *
	 * @throws Exception if any error occurs
	 */
//	@Test
//	public void testIsCircular_false() throws Exception {
//		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
//		Assert.assertFalse(consStruct.isCircular(), "ConsStruct expected to not be circular.");
//	}

	/**
	 * Test for 'toString' method where the ConsStruct is circular.
	 *
	 * @throws Exception if any error occurs
	 */
//	@Test
//	public void testToString_circular() throws Exception {
//		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
//		consStruct.setCdr(consStruct);
//
//		Assert.assertNotNull(consStruct.toString(), "'toString' value for ConsStruct was null.");
//	}

	/**
	 * Test for 'toString' method where the ConsStruct is not circular.
	 *
	 * @throws Exception if any error occurs
	 */
//	@Test
//	public void testToString_notCircular() throws Exception {
//		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
//		Assert.assertNotNull(consStruct.toString(), "'toString' value for ConsStruct was null.");
//	}
}
