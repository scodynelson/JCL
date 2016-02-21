package jcl.lists;

import java.util.List;

import jcl.LispStruct;
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
		final LispStruct value = NullStruct.INSTANCE;

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
		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
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
		final LispStruct value = NullStruct.INSTANCE;

		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
		consStruct.setCdr(value);

		Assert.assertEquals(consStruct.getCdr(), value, "'cdr' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'size' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSize_cdrListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE); // Null.INSTANCE is default CDR, which is a ListStruct
		Assert.assertEquals(consStruct.size(), 1, "'size' of ConsStruct was not expected value of '1'.");
	}

	/**
	 * Test for 'size' method where the cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSize_cdrNotListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
		Assert.assertEquals(consStruct.size(), 2, "'size' of ConsStruct was not expected value of '2'.");
	}

	/**
	 * Test for 'getElement' method where the index passed is zero.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetElement_indexZero() throws Exception {
		final int index = 0;

		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getElement(index), TStruct.INSTANCE, "'element' value of ConsStruct at index 0 was not equal.");
	}

	/**
	 * Test for 'getElement' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetElement_cdrListStruct() throws Exception {
		final int index = 1;

		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE); // Null.INSTANCE is default CDR, which is a ListStruct
		Assert.assertEquals(consStruct.getElement(index), NullStruct.INSTANCE, "'element' value of ConsStruct at index 1 was not equal.");
	}

	/**
	 * Test for 'getElement' method where the index passed is not zero and cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetElement_indexNotZero_cdrNotListStruct() throws Exception {
		final int index = 1;

		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getElement(index), TStruct.INSTANCE, "'element' value of ConsStruct at index 1 was not equal.");
	}

	/**
	 * Test for 'setElement' method where the index passed is zero.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSetElement_indexZero() throws Exception {
		final int index = 0;
		final LispStruct value = NullStruct.INSTANCE;

		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		consStruct.setElement(index, value);

		Assert.assertEquals(consStruct.getElement(index), value, "'element' value of ConsStruct at index 0 was not equal.");
	}

	/**
	 * Test for 'setElement' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSetElement_cdrListStruct() throws Exception {
		final int index = 1;
		final LispStruct value = NullStruct.INSTANCE;

		final ConsStruct innerConsStruct = new ConsStruct(TStruct.INSTANCE);

		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE, innerConsStruct);
		consStruct.setElement(index, value);

		Assert.assertEquals(consStruct.getElement(index), value, "'element' value of ConsStruct at index 1 was not equal.");
	}

	/**
	 * Test for 'setElement' method where the index passed is not zero and cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testSetElement_indexNotZero_cdrNotListStruct() throws Exception {
		final int index = 1;
		final LispStruct value = NullStruct.INSTANCE;

		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
		consStruct.setElement(index, value);

		Assert.assertEquals(consStruct.getElement(index), value, "'element' value of ConsStruct at index 1 was not equal.");
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
		Assert.assertEquals(consStruct.getFirst(), TStruct.INSTANCE, "'first' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'getRest' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetRest_cdrListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);
		Assert.assertEquals(consStruct.getRest(), NullStruct.INSTANCE, "'rest' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'getRest' method where the cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetRest_cdrNotListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);

		final ListStruct rest = consStruct.getRest();
		Assert.assertTrue(rest instanceof ConsStruct, "'rest' value of ConsStruct was not an expected ConsStruct.");
		Assert.assertEquals(rest.getFirst(), NullStruct.INSTANCE, "First value of 'rest' value of ConsStruct was not equal.");
	}

	/**
	 * Test for 'getAsJavaList' method where the cdr of the ConsStruct is a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetAsJavaList_cdrListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(TStruct.INSTANCE);

		final List<LispStruct> javaList = consStruct.getAsJavaList();
		Assert.assertEquals(javaList.size(), 1, "Java List was not expected size of 2");
		Assert.assertEquals(javaList.get(0), TStruct.INSTANCE, "First element of Java List was not equal.");
	}

	/**
	 * Test for 'getAsJavaList' method where the cdr of the ConsStruct is not a ListStruct.
	 *
	 * @throws Exception
	 * 		if any error occurs
	 */
	@Test
	public void testGetAsJavaList_cdrNotListStruct() throws Exception {
		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);

		final List<LispStruct> javaList = consStruct.getAsJavaList();
		Assert.assertEquals(javaList.size(), 2, "Java List was not expected size of 2");
		Assert.assertEquals(javaList.get(0), NullStruct.INSTANCE, "First element of Java List was not equal.");
		Assert.assertEquals(javaList.get(1), TStruct.INSTANCE, "Second element of Java List was not equal.");
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
		final ConsStruct consStruct = new ConsStruct(NullStruct.INSTANCE, TStruct.INSTANCE);
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
