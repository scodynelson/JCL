package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.type.BitType;
import jcl.type.LispType;
import jcl.type.TType;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;

public class NILArrayStructImplTest {

	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Value-Of
	 */

	@Test
	public void valueOf() {

	}

	@Test
	public void valueOf1() {

	}

	@Test
	public void valueOf2() {

	}

	@Test
	public void valueOf3() {

	}

	@Test
	public void valueOf4() {

	}

	@Test
	public void valueOf5() {

	}

	/*
	Adjust-Array
	 */

	@Test
	public void adjustArray() {

	}

	@Test
	public void adjustArray1() {

	}

	@Test
	public void adjustArray2() {

	}

	@Test
	public void adjustArray3() {

	}

	@Test
	public void adjustArray4() {

	}

	/*
	Adjustable-Array-P
	 */

	@Test
	public void test_adjustableArrayP_True() {

		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             TStruct.INSTANCE);
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(TStruct.INSTANCE));
	}

	@Test
	public void test_adjustableArrayP_False() {

		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Aref
	 */

	@Test
	public void aref() {

	}

	/*
	Setf-Aref
	 */

	@Test
	public void setfAref() {

	}

	/*
	Array-Dimension
	 */

	@Test
	public void test_arrayDimension() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Cannot determine array dimension for array with rank 0."));

		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.ZERO);
	}

	/*
	Array-Dimensions
	 */

	@Test
	public void test_arrayDimensions() {
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Array-Element-Type
	 */

	@Test
	public void test_arrayElementType() {
		final BitType elementType = BitType.INSTANCE;
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(elementType,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final LispType result = array.arrayElementType();
		Assert.assertThat(result, is(elementType));
	}

	/*
	Array-Has-Fill-Pointer-P
	 */

	@Test
	public void test_arrayHasFillPointerP() {
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayHasFillPointerP();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Array-Displacement
	 */

	@Test
	public void arrayDisplacement() {

	}

	/*
	Array-In-Bounds-P
	 */

	@Test
	public void test_arrayInBoundsP() {
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP();
		Assert.assertThat(result.booleanValue(), is(true));
	}

	/*
	Array-Rank
	 */

	@Test
	public void test_arrayRank() {
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(0));
	}

	/*
	Array-Row-Major-Index
	 */

	@Test
	public void arrayRowMajorIndex() {

	}

	/*
	Array-Total-Size
	 */

	@Test
	public void test_arrayTotalSize() {
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(1));
	}

	/*
	Row-Major-Aref
	 */

	@Test
	public void rowMajorAref() {

	}

	/*
	To-String
	 */

	@Test
	public void test_toString() {

	}
}