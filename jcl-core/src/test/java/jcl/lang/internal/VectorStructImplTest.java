package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.type.BitType;
import jcl.type.LispType;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;

public class VectorStructImplTest {

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
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(TStruct.INSTANCE));
	}

	@Test
	public void test_adjustableArrayP_False() {

		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
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
	public void test_arrayDimension_OutOfBoundsAxis_MinusOne() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		array.arrayDimension(IntegerStruct.MINUS_ONE);
	}

	@Test
	public void test_arrayDimension_OutOfBoundsAxis_More() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		array.arrayDimension(IntegerStruct.ONE);
	}

	@Test
	public void test_arrayDimension() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final IntegerStruct result = array.arrayDimension(IntegerStruct.ZERO);
		Assert.assertThat(result.intValue(), is(2));

	}

	/*
	Array-Dimensions
	 */

	@Test
	public void test_arrayDimensions() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, not(is(NILStruct.INSTANCE)));
		Assert.assertThat(result.length(), is(1L));

		final LispStruct dim = result.nth(0);
		Assert.assertThat(dim, is(IntegerStruct.TWO));
	}

	/*
	Array-Element-Type
	 */

	@Test
	public void test_arrayElementType() {
		final BitType elementType = BitType.INSTANCE;
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final LispType result = array.arrayElementType();
		Assert.assertThat(result, is(elementType));
	}

	/*
	Array-Has-Fill-Pointer-P
	 */

	@Test
	public void test_arrayHasFillPointerP() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
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
	public void test_arrayInBoundsP_False_WrongNumberOfSubscripts_Less() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final BooleanStruct result = array.arrayInBoundsP();
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_False_WrongNumberOfSubscripts_More() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_False_OutOfBoundsSubscripts_MinusOne() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.MINUS_ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_False_OutOfBoundsSubscripts_More() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.TEN);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_True() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(true));
	}

	/*
	Array-Rank
	 */

	@Test
	public void test_arrayRank() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(1));
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
	public void test_arrayTotalSize_Empty() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.ZERO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(0));
	}

	@Test
	public void test_arrayTotalSize_NotEmpty() {
		final ArrayStruct<LispStruct> array
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialElement(IntegerStruct.ZERO)
				              .build();
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(2));
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