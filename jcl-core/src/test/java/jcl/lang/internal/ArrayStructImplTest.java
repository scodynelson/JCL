package jcl.lang.internal;

import java.util.Arrays;
import java.util.Collections;

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
import static org.hamcrest.CoreMatchers.not;

public class ArrayStructImplTest {

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
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          TStruct.INSTANCE);
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(TStruct.INSTANCE));
	}

	@Test
	public void test_adjustableArrayP_False() {

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
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
	public void test_arrayDimension_0Dimension() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Cannot determine array dimension for array with rank 0."));

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.ZERO);
	}

	@Test
	public void test_arrayDimension_1Dimension_OutOfBoundsAxis_MinusOne() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.MINUS_ONE);
	}

	@Test
	public void test_arrayDimension_1Dimension_OutOfBoundsAxis_More() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.ONE);
	}

	@Test
	public void test_arrayDimension_1Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayDimension(IntegerStruct.ZERO);
		Assert.assertThat(result.intValue(), is(2));

	}

	@Test
	public void test_arrayDimension_MultiDimension_OutOfBoundsAxis_MinusOne() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TEN),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.MINUS_ONE);
	}

	@Test
	public void test_arrayDimension_MultiDimension_OutOfBoundsAxis_More() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds"));

		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TEN),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		array.arrayDimension(IntegerStruct.TEN);
	}

	@Test
	public void test_arrayDimension_MultiDimension_First() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TEN),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayDimension(IntegerStruct.ZERO);
		Assert.assertThat(result.intValue(), is(2));
	}

	@Test
	public void test_arrayDimension_MultiDimension_Second() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TEN),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayDimension(IntegerStruct.ONE);
		Assert.assertThat(result.intValue(), is(10));
	}

	/*
	Array-Dimensions
	 */

	@Test
	public void test_arrayDimensions_0Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	@Test
	public void test_arrayDimensions_1Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, not(is(NILStruct.INSTANCE)));
		Assert.assertThat(result.length(), is(1L));

		final LispStruct dim = result.nth(0);
		Assert.assertThat(dim, is(IntegerStruct.TWO));
	}

	@Test
	public void test_arrayDimensions_MultiDimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TEN),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, not(is(NILStruct.INSTANCE)));
		Assert.assertThat(result.length(), is(2L));

		final LispStruct dim1 = result.nth(0);
		Assert.assertThat(dim1, is(IntegerStruct.TWO));

		final LispStruct dim2 = result.nth(1);
		Assert.assertThat(dim2, is(IntegerStruct.TEN));
	}

	/*
	Array-Element-Type
	 */

	@Test
	public void test_arrayElementType() {
		final BitType elementType = BitType.INSTANCE;
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          elementType,
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
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
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
	public void test_arrayInBoundsP_0Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP();
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_1Dimension_False_WrongNumberOfSubscripts_Less() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP();
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_1Dimension_False_WrongNumberOfSubscripts_More() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_1Dimension_False_OutOfBoundsSubscripts_MinusOne() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.MINUS_ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_1Dimension_False_OutOfBoundsSubscripts_More() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.TEN);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_1Dimension_True() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(true));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_WrongNumberOfSubscripts_Less() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_WrongNumberOfSubscripts_More() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.ONE, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_OutOfBoundsSubscripts_FirstDim_MinusOne() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.MINUS_ONE, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_OutOfBoundsSubscripts_SecondDim_MinusOne() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.MINUS_ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_OutOfBoundsSubscripts_FirstDim_More() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.TEN, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_False_OutOfBoundsSubscripts_SecondDim_More() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.TEN);
		Assert.assertThat(result.booleanValue(), is(false));
	}

	@Test
	public void test_arrayInBoundsP_MultiDimension_True() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final BooleanStruct result = array.arrayInBoundsP(IntegerStruct.ONE, IntegerStruct.ONE);
		Assert.assertThat(result.booleanValue(), is(true));
	}

	/*
	Array-Rank
	 */

	@Test
	public void test_arrayRank_0Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(0));
	}

	@Test
	public void test_arrayRank_1Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(1));
	}

	@Test
	public void test_arrayRank_MultiDimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(3));
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
	public void test_arrayTotalSize_0Dimension() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.emptyList(),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(1));
	}

	@Test
	public void test_arrayTotalSize_1Dimension_Empty() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.ZERO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(0));
	}

	@Test
	public void test_arrayTotalSize_1Dimension_NotEmpty() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Collections.singletonList(IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(2));
	}

	@Test
	public void test_arrayTotalSize_MultiDimension_Empty() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.ZERO, IntegerStruct.ZERO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(0));
	}

	@Test
	public void test_arrayTotalSize_MultiDimension_NotEmpty() {
		final ArrayStruct<LispStruct> array
				= ArrayStructImpl.valueOf(Arrays.asList(IntegerStruct.TWO, IntegerStruct.TWO),
				                          TType.INSTANCE,
				                          IntegerStruct.ZERO,
				                          NILStruct.INSTANCE);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(4));
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