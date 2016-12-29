package jcl.lang.internal;

import java.util.Arrays;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.type.ArrayType;
import jcl.type.BitType;
import jcl.type.LispType;
import jcl.type.NILType;
import jcl.type.SimpleArrayType;
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
	Value-Of (LispType, T, BooleanStruct)
	 */

	@Test
	public void valueOf_IE_Adj_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialElement,
		                           NILStruct.INSTANCE);
	}

	@Test
	public void valueOf_IE_Adj_IsAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	@Test
	public void valueOf_IE_Adj_IsNotAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Value-Of (LispType, SequenceStruct, BooleanStruct)
	 */

	@Test
	public void valueOf_IC_Adj_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialContents,
		                           NILStruct.INSTANCE);
	}

	@Test
	public void valueOf_IC_Adj_IsAdj() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	@Test
	public void valueOf_IC_Adj_IsNotAdj() {

		final SequenceStruct initialContents = NILStruct.INSTANCE;
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Value-Of (LispType, ArrayStruct<T>, IntegerStruct, BooleanStruct)
	 */

	@Test
	public void valueOf_Disp_OutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Requested size is too large to displace to"));

		final ArrayStruct<LispStruct> displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		NILArrayStructImpl.valueOf(TType.INSTANCE,
		                           displacedTo,
		                           IntegerStruct.ONE,
		                           NILStruct.INSTANCE);
	}

	@Test
	public void valueOf_Disp_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not an array with a subtype of the upgraded-array-element-type"));

		final ArrayStruct<LispStruct> displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           displacedTo,
		                           IntegerStruct.ZERO,
		                           NILStruct.INSTANCE);
	}

	@Test
	public void valueOf_Disp_IsAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct<LispStruct> displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             NILStruct.INSTANCE);
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             IntegerStruct.ZERO,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	@Test
	public void valueOf_Disp_IsNotAdj() {

		final ArrayStruct<LispStruct> displacedTo
				= VectorStructImpl.valueOf(2,
				                           TType.INSTANCE,
				                           Arrays.asList(IntegerStruct.TEN, IntegerStruct.TWO),
				                           false,
				                           null);
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             IntegerStruct.ONE,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(IntegerStruct.TWO));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Value-Of (LispType, T)
	 */

	@Test
	public void valueOf_IE_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialElement);
	}

	@Test
	public void valueOf_IE() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Value-Of (LispType, SequenceStruct)
	 */

	@Test
	public void valueOf_IC_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialContents);
	}

	@Test
	public void valueOf_IC() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct<LispStruct> array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
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