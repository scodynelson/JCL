package jcl.lang.internal;

import java.util.Collections;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.PrinterVariables;
import jcl.type.ArrayType;
import jcl.type.BitType;
import jcl.type.LispType;
import jcl.type.NILType;
import jcl.type.SimpleArrayType;
import jcl.type.TType;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.sameInstance;

/**
 * Unit tests for {@link NILArrayStructImpl} implementation class.
 */
public class NILArrayStructImplTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Value-Of (LispType, T, BooleanStruct)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, LispStruct, BooleanStruct)} where a {@link
	 * TypeErrorException} is thrown.
	 */
	@Test
	public void test_valueOf_IE_Adj_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialElement,
		                           NILStruct.INSTANCE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, LispStruct, BooleanStruct)} where the resulting array is
	 * adjustable.
	 */
	@Test
	public void test_valueOf_IE_Adj_IsAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, LispStruct, BooleanStruct)} where the resulting array is not
	 * adjustable.
	 */
	@Test
	public void test_valueOf_IE_Adj_IsNotAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
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

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, SequenceStruct, BooleanStruct)} where a {@link
	 * TypeErrorException} is thrown.
	 */
	@Test
	public void test_valueOf_IC_Adj_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialContents,
		                           NILStruct.INSTANCE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, SequenceStruct, BooleanStruct)} where the resulting array is
	 * adjustable.
	 */
	@Test
	public void test_valueOf_IC_Adj_IsAdj() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, SequenceStruct, BooleanStruct)} where the resulting array is
	 * not adjustable.
	 */
	@Test
	public void test_valueOf_IC_Adj_IsNotAdj() {

		final SequenceStruct initialContents = NILStruct.INSTANCE;
		final ArrayStruct array
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

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, ArrayStruct, IntegerStruct, BooleanStruct)} where the
	 * displacedIndexOffset is out of bounds.
	 */
	@Test
	public void test_valueOf_Disp_OutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Requested size is too large to displace to"));

		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(TType.INSTANCE,
		                           displacedTo,
		                           IntegerStruct.ONE,
		                           NILStruct.INSTANCE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, ArrayStruct, IntegerStruct, BooleanStruct)} where a {@link
	 * TypeErrorException} is thrown.
	 */
	@Test
	public void test_valueOf_Disp_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not an array with a subtype of the upgraded-array-element-type"));

		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           displacedTo,
		                           IntegerStruct.ZERO,
		                           NILStruct.INSTANCE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, ArrayStruct, IntegerStruct, BooleanStruct)} where the
	 * resulting array is adjustable.
	 */
	@Test
	public void test_valueOf_Disp_IsAdj() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             displacedIndexOffset,
				                             TStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(TStruct.INSTANCE));

		final ValuesStruct displacement = array.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, ArrayStruct, IntegerStruct, BooleanStruct)} where the
	 * resulting array is not adjustable.
	 */
	@Test
	public void test_valueOf_Disp_IsNotAdj() {

		final ListStruct initialContents = LispStructFactory.toProperList(IntegerStruct.TEN, IntegerStruct.TWO);
		final ArrayStruct displacedTo
				= VectorStruct.builder(IntegerStruct.TWO)
				              .initialContents(initialContents)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		final IntegerStruct displacedIndexOffset = IntegerStruct.ONE;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             displacedIndexOffset,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(IntegerStruct.TWO));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));

		final ValuesStruct displacement = array.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/*
	Value-Of (LispType, T)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, LispStruct)} where a {@link TypeErrorException} is thrown.
	 */
	@Test
	public void test_valueOf_IE_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialElement);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, LispStruct)}.
	 */
	@Test
	public void test_valueOf_IE() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Value-Of (LispType, SequenceStruct)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, SequenceStruct)} where a {@link TypeErrorException} is
	 * thrown.
	 */
	@Test
	public void test_valueOf_IC_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		NILArrayStructImpl.valueOf(NILType.INSTANCE,
		                           initialContents);
	}

	/**
	 * Test for {@link NILArrayStructImpl#valueOf(LispType, SequenceStruct)}.
	 */
	@Test
	public void test_valueOf_IC() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Adjust-Array (List<IntegerStruct>, LispType, TYPE)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the new
	 * dimensions would result in a different array rank.
	 */
	@Test
	public void test_adjustArray_IE_DifferentRank() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Array cannot be adjusted to a different array dimension rank."));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.singletonList(IntegerStruct.ONE),
		                  TType.INSTANCE,
		                  initialElement,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the fill-pointer
	 * parameter is non-null.
	 */
	@Test
	public void test_adjustArray_IE_FillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Non-vector arrays cannot adjust fill-pointer."));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  initialElement,
		                  IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the element-type
	 * of the original array is not equivalent to the newly provided element-type.
	 */
	@Test
	public void test_adjustArray_IE_InitialElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  initialElement,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the newly
	 * provided element-type is not equivalent to the element-type of the original array.
	 */
	@Test
	@Ignore
	public void test_adjustArray_IE_NewElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ONE;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  initialElement,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the new
	 * initial-element is not a subtype of the provided element-type.
	 */
	@Test
	public void test_adjustArray_IE_InitialElementNotSubtype() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialElement);

		final CharacterStruct newElement = CharacterStruct.toLispCharacter('a');
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  newElement,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the original
	 * array was adjustable.
	 */
	@Test
	public void test_adjustArray_IE_WasAdjustable() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             TStruct.INSTANCE);

		final IntegerStruct newElement = IntegerStruct.ONE;
		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    newElement,
				                    null);
		Assert.assertThat(array, sameInstance(result));
		Assert.assertThat(result.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newElement));
		Assert.assertThat(result.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, LispStruct, IntegerStruct)} where the original
	 * array was not adjustable.
	 */
	@Test
	public void test_adjustArray_IE_WasNotAdjustable() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);

		final IntegerStruct newElement = IntegerStruct.ONE;
		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    newElement,
				                    null);
		Assert.assertThat(array, not(sameInstance(result)));
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));

		Assert.assertThat(result.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newElement));
		Assert.assertThat(result.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Adjust-Array (List<IntegerStruct>, LispType, SequenceStruct)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the new
	 * dimensions would result in a different array rank.
	 */
	@Test
	public void test_adjustArray_IC_DifferentRank() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Array cannot be adjusted to a different array dimension rank."));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);
		array.adjustArray(Collections.singletonList(IntegerStruct.ONE),
		                  TType.INSTANCE,
		                  initialContents,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the
	 * fill-pointer parameter is non-null.
	 */
	@Test
	public void test_adjustArray_IC_FillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Non-vector arrays cannot adjust fill-pointer."));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  initialContents,
		                  IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the
	 * element-type of the original array is not equivalent to the newly provided element-type.
	 */
	@Test
	public void test_adjustArray_IC_InitialElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialContents);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  initialContents,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the newly
	 * provided element-type is not equivalent to the element-type of the original array.
	 */
	@Test
	@Ignore
	public void test_adjustArray_IC_NewElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ONE);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  initialContents,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the new
	 * initial-contents have an element that is not a subtype of the provided element-type.
	 */
	@Test
	public void test_adjustArray_IC_InitialContentsNotSubtype() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialElement);
		final SequenceStruct newContents = LispStructFactory.toProperList(CharacterStruct.toLispCharacter('a'));
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  newContents,
		                  null);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the original
	 * array was adjustable.
	 */
	@Test
	public void test_adjustArray_IC_WasAdjustable() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents,
				                             TStruct.INSTANCE);

		final SequenceStruct newContents = LispStructFactory.toProperList(IntegerStruct.ONE);
		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    newContents,
				                    null);
		Assert.assertThat(array, sameInstance(result));
		Assert.assertThat(result.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newContents));
		Assert.assertThat(result.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, SequenceStruct, IntegerStruct)} where the original
	 * array was not adjustable.
	 */
	@Test
	public void test_adjustArray_IC_WasNotAdjustable() {

		final SequenceStruct initialContents = LispStructFactory.toProperList(IntegerStruct.ZERO);
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialContents);

		final SequenceStruct newContents = LispStructFactory.toProperList(IntegerStruct.ONE);
		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    newContents,
				                    null);
		Assert.assertThat(array, not(sameInstance(result)));
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialContents));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));

		Assert.assertThat(result.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newContents));
		Assert.assertThat(result.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Adjust-Array (List<IntegerStruct>, LispType, ArrayStruct<TYPE>, IntegerStruct)
	 */

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the new dimensions would result in a different array rank.
	 */
	@Test
	public void test_adjustArray_Disp_DifferentRank() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Array cannot be adjusted to a different array dimension rank."));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.singletonList(IntegerStruct.ONE),
		                  TType.INSTANCE,
		                  null,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the fill-pointer parameter is non-null.
	 */
	@Test
	public void test_adjustArray_Disp_FillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Non-vector arrays cannot adjust fill-pointer."));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  IntegerStruct.ZERO,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * displaced-index-offset is not a valid row-major-index in the provided displaced-to array.
	 */
	@Test
	public void test_adjustArray_Disp_OutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Requested size is too large to displace to"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ONE;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  null,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the element-type of the original array is not equivalent to the newly provided element-type.
	 */
	@Test
	public void test_adjustArray_Disp_InitialElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  TType.INSTANCE,
		                  null,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the newly provided element-type is not equivalent to the element-type of the original array.
	 */
	@Test
	@Ignore
	public void test_adjustArray_Disp_NewElementTypeNotEqual() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ONE;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  null,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the element-type of the new displaced-to array is not a subtype of the provided element-type.
	 */
	@Test
	public void test_adjustArray_Disp_DisplacedToNotSubtype() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(BitType.INSTANCE,
				                             initialElement);
		array.adjustArray(Collections.emptyList(),
		                  BitType.INSTANCE,
		                  null,
		                  displacedTo,
		                  displacedIndexOffset);
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the original array was adjustable.
	 */
	@Test
	public void test_adjustArray_Disp_WasAdjustable() {

		final IntegerStruct newElement = IntegerStruct.ONE;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             newElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement,
				                             TStruct.INSTANCE);

		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    null,
				                    displacedTo,
				                    displacedIndexOffset);
		Assert.assertThat(array, sameInstance(result));
		Assert.assertThat(result.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newElement));
		Assert.assertThat(result.adjustableArrayP(), is(TStruct.INSTANCE));

		final ValuesStruct displacement = array.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustArray(List, LispType, IntegerStruct, ArrayStruct, IntegerStruct)} where
	 * the original array was not adjustable.
	 */
	@Test
	public void test_adjustArray_Disp_WasNotAdjustable() {

		final IntegerStruct newElement = IntegerStruct.ONE;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             newElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);

		final ArrayStruct result
				= array.adjustArray(Collections.emptyList(),
				                    TType.INSTANCE,
				                    null,
				                    displacedTo,
				                    displacedIndexOffset);
		Assert.assertThat(array, not(sameInstance(result)));
		Assert.assertThat(array.getType(), is(SimpleArrayType.INSTANCE));
		Assert.assertThat(array.aref(), is(initialElement));
		Assert.assertThat(array.adjustableArrayP(), is(NILStruct.INSTANCE));

		Assert.assertThat(result.getType(), is(ArrayType.INSTANCE));
		Assert.assertThat(result.aref(), is(newElement));
		Assert.assertThat(result.adjustableArrayP(), is(NILStruct.INSTANCE));

		final ValuesStruct displacement = result.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/*
	Adjustable-Array-P
	 */

	/**
	 * Test for {@link NILArrayStructImpl#adjustableArrayP()} where the array is adjustable.
	 */
	@Test
	public void test_adjustableArrayP_True() {

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             TStruct.INSTANCE);
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link NILArrayStructImpl#adjustableArrayP()} where the array is not adjustable.
	 */
	@Test
	public void test_adjustableArrayP_False() {

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO,
				                             NILStruct.INSTANCE);
		final BooleanStruct result = array.adjustableArrayP();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Aref
	 */

	/**
	 * Test for {@link NILArrayStructImpl#aref(IntegerStruct...)} where the wrong number of subscripts were provided.
	 */
	@Test
	public void test_aref_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.aref(IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link NILArrayStructImpl#aref(IntegerStruct...)}.
	 */
	@Test
	public void test_aref() {
		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.aref(), is(initialElement));
	}

	/**
	 * Test for {@link NILArrayStructImpl#aref(IntegerStruct...)} where the array is displaced.
	 */
	@Test
	public void test_aref_Displaced() {
		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             displacedIndexOffset,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.aref(), is(initialElement));
	}

	/*
	Setf-Aref
	 */

	/**
	 * Test for {@link NILArrayStructImpl#setfAref(LispStruct, IntegerStruct...)} where the wrong number of subscripts
	 * were provided.
	 */
	@Test
	public void test_setfAref_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.aref(), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		array.setfAref(newElement, IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link NILArrayStructImpl#setfAref(LispStruct, IntegerStruct...)}.
	 */
	@Test
	public void test_setfAref() {
		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.aref(), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		final LispStruct result = array.setfAref(newElement);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(array.aref(), is(newElement));
	}

	/**
	 * Test for {@link NILArrayStructImpl#setfAref(LispStruct, IntegerStruct...)} where the array is displaced.
	 */
	@Test
	public void test_setfAref_Displaced() {
		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             displacedIndexOffset,
				                             NILStruct.INSTANCE);
		Assert.assertThat(array.aref(), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		final LispStruct result = array.setfAref(newElement);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(array.aref(), is(newElement));
		Assert.assertThat(displacedTo.aref(), is(newElement));
	}

	/*
	Array-Dimension
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayDimension(IntegerStruct)}.
	 */
	@Test
	public void test_arrayDimension() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Cannot determine array dimension for array with rank 0."));

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		array.arrayDimension(IntegerStruct.ZERO);
	}

	/*
	Array-Dimensions
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayDimensions()}.
	 */
	@Test
	public void test_arrayDimensions() {
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		final ListStruct result = array.arrayDimensions();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Array-Element-Type
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayElementType()}.
	 */
	@Test
	public void test_arrayElementType() {
		final BitType elementType = BitType.INSTANCE;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(elementType,
				                             IntegerStruct.ZERO);
		final LispType result = array.arrayElementType();
		Assert.assertThat(result, is(elementType));
	}

	/*
	Array-Has-Fill-Pointer-P
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayHasFillPointerP()}.
	 */
	@Test
	public void test_arrayHasFillPointerP() {
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		final BooleanStruct result = array.arrayHasFillPointerP();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Array-Displacement
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayDisplacement()}.
	 */
	@Test
	public void test_arrayDisplacement() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);

		final ValuesStruct displacement = array.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(displacement.getValuesList().get(1), is(IntegerStruct.ZERO));
	}

	/**
	 * Test for {@link NILArrayStructImpl#arrayDisplacement()} where the array is displaced.
	 */
	@Test
	public void test_arrayDisplacement_Displaced() {

		final IntegerStruct initialElement = IntegerStruct.TWO;
		final ArrayStruct displacedTo
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             displacedTo,
				                             displacedIndexOffset,
				                             NILStruct.INSTANCE);

		final ValuesStruct displacement = array.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/*
	Array-In-Bounds-P
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayInBoundsP(IntegerStruct...)}.
	 */
	@Test
	public void test_arrayInBoundsP() {
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		final BooleanStruct result = array.arrayInBoundsP();
		Assert.assertThat(result.booleanValue(), is(true));
	}

	/*
	Array-Rank
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayRank()}.
	 */
	@Test
	public void test_arrayRank() {
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		final IntegerStruct result = array.arrayRank();
		Assert.assertThat(result.intValue(), is(0));
	}

	/*
	Array-Row-Major-Index
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayRowMajorIndex(IntegerStruct...)} where the wrong number of subscripts
	 * were provided.
	 */
	@Test
	public void test_arrayRowMajorIndex_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.arrayRowMajorIndex(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#arrayRowMajorIndex(IntegerStruct...)}.
	 */
	@Test
	public void test_arrayRowMajorIndex() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final IntegerStruct result = array.arrayRowMajorIndex();
		Assert.assertThat(result, is(IntegerStruct.ZERO));
	}

	/*
	Array-Total-Size
	 */

	/**
	 * Test for {@link NILArrayStructImpl#arrayTotalSize()}.
	 */
	@Test
	public void test_arrayTotalSize() {
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             IntegerStruct.ZERO);
		final IntegerStruct result = array.arrayTotalSize();
		Assert.assertThat(result.intValue(), is(1));
	}

	/*
	Row-Major-Aref
	 */

	/**
	 * Test for {@link NILArrayStructImpl#rowMajorAref(IntegerStruct)} where the provided index was out of bounds.
	 */
	@Test
	public void test_rowMajorAref_OutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		array.rowMajorAref(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#rowMajorAref(IntegerStruct)}.
	 */
	@Test
	public void test_rowMajorAref() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		final LispStruct result = array.rowMajorAref(IntegerStruct.ZERO);
		Assert.assertThat(result, is(initialElement));
	}

	/*
	Setf-Row-Major-Aref
	 */

	/**
	 * Test for {@link NILArrayStructImpl#setfRowMajorAref(LispStruct, IntegerStruct)} where the provided index was out
	 * of bounds.
	 */
	@Test
	public void test_setfRowMajorAref_OutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		array.setfRowMajorAref(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link NILArrayStructImpl#setfRowMajorAref(LispStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfRowMajorAref() {

		final IntegerStruct initialElement = IntegerStruct.ZERO;
		final ArrayStruct array
				= NILArrayStructImpl.valueOf(TType.INSTANCE,
				                             initialElement);
		Assert.assertThat(array.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		final LispStruct result = array.setfRowMajorAref(newElement, IntegerStruct.ZERO);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(array.aref(), is(newElement));
	}

	/*
	To-String
	 */

	/**
	 * Test for {@link NILArrayStructImpl#toString()} where {@link PrinterVariables#PRINT_ARRAY} is true and {@link
	 * PrinterVariables#PRINT_READABLY} is false.
	 */
	@Test
	public void test_toString_PrintArray() {

		final BooleanStruct originalPA = PrinterVariables.PRINT_ARRAY.getVariableValue();
		final BooleanStruct originalPR = PrinterVariables.PRINT_READABLY.getVariableValue();
		try {
			PrinterVariables.PRINT_ARRAY.setValue(TStruct.INSTANCE);
			PrinterVariables.PRINT_READABLY.setValue(NILStruct.INSTANCE);

			final IntegerStruct initialElement = IntegerStruct.ZERO;
			final ArrayStruct array
					= NILArrayStructImpl.valueOf(TType.INSTANCE,
					                             initialElement);
			final String result = array.toString();
			Assert.assertThat(result, is("#0A" + initialElement));
		} finally {
			PrinterVariables.PRINT_ARRAY.setValue(originalPA);
			PrinterVariables.PRINT_READABLY.setValue(originalPR);
		}
	}

	/**
	 * Test for {@link NILArrayStructImpl#toString()} where {@link PrinterVariables#PRINT_ARRAY} is false and {@link
	 * PrinterVariables#PRINT_READABLY} is true.
	 */
	@Test
	public void test_toString_PrintReadably() {

		final BooleanStruct originalPA = PrinterVariables.PRINT_ARRAY.getVariableValue();
		final BooleanStruct originalPR = PrinterVariables.PRINT_READABLY.getVariableValue();
		try {
			PrinterVariables.PRINT_ARRAY.setValue(NILStruct.INSTANCE);
			PrinterVariables.PRINT_READABLY.setValue(TStruct.INSTANCE);

			final IntegerStruct initialElement = IntegerStruct.ZERO;
			final ArrayStruct array
					= NILArrayStructImpl.valueOf(TType.INSTANCE,
					                             initialElement);
			final String result = array.toString();
			Assert.assertThat(result, is("#0A" + initialElement));
		} finally {
			PrinterVariables.PRINT_ARRAY.setValue(originalPA);
			PrinterVariables.PRINT_READABLY.setValue(originalPR);
		}
	}

	/**
	 * Test for {@link NILArrayStructImpl#toString()} where both {@link PrinterVariables#PRINT_ARRAY} and {@link
	 * PrinterVariables#PRINT_READABLY} are true.
	 */
	@Test
	public void test_toString_PrintArray_PrintReadably() {

		final BooleanStruct originalPA = PrinterVariables.PRINT_ARRAY.getVariableValue();
		final BooleanStruct originalPR = PrinterVariables.PRINT_READABLY.getVariableValue();
		try {
			PrinterVariables.PRINT_ARRAY.setValue(TStruct.INSTANCE);
			PrinterVariables.PRINT_READABLY.setValue(TStruct.INSTANCE);

			final IntegerStruct initialElement = IntegerStruct.ZERO;
			final ArrayStruct array
					= NILArrayStructImpl.valueOf(TType.INSTANCE,
					                             initialElement);
			final String result = array.toString();
			Assert.assertThat(result, is("#0A" + initialElement));
		} finally {
			PrinterVariables.PRINT_ARRAY.setValue(originalPA);
			PrinterVariables.PRINT_READABLY.setValue(originalPR);
		}
	}

	/**
	 * Test for {@link NILArrayStructImpl#toString()} where both {@link PrinterVariables#PRINT_ARRAY} and {@link
	 * PrinterVariables#PRINT_READABLY} are false and the array is adjustable.
	 */
	@Test
	public void test_toString_Adjustable() {

		final BooleanStruct originalPA = PrinterVariables.PRINT_ARRAY.getVariableValue();
		final BooleanStruct originalPR = PrinterVariables.PRINT_READABLY.getVariableValue();
		try {
			PrinterVariables.PRINT_ARRAY.setValue(NILStruct.INSTANCE);
			PrinterVariables.PRINT_READABLY.setValue(NILStruct.INSTANCE);

			final IntegerStruct initialElement = IntegerStruct.ZERO;
			final ArrayStruct array
					= NILArrayStructImpl.valueOf(TType.INSTANCE,
					                             initialElement,
					                             TStruct.INSTANCE);
			final String result = array.toString();
			Assert.assertThat(result, containsString("#<"));
			Assert.assertThat(result, containsString(" NIL type "));
			Assert.assertThat(result, containsString(" adjustable"));
			Assert.assertThat(result, containsString(">"));
		} finally {
			PrinterVariables.PRINT_ARRAY.setValue(originalPA);
			PrinterVariables.PRINT_READABLY.setValue(originalPR);
		}
	}

	/**
	 * Test for {@link NILArrayStructImpl#toString()} where both {@link PrinterVariables#PRINT_ARRAY} and {@link
	 * PrinterVariables#PRINT_READABLY} are false and the array is not adjustable.
	 */
	@Test
	public void test_toString_NotAdjustable() {

		final BooleanStruct originalPA = PrinterVariables.PRINT_ARRAY.getVariableValue();
		final BooleanStruct originalPR = PrinterVariables.PRINT_READABLY.getVariableValue();
		try {
			PrinterVariables.PRINT_ARRAY.setValue(NILStruct.INSTANCE);
			PrinterVariables.PRINT_READABLY.setValue(NILStruct.INSTANCE);

			final IntegerStruct initialElement = IntegerStruct.ZERO;
			final ArrayStruct array
					= NILArrayStructImpl.valueOf(TType.INSTANCE,
					                             initialElement);
			final String result = array.toString();
			Assert.assertThat(result, containsString("#<"));
			Assert.assertThat(result, containsString(" NIL type "));
			Assert.assertThat(result, not(containsString(" adjustable")));
			Assert.assertThat(result, containsString(">"));
		} finally {
			PrinterVariables.PRINT_ARRAY.setValue(originalPA);
			PrinterVariables.PRINT_READABLY.setValue(originalPR);
		}
	}
}