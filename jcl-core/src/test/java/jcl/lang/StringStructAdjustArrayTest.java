package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.AbstractStringStructImpl;
import jcl.lang.internal.SimpleStringStructImpl;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.type.ExtendedCharType;
import jcl.type.IntegerType;
import jcl.type.LispType;
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
 * Unit tests for {@link StringStruct#adjustArray(AdjustArrayContext)} variations.
 */
public class StringStructAdjustArrayTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Adjust-Array
	 */

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the dimensions to be adjust to is not valid.
	 */
	@Test
	public void test_adjustArray_WrongDimensions() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Array cannot be adjusted to a different array dimension rank."));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final AdjustArrayContext context = AdjustArrayContext.builder().build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the element-type to be adjust to is not
	 * valid.
	 */
	@Test
	public void test_adjustArray_WrongElementType() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided upgraded-array-element-type"));
		thrown.expectMessage(containsString("must be the same as initial upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .elementType(IntegerType.INSTANCE)
		                                                     .build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'simple' string to a
	 * displaced array and the displaced-to element-type is not valid.
	 */
	@Test
	public void test_adjustArray_Displaced_WrongDisplacedType_Simple() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided array for displacement"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final VectorStruct displacedTo = VectorStruct.builder(IntegerStruct.ONE).build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'complex' string to a
	 * displaced array and the displaced-to element-type is not valid.
	 */
	@Test
	public void test_adjustArray_Displaced_WrongDisplacedType_Complex() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided array for displacement"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		final VectorStruct displacedTo = VectorStruct.builder(IntegerStruct.ONE).build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'simple' string to a
	 * displaced array and the displaced-index-offset is too large.
	 */
	@Test
	public void test_adjustArray_Displaced_DisplacedOffsetTooLarge_Simple() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Requested size is too large to displace to"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.TEN)
		                                                     .build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'complex' string to a
	 * displaced array and the displaced-index-offset is too large.
	 */
	@Test
	public void test_adjustArray_Displaced_DisplacedOffsetTooLarge_Complex() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Requested size is too large to displace to"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.TEN)
		                                                     .build();
		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will have a fill-pointer.
	 * 3.) The resulting array will be adjustable.
	 * 4.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Simple1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will not have a fill-pointer.
	 * 3.) The resulting array will not be adjustable.
	 * 4.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Simple2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex3() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex4() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex5() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex6() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex7() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex8() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex9() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex10() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex11() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Complex12() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .displacedTo(displacedTo)
		                                                     .displacedIndexOffset(IntegerStruct.ONE)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust the contents of a
	 * 'simple' string and the new contents contain an element with an invalid element-type.
	 */
	@Test
	public void test_adjustArray_IContents_BadTypeInContents_Simple() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided element"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final SequenceStruct initialContents = LispStructFactory.toProperList(CharacterConstants.AT_SIGN_CHAR,
		                                                                      IntegerStruct.ONE);
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialContents(initialContents)
		                                                     .build();

		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust the contents of a
	 * 'complex' string and the new contents contain an element with an invalid element-type.
	 */
	@Test
	public void test_adjustArray_IContents_BadTypeInContents_Complex() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided element"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		final SequenceStruct initialContents = LispStructFactory.toProperList(CharacterConstants.AT_SIGN_CHAR,
		                                                                      IntegerStruct.ONE);
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialContents(initialContents)
		                                                     .build();

		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will have a fill-pointer.
	 * 3.) The resulting array will be adjustable.
	 * 4.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Simple1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will not have a fill-pointer.
	 * 3.) The resulting array will not be adjustable.
	 * 4.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Simple2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex3() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex4() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex5() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex6() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex7() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex8() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex9() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex10() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex11() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Complex12() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("bc"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 * 6.) The resulting array size is greater than the initial-contents.
	 * <p>
	 * TODO: This test fails right due to the 'getValidContents' performing checks that it maybe shouldn't do. This is
	 * why the 'catch' clause in {@link SimpleStringStructImpl#charInternal} cannot be hit.
	 */
	@Ignore
	@Test
	public void test_adjustArray_IContents_Simple_NewSizeGreaterThanContentsSize() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .elementType(ExtendedCharType.INSTANCE)
		                                                 .initialContents(StringStruct.toLispString("ab"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStructImpl.valueOf(3))
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialContents(initialContents)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust the contents and the
	 * contents contain an element with an invalid element-type.
	 */
	@Test
	public void test_adjustArray_IElement_NotCharacter() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided element"));
		thrown.expectMessage(containsString("is not a CHARACTER."));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialElement(IntegerStruct.ZERO)
		                                                     .build();

		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'simple' string to a
	 * new element and the new element has an invalid element-type.
	 * <p>
	 * TODO: investigate when reworking type system. This is the test for covering the missing exception case in
	 * {@link AbstractStringStructImpl#validateNewInitialElement(LispStruct, LispType)}.
	 */
	@Test
	@Ignore
	public void test_adjustArray_IElement_NotSubType_Simple() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided element"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE).build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialElement(IntegerStruct.ZERO)
		                                                     .build();

		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust a 'complex' string to a
	 * new element and the new element has an invalid element-type.
	 * <p>
	 * TODO: investigate when reworking type system. This is the test for covering the missing exception case in
	 * {@link AbstractStringStructImpl#validateNewInitialElement(LispStruct, LispType)}.
	 */
	@Test
	@Ignore
	public void test_adjustArray_IElement_NotSubType_Complex() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Provided element"));
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialElement(IntegerStruct.ZERO)
		                                                     .build();

		struct.adjustArray(context);
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will have a fill-pointer.
	 * 3.) The resulting array will be adjustable.
	 * 4.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Simple1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was simple.
	 * 2.) The resulting array will not have a fill-pointer.
	 * 3.) The resulting array will not be adjustable.
	 * 4.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Simple2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.TWO));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex1() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex2() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex3() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex4() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex5() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array had initial-contents.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex6() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex7() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex8() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will have a fill-pointer.
	 * 5.) The resulting array will be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex9() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.T)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.T));
		Assert.assertThat(result.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex10() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array had a fill-pointer.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex11() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had a fill-pointer.
	 * 3.) The original array was displaced.
	 * 4.) The resulting array will not have a fill-pointer.
	 * 5.) The resulting array will not be adjustable.
	 * 6.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IElement_Complex12() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the new intial-element is provided, but the
	 * array size is reduced and the original array was adjustable.
	 */
	@Test
	public void test_adjustArray_IElement_Adjustable_ShrinkString() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("12"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the new intial-element is provided, but the
	 * array size is reduced and the original array was not adjustable.
	 */
	@Test
	public void test_adjustArray_IElement_NotAdjustable_ShrinkString() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .initialContents(StringStruct.toLispString("12"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.ONE)
		                                                     .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
		                                                     .build();

		final StringStruct result = struct.adjustArray(context);
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.arrayTotalSize(), is(IntegerStruct.TWO));
		Assert.assertThat(result.adjustableArrayP(), is(BooleanStruct.NIL));
		try {
			result.fillPointer();
			Assert.fail("Expected String not to have fill-pointer.");
		} catch (final TypeErrorException ex) {
			Assert.assertThat(ex.getMessage(), containsString("STRING has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}
}
