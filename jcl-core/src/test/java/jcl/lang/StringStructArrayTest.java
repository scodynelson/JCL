package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.IntegerType;
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
 * Unit tests for {@link StringStruct} array methods.
 */
public class StringStructArrayTest {

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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust to a displaced array and
	 * the displaced-to element-type is not valid.
	 */
	@Test
	public void test_adjustArray_Displaced_WrongDisplacedType() {
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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust to a displaced array and
	 * the displaced-index-offset is too large.
	 */
	@Test
	public void test_adjustArray_Displaced_DisplacedOffsetTooLarge() {
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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_Adjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
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
	public void test_adjustArray_Displaced_Adjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
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
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was not adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_NotAdjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .elementType(ExtendedCharType.INSTANCE)
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
	 * 1.) The original array was not adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be a displaced array.
	 */
	@Test
	public void test_adjustArray_Displaced_NotAdjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct displacedTo = StringStruct.builder(IntegerStructImpl.valueOf(3))
		                                             .initialContents(StringStruct.toLispString("abc"))
		                                             .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(displacedTo));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ONE));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_C_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust the contents and the
	 * contents contain an element with an invalid element-type.
	 */
	@Test
	public void test_adjustArray_IContents_BadTypeInContents() {
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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_Adjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("ab"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
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
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
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
	public void test_adjustArray_IContents_Adjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("ab"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was not adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_NotAdjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .elementType(ExtendedCharType.INSTANCE)
		                                                 .initialContents(StringStruct.toLispString("ab"))
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
	 * 1.) The original array was not adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new initial-contents.
	 */
	@Test
	public void test_adjustArray_IContents_NotAdjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final StringStruct initialContents = StringStruct.builder(IntegerStruct.TWO)
		                                                 .initialContents(StringStruct.toLispString("ab"))
		                                                 .build();
		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where attempting to adjust to a new element and the
	 * new element has an invalid element-type.
	 *
	 * TODO: cannot support this test due to the current type system among other things and not properly subtyping characters
	 */
	@Test
	@Ignore
	public void test_adjustArray_IElement_NotSubType() {
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
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new element filling in the contents.
	 */
	@Test
	public void test_adjustArray_IElement_Adjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .initialContents(StringStruct.toLispString("1"))
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .fillPointer(IntegerStruct.ONE)
		                                                     .adjustable(BooleanStruct.T)
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
	 * 5.) The resulting array will be an array with the new element filling in the contents.
	 */
	@Test
	public void test_adjustArray_IElement_Adjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.T)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
		Assert.assertThat(result.char_(IntegerStruct.ONE), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/**
	 * Test for {@link StringStruct#adjustArray(AdjustArrayContext)} where the following applies:
	 * 1.) The original array was not adjustable.
	 * 2.) The original array had initial-contents.
	 * 3.) The resulting array will have a fill-pointer.
	 * 4.) The resulting array will be adjustable.
	 * 5.) The resulting array will be an array with the new element filling in the contents.
	 */
	@Test
	public void test_adjustArray_IElement_NotAdjustable_NewFillPointer_WillBeAdjustable_HadContents() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
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
	 * 1.) The original array was not adjustable.
	 * 2.) The original array was displaced.
	 * 3.) The resulting array will not have a fill-pointer.
	 * 4.) The resulting array will not be adjustable.
	 * 5.) The resulting array will be an array with the new element filling in the contents.
	 */
	@Test
	public void test_adjustArray_IElement_NotAdjustable_NoFillPointer_WillNotBeAdjustable_HadDisplacement() {
		final StringStruct originalDisplacedTo = StringStruct.builder(IntegerStruct.ONE)
		                                                     .initialContents(StringStruct.toLispString("1"))
		                                                     .build();
		final StringStruct struct = StringStruct.builder(IntegerStruct.ONE)
		                                        .adjustable(BooleanStruct.NIL)
		                                        .displacedTo(originalDisplacedTo)
		                                        .build();

		final AdjustArrayContext context = AdjustArrayContext.builder(IntegerStruct.TWO)
		                                                     .adjustable(BooleanStruct.NIL)
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
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
			Assert.assertThat(ex.getMessage(), containsString("VECTOR has no fill-pointer to retrieve."));
		}
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(result.arrayDisplacement().getValuesList().get(1), is(IntegerStruct.ZERO));
		Assert.assertThat(result.char_(IntegerStruct.ZERO), is(CharacterConstants.DOLLAR_SIGN_CHAR));
	}

	/*
	Adjustable-Array-P
	 */

	/**
	 * Test for {@link StringStruct#adjustableArrayP()} where the array is adjustable.
	 */
	@Test
	public void test_adjustableArrayP_True() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		Assert.assertThat(struct.adjustableArrayP(), is(TStruct.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct#adjustableArrayP()} where the array is not adjustable.
	 */
	@Test
	public void test_adjustableArrayP_False() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .adjustable(NILStruct.INSTANCE)
				              .build();
		Assert.assertThat(struct.adjustableArrayP(), is(NILStruct.INSTANCE));
	}

	/*
	Aref
	 */

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)} where the wrong number of subscripts were provided.
	 */
	@Test
	public void test_aref_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.aref();
	}

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)} where the index provided was too small.
	 */
	@Test
	public void test_aref_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.aref(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)} where the index provided was too large.
	 */
	@Test
	public void test_aref_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.aref(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)} where the total size is greater than the contents.
	 */
	@Test
	public void test_aref_TotalSizeGreaterThanContents() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		struct.adjustArray(AdjustArrayContext.builder(IntegerStruct.TWO)
		                                     .build());
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));
		Assert.assertThat(struct.aref(IntegerStruct.ONE), is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)}.
	 */
	@Test
	public void test_aref() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#aref(IntegerStruct...)} where the array is displaced.
	 */
	@Test
	public void test_aref_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));
	}

	/*
	Setf-Aref
	 */

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)} where the provided new element is not a
	 * {@link CharacterStruct}.
	 */
	@Test
	public void test_setfAref_NewElementNotCharacterStruct() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a character type."));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		struct.setfAref(newElement, IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)} where the wrong number of subscripts
	 * were provided.
	 */
	@Test
	public void test_setfAref_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfAref(newElement);
	}

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)} where the index provided was too small.
	 */
	@Test
	public void test_setfAref_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfAref(newElement, IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)} where the index provided was too large.
	 */
	@Test
	public void test_setfAref_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfAref(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)}.
	 */
	@Test
	public void test_setfAref() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfAref(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#setfAref(LispStruct, IntegerStruct...)} where the array is displaced.
	 */
	@Test
	public void test_setfAref_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfAref(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.aref(IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(displacedTo.aref(IntegerStruct.ZERO), is(newElement));
	}

	/*
	Array-Dimension
	 */

	/**
	 * Test for {@link StringStruct#arrayDimension(IntegerStruct)} where the provided axis is out of bounds.
	 */
	@Test
	public void test_arrayDimension_AxisOutOfBounds() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		struct.arrayDimension(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#arrayDimension(IntegerStruct)}.
	 */
	@Test
	public void test_arrayDimension() {
		final IntegerStruct size = IntegerStruct.TEN;
		final StringStruct struct
				= StringStruct.builder(size)
				              .build();
		Assert.assertThat(struct.arrayDimension(IntegerStruct.ZERO), is(size));
	}

	/*
	Array-Dimensions
	 */

	/**
	 * Test for {@link StringStruct#arrayDimensions()}.
	 */
	@Test
	public void test_arrayDimensions() {
		final IntegerStruct size = IntegerStruct.TEN;
		final StringStruct struct
				= StringStruct.builder(size)
				              .build();
		final ListStruct result = struct.arrayDimensions();
		Assert.assertThat(result.length(), is(IntegerStruct.ONE));
		Assert.assertThat(result.getCar(), is(size));
	}

	/*
	Array-Element-Type
	 */

	/**
	 * Test for {@link StringStruct#arrayElementType()}.
	 */
	@Test
	public void test_arrayElementType() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.arrayElementType(), is(CharacterType.INSTANCE));
	}

	/*
	Array-Has-Fill-Pointer-P
	 */

	/**
	 * Test for {@link StringStruct#arrayHasFillPointerP()} where no fill-pointer exists.
	 */
	@Test
	public void test_arrayHasFillPointerP_false() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .build();
		Assert.assertThat(struct.arrayHasFillPointerP(), is(NILStruct.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct#arrayHasFillPointerP()} where a fill-pointer exists.
	 */
	@Test
	public void test_arrayHasFillPointerP_true() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .fillPointer(IntegerStruct.TWO)
				              .build();
		Assert.assertThat(struct.arrayHasFillPointerP(), is(TStruct.INSTANCE));
	}

	/*
	Array-Displacement
	 */

	/**
	 * Test for {@link StringStruct#arrayDisplacement()} where the array is not displaced.
	 */
	@Test
	public void test_arrayDisplacement() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();

		final ValuesStruct displacement = struct.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(NILStruct.INSTANCE));
		Assert.assertThat(displacement.getValuesList().get(1), is(IntegerStruct.ZERO));
	}

	/**
	 * Test for {@link StringStruct#arrayDisplacement()} where the array is displaced.
	 */
	@Test
	public void test_arrayDisplacement_Displaced() {
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.TEN)
				              .initialElement(CharacterConstants.DOLLAR_SIGN_CHAR)
				              .build();
		final IntegerStruct displacedIndexOffset = IntegerStruct.TWO;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .displacedIndexOffset(displacedIndexOffset)
				              .build();

		final ValuesStruct displacement = struct.arrayDisplacement();
		Assert.assertThat(displacement.getValuesList().get(0), is(displacedTo));
		Assert.assertThat(displacement.getValuesList().get(1), is(displacedIndexOffset));
	}

	/*
	Array-In-Bounds-P
	 */

	/**
	 * Test for {@link StringStruct#arrayInBoundsP(IntegerStruct...)} where the wrong number of subscripts were
	 * provided.
	 */
	@Test
	public void test_arrayInBoundsP_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		struct.arrayInBoundsP();
	}

	/**
	 * Test for {@link StringStruct#arrayInBoundsP(IntegerStruct...)} where the subscripts are not in bound.
	 */
	@Test
	public void test_arrayInBoundsP_false() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.arrayInBoundsP(IntegerStruct.TEN), is(NILStruct.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct#arrayInBoundsP(IntegerStruct...)} where the subscripts are in bound.
	 */
	@Test
	public void test_arrayInBoundsP_true() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.arrayInBoundsP(IntegerStruct.ZERO), is(TStruct.INSTANCE));
	}

	/*
	Array-Rank
	 */

	/**
	 * Test for {@link StringStruct#arrayRank()}.
	 */
	@Test
	public void test_arrayRank() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.arrayRank(), is(IntegerStruct.ONE));
	}

	/*
	Array-Row-Major-Index
	 */

	/**
	 * Test for {@link StringStruct#arrayRowMajorIndex(IntegerStruct...)} where the wrong number of subscripts
	 * were provided.
	 */
	@Test
	public void test_arrayRowMajorIndex_WrongNumOfSubscripts() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Wrong number of subscripts"));

		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .build();
		struct.arrayRowMajorIndex();
	}

	/**
	 * Test for {@link StringStruct#arrayRowMajorIndex(IntegerStruct...)} where the index provided was too small.
	 */
	@Test
	public void test_arrayRowMajorIndex_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		struct.arrayRowMajorIndex(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#arrayRowMajorIndex(IntegerStruct...)} where the index provided was too large.
	 */
	@Test
	public void test_arrayRowMajorIndex_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .build();
		struct.arrayRowMajorIndex(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#arrayRowMajorIndex(IntegerStruct...)}.
	 */
	@Test
	public void test_arrayRowMajorIndex() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .build();
		Assert.assertThat(struct.arrayRowMajorIndex(IntegerStruct.TWO), is(IntegerStruct.TWO));
	}

	/*
	Array-Total-Size
	 */

	/**
	 * Test for {@link StringStruct#arrayTotalSize()}.
	 */
	@Test
	public void test_arrayTotalSize() {
		final IntegerStruct size = IntegerStruct.TEN;
		final StringStruct struct
				= StringStruct.builder(size)
				              .build();
		Assert.assertThat(struct.arrayTotalSize(), is(size));
	}

	/*
	Row-Major-Aref
	 */

	/**
	 * Test for {@link StringStruct#rowMajorAref(IntegerStruct)} where the index provided was too small.
	 */
	@Test
	public void test_rowMajorAref_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.rowMajorAref(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#rowMajorAref(IntegerStruct)} where the index provided was too large.
	 */
	@Test
	public void test_rowMajorAref_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.rowMajorAref(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#rowMajorAref(IntegerStruct)} where the total size is greater than the contents.
	 */
	@Test
	public void test_rowMajorAref_TotalSizeGreaterThanContents() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		struct.adjustArray(AdjustArrayContext.builder(IntegerStruct.TWO)
		                                     .build());
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ONE), is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link StringStruct#rowMajorAref(IntegerStruct)}.
	 */
	@Test
	public void test_rowMajorAref() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#rowMajorAref(IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_rowMajorAref_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));
	}

	/*
	Setf-Row-Major-Aref
	 */

	/**
	 * Test for {@link StringStruct#setfRowMajorAref(LispStruct, IntegerStruct)} where the provided new element is
	 * not a {@link CharacterStruct}.
	 */
	@Test
	public void test_setfRowMajorAref_NewElementNotCharacterStruct() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a character type."));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		struct.setfRowMajorAref(newElement, IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#setfRowMajorAref(LispStruct, IntegerStruct)} where the index provided was too
	 * small.
	 */
	@Test
	public void test_setfRowMajorAref_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfRowMajorAref(newElement, IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#setfRowMajorAref(LispStruct, IntegerStruct)} where the index provided was too
	 * large.
	 */
	@Test
	public void test_setfRowMajorAref_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfRowMajorAref(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#setfRowMajorAref(LispStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfRowMajorAref() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		final LispStruct result = struct.setfRowMajorAref(newElement, IntegerStruct.ZERO);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#setfRowMajorAref(LispStruct, IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_setfRowMajorAref_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfRowMajorAref(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.rowMajorAref(IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(displacedTo.rowMajorAref(IntegerStruct.ZERO), is(newElement));
	}
}
