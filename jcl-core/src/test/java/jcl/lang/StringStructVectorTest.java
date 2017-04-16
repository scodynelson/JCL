package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.CharacterConstants;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.sameInstance;

/**
 * Unit tests for {@link StringStruct} vector methods.
 */
public class StringStructVectorTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Svref
	 */

	/**
	 * Test for {@link StringStruct#svref(IntegerStruct)}.
	 */
	@Test
	public void test_svref() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage("is not of the expected type");

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.svref(IntegerStruct.ZERO);
	}

	/*
	Setf-Svref
	 */

	/**
	 * Test for {@link StringStruct#setfSvref(LispStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfSvref() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage("is not of the expected type");

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.setfSvref(CharacterConstants.DOLLAR_SIGN_CHAR, IntegerStruct.ZERO);
	}

	/*
	Fill-Pointer
	 */

	/**
	 * Test for {@link StringStruct#fillPointer()}.
	 */
	@Test
	public void test_fillPointer() {
		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();
		final IntegerStruct result = struct.fillPointer();
		Assert.assertThat(result, is(fillPointer));
	}

	/**
	 * Test for {@link StringStruct#fillPointer()} where there is no fill-pointer value.
	 */
	@Test
	public void test_fillPointer_NoFillPointer() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage("STRING has no fill-pointer to retrieve.");

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.fillPointer();
	}

	/*
	Setf-Fill-Pointer
	 */

	/**
	 * Test for {@link StringStruct#setfFillPointer(IntegerStruct)}.
	 */
	@Test
	public void test_setfFillPointer() {
		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();

		final IntegerStruct newFillPointer = IntegerStruct.ONE;
		final IntegerStruct result = struct.setfFillPointer(newFillPointer);
		Assert.assertThat(result, is(newFillPointer));
	}

	/**
	 * Test for {@link StringStruct#setfFillPointer(IntegerStruct)} where the provided fill-pointer value is too small.
	 */
	@Test
	public void test_setfFillPointer_OutOfBounds_TooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("value is out of bounds for STRING with size");

		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();

		final IntegerStruct newFillPointer = IntegerStruct.MINUS_ONE;
		struct.setfFillPointer(newFillPointer);
	}

	/**
	 * Test for {@link StringStruct#setfFillPointer(IntegerStruct)} where the provided fill-pointer value is too large.
	 */
	@Test
	public void test_setfFillPointer_OutOfBounds_TooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("value is out of bounds for STRING with size");

		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();

		final IntegerStruct newFillPointer = IntegerStruct.TEN;
		struct.setfFillPointer(newFillPointer);
	}

	/*
	Vector-Pop
	 */

	/**
	 * Test for {@link StringStruct#vectorPop()} where the string has no fill-pointer.
	 */
	@Test
	public void test_vectorPop_NoFillPointer() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Cannot pop from a STRING with no fill-pointer."));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.vectorPop();
	}

	/**
	 * Test for {@link StringStruct#vectorPop()} where the string has a fill-pointer with the value '0'.
	 */
	@Test
	public void test_vectorPop_ZeroFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Nothing left to pop."));

		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.ZERO)
		                                        .build();
		struct.vectorPop();
	}

	/**
	 * Test for {@link StringStruct#vectorPop()} where the string is displaced.
	 */
	@Test
	public void test_vectorPop_Displaced() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final LispStruct result = struct.vectorPop();
		Assert.assertThat(result, is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(struct.toJavaString(true), is(str));
	}

	/**
	 * Test for {@link StringStruct#vectorPop()} where the string is not displaced.
	 */
	@Test
	public void test_vectorPop_NotDisplaced() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final LispStruct result = struct.vectorPop();
		Assert.assertThat(result, is(CharacterConstants.LATIN_SMALL_LETTER_B_CHAR));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.ONE));
		Assert.assertThat(struct.toJavaString(true), is(str));
	}

	/*
	Vector-Push
	 */

	/**
	 * Test for {@link StringStruct#vectorPush(LispStruct)} where the new element provided is not a {@link
	 * CharacterStruct}.
	 */
	@Test
	public void test_vectorPush_NonCharacterElement() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a character type."));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.vectorPush(IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#vectorPush(LispStruct)} where the string has no fill-pointer.
	 */
	@Test
	public void test_vectorPush_NoFillPointer() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Cannot push into a STRING with no fill-pointer."));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.vectorPush(CharacterConstants.DOLLAR_SIGN_CHAR);
	}

	/**
	 * Test for {@link StringStruct#vectorPush(LispStruct)} where the string has a fill-pointer that is the same as its
	 * size.
	 */
	@Test
	public void test_vectorPush_FillPointerIsTotalSize() {
		final String str = "abc";
		final IntegerStruct size = IntegerStructImpl.valueOf(str.length());

		final StringStruct struct = StringStruct.builder(size)
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(size)
		                                        .build();
		final LispStruct result = struct.vectorPush(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
		Assert.assertThat(struct.fillPointer(), is(size));
		Assert.assertThat(struct.toJavaString(true), is("abc"));
	}

	/**
	 * Test for {@link StringStruct#vectorPush(LispStruct)} where the string is displaced.
	 */
	@Test
	public void test_vectorPush_Displaced() {
		final String str = "abc";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		final IntegerStruct fillPointer = IntegerStruct.ONE;

		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .displacedTo(displacedTo)
		                                        .fillPointer(fillPointer)
		                                        .build();
		final LispStruct result = struct.vectorPush(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(fillPointer));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.TWO));
		Assert.assertThat(struct.toJavaString(true), is("a$c"));
		Assert.assertThat(struct.arrayDisplacement().getPrimaryValue(), sameInstance(displacedTo));
	}

	/**
	 * Test for {@link StringStruct#vectorPush(LispStruct)} where the string is not displaced.
	 */
	@Test
	public void test_vectorPush_NotDisplaced() {
		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.ONE;

		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();
		final LispStruct result = struct.vectorPush(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(fillPointer));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.TWO));
		Assert.assertThat(struct.toJavaString(true), is("a$c"));
	}

	/*
	Vector-Push-Extend
	 */

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the new element provided is not a {@link
	 * CharacterStruct}.
	 */
	@Test
	public void test_vectorPushExtend_NonCharacterElement() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a character type."));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.vectorPushExtend(IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string has no fill-pointer.
	 */
	@Test
	public void test_vectorPushExtend_NoFillPointer() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Cannot push into a STRING with no fill-pointer."));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string has a fill-pointer that is the same
	 * as its size, but is not adjustable.
	 */
	@Test
	public void test_vectorPushExtend_Extension_NotAdjustable() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("VECTOR would be extended and is not adjustable."));

		final String str = "abc";
		final IntegerStruct size = IntegerStructImpl.valueOf(str.length());
		final StringStruct struct = StringStruct.builder(size)
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(size)
		                                        .build();
		struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string has a fill-pointer that is the same
	 * as its size, is adjustable and displaced.
	 */
	@Test
	public void test_vectorPushExtend_Extension_Displaced() {
		final String str = "abc";
		final IntegerStruct size = IntegerStructImpl.valueOf(str.length());
		final StringStruct displacedTo = StringStruct.toLispString(str);

		final StringStruct struct = StringStruct.builder(size)
		                                        .displacedTo(displacedTo)
		                                        .fillPointer(size)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();
		final LispStruct result = struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(size));
		Assert.assertThat(struct.fillPointer(), is(size.add(IntegerStruct.ONE)));
		Assert.assertThat(struct.toJavaString(true), is("abc$"));
		Assert.assertThat(struct.arrayDisplacement().getPrimaryValue(), not(sameInstance(displacedTo)));
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string has a fill-pointer that is the same
	 * as its size, is adjustable and not displaced.
	 */
	@Test
	public void test_vectorPushExtend_Extension_NotDisplaced() {
		final String str = "abc";
		final IntegerStruct size = IntegerStructImpl.valueOf(str.length());

		final StringStruct struct = StringStruct.builder(size)
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(size)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();
		final LispStruct result = struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(size));
		Assert.assertThat(struct.fillPointer(), is(size.add(IntegerStruct.ONE)));
		Assert.assertThat(struct.toJavaString(true), is("abc$"));
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string is displaced.
	 */
	@Test
	public void test_vectorPushExtend_Displaced() {
		final String str = "abc";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		final IntegerStruct fillPointer = IntegerStruct.ONE;

		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .displacedTo(displacedTo)
		                                        .fillPointer(fillPointer)
		                                        .build();
		final LispStruct result = struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(fillPointer));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.TWO));
		Assert.assertThat(struct.toJavaString(true), is("a$c"));
		Assert.assertThat(struct.arrayDisplacement().getPrimaryValue(), sameInstance(displacedTo));
	}

	/**
	 * Test for {@link StringStruct#vectorPushExtend(LispStruct)} where the string is not displaced.
	 */
	@Test
	public void test_vectorPushExtend_NotDisplaced() {
		final String str = "abc";
		final IntegerStruct fillPointer = IntegerStruct.ONE;

		final StringStruct struct = StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(fillPointer)
		                                        .build();
		final LispStruct result = struct.vectorPushExtend(CharacterConstants.DOLLAR_SIGN_CHAR);
		Assert.assertThat(result, is(fillPointer));
		Assert.assertThat(struct.fillPointer(), is(IntegerStruct.TWO));
		Assert.assertThat(struct.toJavaString(true), is("a$c"));
	}
}
