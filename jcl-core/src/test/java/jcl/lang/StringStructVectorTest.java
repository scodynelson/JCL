package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.CharacterConstants;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.is;

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
		thrown.expectMessage("VECTOR has no fill-pointer to retrieve.");

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
		thrown.expectMessage("value is out of bounds for VECTOR with size");

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
		thrown.expectMessage("value is out of bounds for VECTOR with size");

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

	@Test
	public void test_vectorPop() {
	}

	/*
	Vector-Push
	 */

	@Test
	public void test_vectorPush() {
	}

	/*
	Vector-Push-Extend
	 */

	@Test
	public void test_vectorPushExtend() {
	}
}
