package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.CharacterConstants;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.is;

/**
 * Unit tests for {@link CharacterStruct} character equality methods.
 */
public class CharacterStructCharacterEqualityTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Char=
	 */

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isEqualTo_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct)} where the characters are not equal.
	 */
	@Test
	public void test_isEqualTo_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isEqualTo_FalseByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isEqualTo_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isEqualTo();
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isEqualTo_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct...)} where the characters are not equal.
	 */
	@Test
	public void test_isEqualTo_Static_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualTo(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isEqualTo_Static_FalseByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char/=
	 */

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct)} where the characters are not equal.
	 */
	@Test
	public void test_isNotEqualTo_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isNotEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isNotEqualTo_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isNotEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct)} where the characters are not equal by case.
	 */
	@Test
	public void test_isNotEqualTo_FalseByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isNotEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isNotEqualTo_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isNotEqualTo();
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} where the characters are not equal.
	 */
	@Test
	public void test_isNotEqualTo_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isNotEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isNotEqualTo_Static_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isNotEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} where the characters are not equal by case.
	 */
	@Test
	public void test_isNotEqualTo_Static_TrueByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.DOLLAR_SIGN_CHAR;
		final boolean result = CharacterStruct.isNotEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/*
	Char<
	 */

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isLessThan_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThan(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isLessThan_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isLessThan_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThan_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isLessThan(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct)} where the characters are greater by case.
	 */
	@Test
	public void test_isLessThan_False_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isLessThan_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isLessThan();
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isLessThan_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isLessThan_Static_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isLessThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isLessThan_Static_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThan_Static_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThan(CharacterStruct...)} where the characters are greater by case.
	 */
	@Test
	public void test_isLessThan_Static_False_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char>
	 */

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThan_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThan(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThan_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThan_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isGreaterThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isGreaterThan_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThan(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct)} where the characters are less by case.
	 */
	@Test
	public void test_isGreaterThan_False_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThan(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isGreaterThan_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isGreaterThan();
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThan_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThan_Static_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThan_Static_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isGreaterThan_Static_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThan(CharacterStruct...)} where the characters are less by case.
	 */
	@Test
	public void test_isGreaterThan_Static_False_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThan(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char<=
	 */

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isLessThanOrEqualTo_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isLessThanOrEqualTo_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isLessThanOrEqualTo_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThanOrEqualTo_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct)} where the characters are greater by case.
	 */
	@Test
	public void test_isLessThanOrEqualTo_False_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isLessThanOrEqualTo();
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} where the characters are greater by
	 * case.
	 */
	@Test
	public void test_isLessThanOrEqualTo_Static_False_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char>=
	 */

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isGreaterThanOrEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualTo(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct)} where the characters are less by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_False_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualTo(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isGreaterThanOrEqualTo();
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where the characters are equal by
	 * case.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualTo(CharacterStruct...)} where the characters are less by
	 * case.
	 */
	@Test
	public void test_isGreaterThanOrEqualTo_Static_False_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualTo(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char-Equal
	 */

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isEqualToIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct)} where the characters are not equal.
	 */
	@Test
	public void test_isEqualToIgnoreCase_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isEqualToIgnoreCase_TrueByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isEqualToIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isEqualToIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isEqualToIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} where the characters are not equal.
	 */
	@Test
	public void test_isEqualToIgnoreCase_Static_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isEqualToIgnoreCase_Static_TrueByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/*
	Char-Not-Equal
	 */

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct)} where the characters are not equal.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isNotEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isNotEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct)} where the characters are not equal by
	 * case.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_FalseByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isNotEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isNotEqualToIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} where the characters are not equal.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isNotEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_Static_False() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isNotEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} where the characters are not equal by
	 * case.
	 */
	@Test
	public void test_isNotEqualToIgnoreCase_Static_FalseByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.DOLLAR_SIGN_CHAR;
		final boolean result = CharacterStruct.isNotEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/*
	Char-Lessp
	 */

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isLessThanIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThanIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isLessThanIgnoreCase_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isLessThanIgnoreCase_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThanIgnoreCase_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct)} where the characters are greater by case.
	 */
	@Test
	public void test_isLessThanIgnoreCase_True_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThanIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isLessThanIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isLessThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where the characters are equal by case.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} where the characters are greater by
	 * case.
	 */
	@Test
	public void test_isLessThanIgnoreCase_Static_True_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/*
	Char-Greaterp
	 */

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isGreaterThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct)} where the characters are equal by case.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct)} where the characters are less by case.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_True_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where no characters were provided.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isGreaterThanIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where the characters are greater.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where the characters are equal.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_False_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where the characters are equal by
	 * case.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} where the characters are less by
	 * case.
	 */
	@Test
	public void test_isGreaterThanIgnoreCase_Static_True_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/*
	Char-Not-GreaterP
	 */

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isLessThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are equal.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are greater.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are equal by
	 * case.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are greater
	 * by case.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_True_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isLessThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where no characters were
	 * provided.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isLessThanOrEqualToIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are less.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * equal.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * greater.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_False_Greater() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are equal
	 * by case.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * greater by case.
	 */
	@Test
	public void test_isLessThanOrEqualToIgnoreCase_Static_True_GreaterByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final boolean result = CharacterStruct.isLessThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/*
	Char-Not-Lessp
	 */

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are
	 * greater.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are
	 * equal.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are less.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = char1.isGreaterThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are equal
	 * by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_True_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct)} where the characters are less
	 * by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_True_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = char1.isGreaterThanOrEqualToIgnoreCase(char2);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where no characters were
	 * provided.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_TooFewArguments() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage("At least one character required to test equality.");

		CharacterStruct.isGreaterThanOrEqualToIgnoreCase();
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * greater.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_True() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * equal.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_True_Equal() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * less.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_False_Less() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * equal by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_False_EqualByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_SMALL_LETTER_B_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} where the characters are
	 * less by case.
	 */
	@Test
	public void test_isGreaterThanOrEqualToIgnoreCase_Static_True_LessByCase() {
		final CharacterStruct char1 = CharacterConstants.LATIN_CAPITAL_LETTER_C_CHAR;
		final CharacterStruct char2 = CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR;
		final CharacterStruct char3 = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final boolean result = CharacterStruct.isGreaterThanOrEqualToIgnoreCase(char1, char2, char3).toJavaPBoolean();
		Assert.assertThat(result, is(true));
	}
}