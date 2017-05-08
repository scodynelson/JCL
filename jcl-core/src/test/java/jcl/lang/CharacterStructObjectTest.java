package jcl.lang;

import jcl.lang.statics.CharacterConstants;
import org.junit.Assert;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;

/**
 * Unit tests for {@link CharacterStruct} object methods.
 */
public class CharacterStructObjectTest {

	/*
	Eq
	 */

	/**
	 * Test for {@link CharacterStruct#eq(LispStruct)}.
	 */
	@Test
	public void test_eq_true() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eq(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#eq(LispStruct)} where the characters differ by case.
	 */
	@Test
	public void test_eq_false_character_by_case() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eq(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#eq(LispStruct)} where the characters are different.
	 */
	@Test
	public void test_eq_false_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eq(CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#eq(LispStruct)} where the object of comparison is not a {@link CharacterStruct}.
	 */
	@Test
	public void test_eq_false_non_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eq(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Eql
	 */

	/**
	 * Test for {@link CharacterStruct#eql(LispStruct)}.
	 */
	@Test
	public void test_eql_true() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eql(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#eql(LispStruct)} where the characters differ by case.
	 */
	@Test
	public void test_eql_false_character_by_case() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eql(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#eql(LispStruct)} where the characters are different.
	 */
	@Test
	public void test_eql_false_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eql(CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#eql(LispStruct)} where the object of comparison is not a {@link CharacterStruct}.
	 */
	@Test
	public void test_eql_false_non_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.eql(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Equal
	 */

	/**
	 * Test for {@link CharacterStruct#equal(LispStruct)}.
	 */
	@Test
	public void test_equal_true() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equal(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#equal(LispStruct)} where the characters differ by case.
	 */
	@Test
	public void test_equal_false_character_by_case() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equal(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#equal(LispStruct)} where the characters are different.
	 */
	@Test
	public void test_equal_false_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equal(CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#equal(LispStruct)} where the object of comparison is not a {@link
	 * CharacterStruct}.
	 */
	@Test
	public void test_equal_false_non_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equal(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Equalp
	 */

	/**
	 * Test for {@link CharacterStruct#equalp(LispStruct)}.
	 */
	@Test
	public void test_equalp_true() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equalp(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#equalp(LispStruct)} where the characters differ by case.
	 */
	@Test
	public void test_equalp_true_character_by_case() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equalp(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link CharacterStruct#equalp(LispStruct)} where the characters are different.
	 */
	@Test
	public void test_equalp_false_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equalp(CharacterConstants.LATIN_CAPITAL_LETTER_B_CHAR);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link CharacterStruct#equalp(LispStruct)} where the object of comparison is not a {@link
	 * CharacterStruct}.
	 */
	@Test
	public void test_equalp_false_non_character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final boolean result = character.equalp(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}
}
