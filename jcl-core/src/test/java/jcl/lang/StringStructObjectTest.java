package jcl.lang;

import jcl.lang.internal.NILArrayStructImpl;
import jcl.type.CharacterType;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.is;

/**
 * Unit tests for {@link StringStruct} object methods.
 */
public class StringStructObjectTest {

	/*
	Eq
	 */

	/**
	 * Test for {@link StringStruct#eq(LispStruct)}.
	 */
	@Test
	public void test_eq_true() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eq(string);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#eq(LispStruct)} where the strings are not the same.
	 */
	@Test
	public void test_eq_false_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eq(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#eq(LispStruct)} where the object of comparison is not a {@link StringStruct}.
	 */
	@Test
	public void test_eq_false_non_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eq(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Eql
	 */

	/**
	 * Test for {@link StringStruct#eql(LispStruct)}.
	 */
	@Test
	public void test_eql_true() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eql(string);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#eql(LispStruct)} where the strings are not the same.
	 */
	@Test
	public void test_eql_false_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eql(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#eql(LispStruct)} where the object of comparison is not a {@link StringStruct}.
	 */
	@Test
	public void test_eql_false_non_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.eql(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Equal
	 */

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where both strings are the same instance.
	 */
	@Test
	public void test_equal_true_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(string);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)}.
	 */
	@Test
	public void test_equal_true_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the strings differ by length and the second string is
	 * shorter than the first.
	 */
	@Test
	public void test_equal_false_string_by_length_less() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("abcd"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the strings differ by length and the second string is
	 * longer than the first.
	 */
	@Test
	public void test_equal_false_string_by_length_greater() {
		final StringStruct string = StringStruct.toLispString("abcd");
		final boolean result = string.equal(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the strings differ by case.
	 */
	@Test
	public void test_equal_false_string_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("ABC"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the first character is different.
	 */
	@Test
	public void test_equal_false_string_first() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("dbc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where an inner character is different.
	 */
	@Test
	public void test_equal_false_string_inner() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("adc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the last character is different.
	 */
	@Test
	public void test_equal_false_string_last() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(StringStruct.toLispString("abd"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with the same contents.
	 */
	@Test
	@Ignore
	public void test_equal_true_nil_array_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(NILArrayStructImpl.valueOf(CharacterType.INSTANCE, string));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with a equal contents.
	 */
	@Test
	@Ignore
	public void test_equal_true_nil_array_equal() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("abc")));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with differing contents by case.
	 */
	@Test
	@Ignore
	public void test_equal_false_nil_array_equal_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("ABC")));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with a differing contents.
	 */
	@Test
	@Ignore
	public void test_equal_false_nil_array() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("def")));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link VectorStruct}.
	 */
	@Test
	public void test_equal_false_vector_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final VectorStruct vector = VectorStruct.builder(string.length())
		                                        .initialContents(string)
		                                        .build();
		final boolean result = string.equal(vector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link VectorStruct} with a
	 * equal contents.
	 */
	@Test
	public void test_equal_false_vector_equal() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("abc");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equal(vector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link VectorStruct} with
	 * differing contents by case.
	 */
	@Test
	public void test_equal_false_vector_equal_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("ABC");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equal(vector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is a {@link VectorStruct} with a
	 * differing contents.
	 */
	@Test
	public void test_equal_false_vector() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("def");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equal(vector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equal(LispStruct)} where the object of comparison is not a {@link StringStruct}.
	 */
	@Test
	public void test_equal_false_non_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equal(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}

	/*
	Equalp
	 */

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where both strings are the same instance.
	 */
	@Test
	public void test_equalp_true_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(string);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)}.
	 */
	@Test
	public void test_equalp_true_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the strings differ by length and the second string is
	 * shorter than the first.
	 */
	@Test
	public void test_equalp_false_string_by_length_less() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("abcd"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the strings differ by length and the second string is
	 * longer than the first.
	 */
	@Test
	public void test_equalp_false_string_by_length_greater() {
		final StringStruct string = StringStruct.toLispString("abcd");
		final boolean result = string.equalp(StringStruct.toLispString("abc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the strings differ by case.
	 */
	@Test
	public void test_equalp_true_string_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("ABC"));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the first character is different.
	 */
	@Test
	public void test_equalp_false_string_first() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("dbc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where an inner character is different.
	 */
	@Test
	public void test_equalp_false_string_inner() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("adc"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the last character is different.
	 */
	@Test
	public void test_equalp_false_string_last() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(StringStruct.toLispString("abd"));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with the same contents.
	 */
	@Test
	@Ignore
	public void test_equalp_true_nil_array_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(NILArrayStructImpl.valueOf(CharacterType.INSTANCE, string));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with a equal contents.
	 */
	@Test
	@Ignore
	public void test_equalp_true_nil_array_equal() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("abc")));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with differing contents by case.
	 */
	@Test
	@Ignore
	public void test_equalp_true_nil_array_equal_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("ABC")));
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link NILArrayStructImpl}
	 * with a differing contents.
	 */
	@Test
	@Ignore
	public void test_equalp_false_nil_array() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(
				NILArrayStructImpl.valueOf(CharacterType.INSTANCE, StringStruct.toLispString("def")));
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link BitVectorStruct}.
	 */
	@Test
	public void test_equalp_false_bit_vector() {
		final StringStruct string = StringStruct.toLispString("101");
		final BitVectorStruct bitVector = BitVectorStruct.toLispBitVector("101");
		final boolean result = string.equalp(bitVector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link VectorStruct}.
	 */
	@Test
	public void test_equalp_true_vector_eq() {
		final StringStruct string = StringStruct.toLispString("abc");
		final VectorStruct vector = VectorStruct.builder(string.length())
		                                        .initialContents(string)
		                                        .build();
		final boolean result = string.equalp(vector);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link VectorStruct} with a
	 * equal contents.
	 */
	@Test
	public void test_equalp_true_vector_equal() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("abc");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equalp(vector);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link VectorStruct} with
	 * differing contents by case.
	 */
	@Test
	public void test_equalp_true_vector_equal_by_case() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("ABC");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equalp(vector);
		Assert.assertThat(result, is(true));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is a {@link VectorStruct} with a
	 * differing contents.
	 */
	@Test
	public void test_equalp_false_vector() {
		final StringStruct string = StringStruct.toLispString("abc");
		final StringStruct initialContents = StringStruct.toLispString("def");
		final VectorStruct vector = VectorStruct.builder(initialContents.length())
		                                        .initialContents(initialContents)
		                                        .build();
		final boolean result = string.equalp(vector);
		Assert.assertThat(result, is(false));
	}

	/**
	 * Test for {@link StringStruct#equalp(LispStruct)} where the object of comparison is not a {@link StringStruct}.
	 */
	@Test
	public void test_equalp_false_non_string() {
		final StringStruct string = StringStruct.toLispString("abc");
		final boolean result = string.equalp(IntegerStruct.ZERO);
		Assert.assertThat(result, is(false));
	}
}
