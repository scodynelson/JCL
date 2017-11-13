package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CharacterConstants;
import jcl.type.BaseCharType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.IntegerType;
import jcl.type.StandardCharType;
import org.apache.commons.lang3.text.WordUtils;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;

/**
 * Unit tests for {@link StringStruct} string methods.
 */
public class StringStructStringTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Char
	 */

	/**
	 * Test for {@link StringStruct#char_(IntegerStruct)} where the index provided was too small.
	 */
	@Test
	public void test_char_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.char_(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#char_(IntegerStruct)} where the index provided was too large.
	 */
	@Test
	public void test_char_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.char_(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#char_(IntegerStruct)} where the total size is greater than the contents.
	 */
	@Test
	public void test_char_TotalSizeGreaterThanContents() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(BooleanStruct.T)
				              .build();
		struct.adjustArray(AdjustArrayContext.builder(IntegerStruct.TWO)
		                                     .build());
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));
		Assert.assertThat(struct.char_(IntegerStruct.ONE), is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link StringStruct#char_(IntegerStruct)}.
	 */
	@Test
	public void test_char() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#char_(IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_char_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));
	}

	/*
	Setf-Char
	 */

	/**
	 * Test for {@link StringStruct#setfChar(CharacterStruct, IntegerStruct)} where the index provided was too
	 * small.
	 */
	@Test
	public void test_setfChar_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfChar(newElement, IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#setfChar(CharacterStruct, IntegerStruct)} where the index provided was too
	 * large.
	 */
	@Test
	public void test_setfChar_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfChar(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#setfChar(CharacterStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfChar() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfChar(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#setfChar(CharacterStruct, IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_setfChar_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfChar(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(displacedTo.char_(IntegerStruct.ZERO), is(newElement));
	}

	/*
	SChar
	 */

	/**
	 * Test for {@link StringStruct#schar(IntegerStruct)}.
	 */
	@Test
	public void test_schar_NotSimpleType() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not of the expected type"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(BooleanStruct.T)
				              .build();
		Assert.assertThat(struct.schar(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#schar(IntegerStruct)} where the index provided was too small.
	 */
	@Test
	public void test_schar_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.schar(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#schar(IntegerStruct)} where the index provided was too large.
	 */
	@Test
	public void test_schar_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.schar(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#schar(IntegerStruct)}.
	 */
	@Test
	public void test_schar() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.schar(IntegerStruct.ZERO), is(initialElement));
	}

	/*
	Setf-SChar
	 */

	/**
	 * Test for {@link StringStruct#setfSchar(CharacterStruct, IntegerStruct)} where the index provided was too
	 * small.
	 */
	@Test
	public void test_setfSchar_NotSimpleType() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not of the expected type"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(BooleanStruct.T)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfSchar(newElement, IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#setfSchar(CharacterStruct, IntegerStruct)} where the index provided was too
	 * small.
	 */
	@Test
	public void test_setfSchar_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfSchar(newElement, IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#setfSchar(CharacterStruct, IntegerStruct)} where the index provided was too
	 * large.
	 */
	@Test
	public void test_setfSchar_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.char_(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfSchar(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#setfSchar(CharacterStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfSchar() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.schar(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfSchar(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.schar(IntegerStruct.ZERO), is(newElement));
	}

	/*
	String-Upcase
	 */

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the start index was
	 * negative.
	 */
	@Test
	public void test_stringUpcase_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .start(IntegerStruct.MINUS_ONE)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the total size.
	 */
	@Test
	public void test_stringUpcase_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .start(IntegerStruct.ONE)
		                                           .build());

	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_stringUpcase_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .start(IntegerStruct.TWO)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_stringUpcase_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .end(IntegerStruct.MINUS_ONE)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_stringUpcase_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .end(IntegerStruct.ONE)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_stringUpcase_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .end(IntegerStruct.TWO)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the end index.
	 */
	@Test
	public void test_stringUpcase_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.stringUpcase(StringIntervalOpContext.builder()
		                                           .start(IntegerStruct.ONE)
		                                           .end(IntegerStruct.ZERO)
		                                           .build());
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)}.
	 */
	@Test
	public void test_stringUpcase_NoStartAndNoEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .build());
		Assert.assertThat(result.toJavaString(), is(str.toUpperCase()));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringUpcase_StartAndNoEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .start(IntegerStruct.ONE)
		                                                                       .build());
		Assert.assertThat(result.toJavaString(), is("aBC"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringUpcase_NoStartAndEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .end(IntegerStruct.TWO)
		                                                                       .build());
		Assert.assertThat(result.toJavaString(), is("ABc"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringUpcase_StartAndEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .start(IntegerStruct.ONE)
		                                                                       .end(IntegerStruct.TWO)
		                                                                       .build());
		Assert.assertThat(result.toJavaString(), is("aBc"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the string has a fill
	 * pointer.
	 */
	@Test
	public void test_stringUpcase_FillPointer() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .build());
		Assert.assertThat(result.toJavaString(true), is("AB"));
		Assert.assertThat(result.toJavaString(false), is("AB"));
		Assert.assertThat(struct.toJavaString(true), is(str));
		Assert.assertThat(struct.toJavaString(false), is("ab"));
	}

	/**
	 * Test for {@link StringStruct#stringUpcase(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_stringUpcase_Displaced() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.stringUpcase(StringIntervalOpContext.builder()
		                                                                       .build());
		Assert.assertThat(result.toJavaString(), is("BC"));
		Assert.assertThat(struct.toJavaString(), is("bc"));
	}

	/*
	String-Downcase
	 */

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the start index was
	 * negative.
	 */
	@Test
	public void test_stringDowncase_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .start(IntegerStruct.MINUS_ONE)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the total size.
	 */
	@Test
	public void test_stringDowncase_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .start(IntegerStruct.ONE)
		                                             .build());

	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_stringDowncase_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .start(IntegerStruct.TWO)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_stringDowncase_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .end(IntegerStruct.MINUS_ONE)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_stringDowncase_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .end(IntegerStruct.ONE)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_stringDowncase_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .end(IntegerStruct.TWO)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the end index.
	 */
	@Test
	public void test_stringDowncase_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("ABC");
		struct.stringDowncase(StringIntervalOpContext.builder()
		                                             .start(IntegerStruct.ONE)
		                                             .end(IntegerStruct.ZERO)
		                                             .build());
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)}.
	 */
	@Test
	public void test_stringDowncase_NoStartAndNoEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .build());
		Assert.assertThat(result.toJavaString(), is(str.toLowerCase()));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringDowncase_StartAndNoEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .start(IntegerStruct.ONE)
		                                                                         .build());
		Assert.assertThat(result.toJavaString(), is("Abc"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringDowncase_NoStartAndEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .end(IntegerStruct.TWO)
		                                                                         .build());
		Assert.assertThat(result.toJavaString(), is("abC"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringDowncase_StartAndEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .start(IntegerStruct.ONE)
		                                                                         .end(IntegerStruct.TWO)
		                                                                         .build());
		Assert.assertThat(result.toJavaString(), is("AbC"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the string has a
	 * fill pointer.
	 */
	@Test
	public void test_stringDowncase_FillPointer() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .build());
		Assert.assertThat(result.toJavaString(true), is("ab"));
		Assert.assertThat(result.toJavaString(false), is("ab"));
		Assert.assertThat(struct.toJavaString(true), is(str));
		Assert.assertThat(struct.toJavaString(false), is("AB"));
	}

	/**
	 * Test for {@link StringStruct#stringDowncase(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_stringDowncase_Displaced() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.stringDowncase(StringIntervalOpContext.builder()
		                                                                         .build());
		Assert.assertThat(result.toJavaString(), is("bc"));
		Assert.assertThat(struct.toJavaString(), is("BC"));
	}

	/*
	String-Capitalize
	 */

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the start index
	 * was negative.
	 */
	@Test
	public void test_stringCapitalize_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .start(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the total size.
	 */
	@Test
	public void test_stringCapitalize_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .start(IntegerStruct.ONE)
		                                               .build());

	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the fill pointer.
	 */
	@Test
	public void test_stringCapitalize_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .start(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_stringCapitalize_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .end(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_stringCapitalize_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .end(IntegerStruct.ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_stringCapitalize_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .end(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the end index.
	 */
	@Test
	public void test_stringCapitalize_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("abc def");
		struct.stringCapitalize(StringIntervalOpContext.builder()
		                                               .start(IntegerStruct.ONE)
		                                               .end(IntegerStruct.ZERO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)}.
	 */
	@Test
	public void test_stringCapitalize_NoStartAndNoEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .build());
		Assert.assertThat(result.toJavaString(), is(WordUtils.capitalize(str)));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringCapitalize_StartAndNoEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .start(IntegerStruct.ONE)
		                                                                           .build());
		Assert.assertThat(result.toJavaString(), is("aBc Def"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringCapitalize_NoStartAndEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .end(IntegerStruct.TWO)
		                                                                           .build());
		Assert.assertThat(result.toJavaString(), is("Abc def"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where a start index and
	 * an end index were provided.
	 */
	@Test
	public void test_stringCapitalize_StartAndEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .start(IntegerStruct.ONE)
		                                                                           .end(IntegerStruct.TWO)
		                                                                           .build());
		Assert.assertThat(result.toJavaString(), is("aBc def"));
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the string has a
	 * fill pointer.
	 */
	@Test
	public void test_stringCapitalize_FillPointer() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.toLispInteger(4))
		                                        .build();
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .build());
		Assert.assertThat(result.toJavaString(true), is("Abc "));
		Assert.assertThat(result.toJavaString(false), is("Abc "));
		Assert.assertThat(struct.toJavaString(true), is(str));
		Assert.assertThat(struct.toJavaString(false), is("abc "));
	}

	/**
	 * Test for {@link StringStruct#stringCapitalize(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_stringCapitalize_Displaced() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(6))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.stringCapitalize(StringIntervalOpContext.builder()
		                                                                           .build());
		Assert.assertThat(result.toJavaString(), is("Bc Def"));
		Assert.assertThat(struct.toJavaString(), is("bc def"));
	}

	/*
	NString-Upcase
	 */

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the start index was
	 * negative.
	 */
	@Test
	public void test_nStringUpcase_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .start(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the total size.
	 */
	@Test
	public void test_nStringUpcase_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .start(IntegerStruct.ONE)
		                                            .build());

	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_nStringUpcase_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .start(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_nStringUpcase_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .end(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_nStringUpcase_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .end(IntegerStruct.ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_nStringUpcase_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .end(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the start index was
	 * more than the end index.
	 */
	@Test
	public void test_nStringUpcase_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("abc");
		struct.nStringUpcase(StringIntervalOpContext.builder()
		                                            .start(IntegerStruct.ONE)
		                                            .end(IntegerStruct.ZERO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)}.
	 */
	@Test
	public void test_nStringUpcase_NoStartAndNoEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is(str.toUpperCase()));
		Assert.assertThat(struct.toJavaString(), is(str.toUpperCase()));
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_nStringUpcase_StartAndNoEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .start(IntegerStruct.ONE)
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("aBC"));
		Assert.assertThat(struct.toJavaString(), is("aBC"));
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_nStringUpcase_NoStartAndEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .end(IntegerStruct.TWO)
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("ABc"));
		Assert.assertThat(struct.toJavaString(), is("ABc"));
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_nStringUpcase_StartAndEnd() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .start(IntegerStruct.ONE)
		                                                                        .end(IntegerStruct.TWO)
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("aBc"));
		Assert.assertThat(struct.toJavaString(), is("aBc"));
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the string has a fill
	 * pointer.
	 */
	@Test
	public void test_nStringUpcase_FillPointer() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(true), is("ABc"));
		Assert.assertThat(result.toJavaString(false), is("AB"));
		Assert.assertThat(struct.toJavaString(true), is("ABc"));
		Assert.assertThat(struct.toJavaString(false), is("AB"));
	}

	/**
	 * Test for {@link StringStruct#nStringUpcase(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_nStringUpcase_Displaced() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.nStringUpcase(StringIntervalOpContext.builder()
		                                                                        .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("BC"));
		Assert.assertThat(struct.toJavaString(), is("BC"));
	}

	/*
	NString-Downcase
	 */

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the start index was
	 * negative.
	 */
	@Test
	public void test_nStringDowncase_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .start(IntegerStruct.MINUS_ONE)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the total size.
	 */
	@Test
	public void test_nStringDowncase_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .start(IntegerStruct.ONE)
		                                              .build());

	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_nStringDowncase_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .start(IntegerStruct.TWO)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_nStringDowncase_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .end(IntegerStruct.MINUS_ONE)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_nStringDowncase_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .end(IntegerStruct.ONE)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_nStringDowncase_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .end(IntegerStruct.TWO)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the start index was
	 * more than the end index.
	 */
	@Test
	public void test_nStringDowncase_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("ABC");
		struct.nStringDowncase(StringIntervalOpContext.builder()
		                                              .start(IntegerStruct.ONE)
		                                              .end(IntegerStruct.ZERO)
		                                              .build());
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)}.
	 */
	@Test
	public void test_nStringDowncase_NoStartAndNoEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is(str.toLowerCase()));
		Assert.assertThat(struct.toJavaString(), is(str.toLowerCase()));
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_nStringDowncase_StartAndNoEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .start(IntegerStruct.ONE)
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("Abc"));
		Assert.assertThat(struct.toJavaString(), is("Abc"));
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_nStringDowncase_NoStartAndEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .end(IntegerStruct.TWO)
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("abC"));
		Assert.assertThat(struct.toJavaString(), is("abC"));
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where a start index and
	 * an end index were provided.
	 */
	@Test
	public void test_nStringDowncase_StartAndEnd() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .start(IntegerStruct.ONE)
		                                                                          .end(IntegerStruct.TWO)
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("AbC"));
		Assert.assertThat(struct.toJavaString(), is("AbC"));
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the string has a
	 * fill pointer.
	 */
	@Test
	public void test_nStringDowncase_FillPointer() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(true), is("abC"));
		Assert.assertThat(result.toJavaString(false), is("ab"));
		Assert.assertThat(struct.toJavaString(true), is("abC"));
		Assert.assertThat(struct.toJavaString(false), is("ab"));
	}

	/**
	 * Test for {@link StringStruct#nStringDowncase(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_nStringDowncase_Displaced() {
		final String str = "ABC";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.nStringDowncase(StringIntervalOpContext.builder()
		                                                                          .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("bc"));
		Assert.assertThat(struct.toJavaString(), is("bc"));
	}

	/*
	NString-Capitalize
	 */

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the start index
	 * was negative.
	 */
	@Test
	public void test_nStringCapitalize_NegativeStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .start(IntegerStruct.MINUS_ONE)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the total size.
	 */
	@Test
	public void test_nStringCapitalize_StartMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .start(IntegerStruct.ONE)
		                                                .build());

	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the fill pointer.
	 */
	@Test
	public void test_nStringCapitalize_StartMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .start(IntegerStruct.TWO)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the end index was
	 * negative.
	 */
	@Test
	public void test_nStringCapitalize_NegativeEnd() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .end(IntegerStruct.MINUS_ONE)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the end index was
	 * more than the total size.
	 */
	@Test
	public void test_nStringCapitalize_EndMoreThanTotalSize() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.emptyString();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .end(IntegerStruct.ONE)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the end index was
	 * more than the fill pointer.
	 */
	@Test
	public void test_nStringCapitalize_EndMoreThanFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .fillPointer(IntegerStruct.ONE)
		                                        .build();
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .end(IntegerStruct.TWO)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the start index
	 * was more than the end index.
	 */
	@Test
	public void test_nStringCapitalize_EndMoreThanStart() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct = StringStruct.toLispString("abc def");
		struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                .start(IntegerStruct.ONE)
		                                                .end(IntegerStruct.ZERO)
		                                                .build());
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)}.
	 */
	@Test
	public void test_nStringCapitalize_NoStartAndNoEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is(WordUtils.capitalize(str)));
		Assert.assertThat(struct.toJavaString(), is(WordUtils.capitalize(str)));
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_nStringCapitalize_StartAndNoEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .start(IntegerStruct.ONE)
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("aBc Def"));
		Assert.assertThat(struct.toJavaString(), is("aBc Def"));
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_nStringCapitalize_NoStartAndEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .end(IntegerStruct.TWO)
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("Abc def"));
		Assert.assertThat(struct.toJavaString(), is("Abc def"));
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where a start index and
	 * an end index were provided.
	 */
	@Test
	public void test_nStringCapitalize_StartAndEnd() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .start(IntegerStruct.ONE)
		                                                                            .end(IntegerStruct.TWO)
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("aBc def"));
		Assert.assertThat(struct.toJavaString(), is("aBc def"));
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the string has a
	 * fill pointer.
	 */
	@Test
	public void test_nStringCapitalize_FillPointer() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.toLispInteger(4))
		                                        .build();
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(true), is("Abc def"));
		Assert.assertThat(result.toJavaString(false), is("Abc "));
		Assert.assertThat(struct.toJavaString(true), is("Abc def"));
		Assert.assertThat(struct.toJavaString(false), is("Abc "));
	}

	/**
	 * Test for {@link StringStruct#nStringCapitalize(StringIntervalOpContext)} where the string is
	 * displaced.
	 */
	@Test
	public void test_nStringCapitalize_Displaced() {
		final String str = "abc def";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(6))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final StringStruct result = struct.nStringCapitalize(StringIntervalOpContext.builder()
		                                                                            .build());
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(), is("Bc Def"));
		Assert.assertThat(struct.toJavaString(), is("Bc Def"));
	}

	/*
	String-Trim
	 */

	/**
	 * Test for {@link StringStruct#stringTrim(SequenceStruct)} where a non-character is part of the provided
	 * character bag {@link SequenceStruct}.
	 */
	@Test
	public void test_stringTrim_NonCharacterInBag() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Non-character elements provided in character bag: "));

		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				IntegerStruct.ZERO,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);
		struct.stringTrim(characterBag);
	}

	/**
	 * Test for {@link StringStruct#stringTrim(SequenceStruct)}.
	 */
	@Test
	public void test_stringTrim() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);

		final StringStruct result = struct.stringTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("dabe"));
		Assert.assertThat(struct.toJavaString(), is("cabdabeac"));
	}

	/**
	 * Test for {@link StringStruct#stringTrim(SequenceStruct)} where the string has a fill pointer.
	 */
	@Test
	public void test_stringTrim_FillPointer() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.toLispInteger(5))
		                                        .build();

		final StringStruct result = struct.stringTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("d"));
		Assert.assertThat(struct.toJavaString(true), is("cabdabeac"));
		Assert.assertThat(struct.toJavaString(false), is("cabda"));
	}

	/**
	 * Test for {@link StringStruct#stringTrim(SequenceStruct)} where the string is displaced.
	 */
	@Test
	public void test_stringTrim_Displaced() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(8))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();

		final StringStruct result = struct.stringTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("dabe"));
		Assert.assertThat(struct.toJavaString(), is("abdabeac"));
	}

	/*
	String-Left-Trim
	 */

	/**
	 * Test for {@link StringStruct#stringLeftTrim(SequenceStruct)} where a non-character is part of the provided
	 * character bag {@link SequenceStruct}.
	 */
	@Test
	public void test_stringLeftTrim_NonCharacterInBag() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Non-character elements provided in character bag: "));

		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				IntegerStruct.ZERO,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);
		struct.stringLeftTrim(characterBag);
	}

	/**
	 * Test for {@link StringStruct#stringLeftTrim(SequenceStruct)}.
	 */
	@Test
	public void test_stringLeftTrim() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);

		final StringStruct result = struct.stringLeftTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("dabeac"));
		Assert.assertThat(struct.toJavaString(), is("cabdabeac"));
	}

	/**
	 * Test for {@link StringStruct#stringLeftTrim(SequenceStruct)} where the string has a fill pointer.
	 */
	@Test
	public void test_stringLeftTrim_FillPointer() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.toLispInteger(5))
		                                        .build();

		final StringStruct result = struct.stringLeftTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("da"));
		Assert.assertThat(struct.toJavaString(true), is("cabdabeac"));
		Assert.assertThat(struct.toJavaString(false), is("cabda"));
	}

	/**
	 * Test for {@link StringStruct#stringLeftTrim(SequenceStruct)} where the string is displaced.
	 */
	@Test
	public void test_stringLeftTrim_Displaced() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(8))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();

		final StringStruct result = struct.stringLeftTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("dabeac"));
		Assert.assertThat(struct.toJavaString(), is("abdabeac"));
	}

	/*
	String-Right-Trim
	 */

	/**
	 * Test for {@link StringStruct#stringRightTrim(SequenceStruct)} where a non-character is part of the provided
	 * character bag {@link SequenceStruct}.
	 */
	@Test
	public void test_stringRightTrim_NonCharacterInBag() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("Non-character elements provided in character bag: "));

		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				IntegerStruct.ZERO,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);
		struct.stringRightTrim(characterBag);
	}

	/**
	 * Test for {@link StringStruct#stringRightTrim(SequenceStruct)}.
	 */
	@Test
	public void test_stringRightTrim() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.toLispString(str);

		final StringStruct result = struct.stringRightTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("cabdabe"));
		Assert.assertThat(struct.toJavaString(), is("cabdabeac"));
	}

	/**
	 * Test for {@link StringStruct#stringRightTrim(SequenceStruct)} where the string has a fill pointer.
	 */
	@Test
	public void test_stringRightTrim_FillPointer() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.toLispInteger(5))
		                                        .build();

		final StringStruct result = struct.stringRightTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("cabd"));
		Assert.assertThat(struct.toJavaString(true), is("cabdabeac"));
		Assert.assertThat(struct.toJavaString(false), is("cabda"));
	}

	/**
	 * Test for {@link StringStruct#stringRightTrim(SequenceStruct)} where the string is displaced.
	 */
	@Test
	public void test_stringRightTrim_Displaced() {
		final ListStruct characterBag = ListStruct.toLispList(
				CharacterConstants.LATIN_SMALL_LETTER_A_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_B_CHAR,
				CharacterConstants.LATIN_SMALL_LETTER_C_CHAR
		);

		final String str = "cabdabeac";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(8))
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();

		final StringStruct result = struct.stringRightTrim(characterBag);
		Assert.assertThat(result.toJavaString(), is("abdabe"));
		Assert.assertThat(struct.toJavaString(), is("abdabeac"));
	}

	/*
	Is-Simple-String
	 */

	/**
	 * Test for {@link StringStruct#isSimpleString()} where the string is 'simple'.
	 */
	@Test
	public void test_isSimpleString_True() {
		final StringStruct struct = StringStruct.EMPTY_STRING;
		Assert.assertThat(struct.isSimpleString(), is(BooleanStruct.T));
	}

	/**
	 * Test for {@link StringStruct#isSimpleString()} where the string is 'complex'.
	 */
	@Test
	public void test_isSimpleString_False() {
		final StringStruct struct = StringStruct.builder(IntegerStruct.ZERO)
		                                        .adjustable(BooleanStruct.T)
		                                        .build();
		Assert.assertThat(struct.isSimpleString(), is(BooleanStruct.NIL));
	}

	/*
	To-Java-String
	 */

	/**
	 * Test for {@link StringStruct#toJavaString()} where the provided {@link StringStruct} has no fill-pointer.
	 */
	@Test
	public void test_toJavaString_noArg_NoFillPointer() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final String result = struct.toJavaString();
		Assert.assertThat(result, is(str));
	}

	/**
	 * Test for {@link StringStruct#toJavaString()} where the provided {@link StringStruct} has a fill-pointer.
	 */
	@Test
	public void test_toJavaString_noArg_FillPointer() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final String result = struct.toJavaString();
		Assert.assertThat(result, is(str));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} is displaced with no
	 * fill-pointer and the fill-pointer is to be ignored.
	 */
	@Test
	public void test_toJavaString_Displaced_NoFillPointer_Ignore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final String result = struct.toJavaString(true);
		Assert.assertThat(result, is(str.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} is displaced with no
	 * fill-pointer and the fill-pointer is to be accounted for.
	 */
	@Test
	public void test_toJavaString_Displaced_NoFillPointer_NotIgnore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final String result = struct.toJavaString(false);
		Assert.assertThat(result, is(str.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} is displaced and has
	 * a fill-pointer and the fill-pointer is to be ignored.
	 */
	@Test
	public void test_toJavaString_Displaced_FillPointer_Ignore() {
		final String str = "abc";
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
				              .initialContents(StringStruct.toLispString(str))
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .displacedTo(displacedTo)
				              .displacedIndexOffset(IntegerStruct.ONE)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		final String result = struct.toJavaString(true);
		Assert.assertThat(result, is(str.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} is displaced and has
	 * a fill-pointer and the fill-pointer is to be accounted for.
	 */
	@Test
	public void test_toJavaString_Displaced_FillPointer_NotIgnore() {
		final String str = "abc";
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
				              .initialContents(StringStruct.toLispString(str))
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .displacedTo(displacedTo)
				              .displacedIndexOffset(IntegerStruct.ONE)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		final String result = struct.toJavaString(false);
		Assert.assertThat(result, is(str.substring(1, 2)));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} has no fill-pointer
	 * and the fill-pointer is to be ignored.
	 */
	@Test
	public void test_toJavaString_NoFillPointer_Ignore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final String result = struct.toJavaString(true);
		Assert.assertThat(result, is(str));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} has no fill-pointer
	 * and the fill-pointer is to be accounted for.
	 */
	@Test
	public void test_toJavaString_NoFillPointer_NotIgnore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final String result = struct.toJavaString(false);
		Assert.assertThat(result, is(str));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} has a fill-pointer
	 * and the fill-pointer is to be ignored.
	 */
	@Test
	public void test_toJavaString_FillPointer_Ignore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final String result = struct.toJavaString(true);
		Assert.assertThat(result, is(str));
	}

	/**
	 * Test for {@link StringStruct#toJavaString(boolean)} where the provided {@link StringStruct} has a fill-pointer
	 * and the fill-pointer is to be accounted for.
	 */
	@Test
	public void test_toJavaString_FillPointer_NotIgnore() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final String result = struct.toJavaString(false);
		Assert.assertThat(result, is("ab"));
		Assert.assertThat(result, is(str.substring(0, 2)));
	}

	/*
	To-Java-String
	 */

	/**
	 * Test for {@link StringStruct#emptyString()} method.
	 */
	@Test
	public void test_emptyString() {
		final StringStruct struct = StringStruct.emptyString();
		Assert.assertThat(struct, sameInstance(StringStruct.EMPTY_STRING));
	}

	/*
	To-Lisp-String
	 */

	/**
	 * Test for {@link StringStruct#toLispString(String)} method.
	 */
	@Test
	public void test_toLispString() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		Assert.assertThat(struct.toJavaString(), is(str));
	}

	/*
	Builder
	 */

	/**
	 * Test for {@link StringStruct#builder(IntegerStruct)} method.
	 */
	@Test
	public void test_builder() {
		final StringStruct.Builder builder = StringStruct.builder(IntegerStruct.TWO);
		Assert.assertThat(builder.build().length(), is(IntegerStruct.TWO));
	}

	/*
	To-String
	 */

	/**
	 * Test for {@link StringStruct#toString()} where escaping is on and the {@link StringStruct} is displaced.
	 */
	@Test
	public void test_toString_Displaced_Escape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();
		final String result = struct.toString();
		Assert.assertThat(result, is('"' + str.substring(1) + '"'));
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is on and the {@link StringStruct} has a fill-pointer.
	 */
	@Test
	public void test_toString_FillPointer_Escape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();
		final String result = struct.toString();
		Assert.assertThat(result, is('"' + str.substring(0, 2) + '"'));
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is on and the {@link StringStruct} is a simple string.
	 */
	@Test
	public void test_toString_Simple_Escape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);
		final String result = struct.toString();
		Assert.assertThat(result, is('"' + str + '"'));
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is on and the {@link StringStruct} has the {@code '\'}
	 * and {@link '"'} characters.
	 */
	@Test
	public void test_toString_Simple_Escape_WithSpecialEscapes() {
		final String str = "a\\4*j\"p";
		final StringStruct struct = StringStruct.toLispString(str);
		final String result = struct.toString();
		Assert.assertThat(result, is("\"a\\\\4*j\\\"p\""));
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off and the {@link StringStruct} is displaced.
	 */
	@Test
	public void test_toString_Displaced_NoEscape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.TWO)
		                                        .displacedTo(StringStruct.toLispString(str))
		                                        .displacedIndexOffset(IntegerStruct.ONE)
		                                        .build();

		ToStringTestUtils.validateToStringWithNoEscapes(str.substring(1), struct);
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off and the {@link StringStruct} has a fill-pointer.
	 */
	@Test
	public void test_toString_FillPointer_NoEscape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .fillPointer(IntegerStruct.TWO)
		                                        .build();

		ToStringTestUtils.validateToStringWithNoEscapes(str.substring(0, 2), struct);
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off and the {@link StringStruct} is a 'simple' string.
	 */
	@Test
	public void test_toString_Simple_NoEscape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.toLispString(str);

		ToStringTestUtils.validateToStringWithNoEscapes(str, struct);
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off and the {@link StringStruct} is a 'complex'
	 * string.
	 */
	@Test
	public void test_toString_Complex_NoEscape() {
		final String str = "abc";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		ToStringTestUtils.validateToStringWithNoEscapes(str, struct);
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off, the {@link StringStruct} has the {@code '\'} and
	 * {@link '"'} characters, and the {@link StringStruct} is a 'simple' string.
	 */
	@Test
	public void test_toString_Simple_NoEscape_WithSpecialEscapes() {
		final String str = "a\\4*j\"p";
		final StringStruct struct = StringStruct.toLispString(str);

		ToStringTestUtils.validateToStringWithNoEscapes("a\\\\4*j\\\"p", struct);
	}

	/**
	 * Test for {@link StringStruct#toString()} where escaping is off, the {@link StringStruct} has the {@code '\'} and
	 * {@link '"'} characters, and the {@link StringStruct} is a 'complex' string.
	 */
	@Test
	public void test_toString_Complex_NoEscape_WithSpecialEscapes() {
		final String str = "a\\4*j\"p";
		final StringStruct struct = StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		                                        .initialContents(StringStruct.toLispString(str))
		                                        .adjustable(BooleanStruct.T)
		                                        .build();

		ToStringTestUtils.validateToStringWithNoEscapes("a\\\\4*j\\\"p", struct);
	}

	/*
	Builder edge cases
	 */

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided element-type is {@link CharacterType#INSTANCE}.
	 */
	@Test
	public void test_Builder_ElementType_Character() {
		final StringStruct result = StringStruct.builder(IntegerStruct.ONE)
		                                        .elementType(CharacterType.INSTANCE)
		                                        .build();
		Assert.assertThat(result.arrayElementType(), is(CharacterType.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided element-type is {@link BaseCharType#INSTANCE}.
	 */
	@Test
	public void test_Builder_ElementType_BaseChar() {
		final StringStruct result = StringStruct.builder(IntegerStruct.ONE)
		                                        .elementType(BaseCharType.INSTANCE)
		                                        .build();
		Assert.assertThat(result.arrayElementType(), is(CharacterType.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided element-type is {@link
	 * StandardCharType#INSTANCE}.
	 */
	@Test
	public void test_Builder_ElementType_StandardChar() {
		final StringStruct result = StringStruct.builder(IntegerStruct.ONE)
		                                        .elementType(StandardCharType.INSTANCE)
		                                        .build();
		Assert.assertThat(result.arrayElementType(), is(CharacterType.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided element-type is {@link
	 * ExtendedCharType#INSTANCE}.
	 */
	@Test
	public void test_Builder_ElementType_ExtendedChar() {
		final StringStruct result = StringStruct.builder(IntegerStruct.ONE)
		                                        .elementType(ExtendedCharType.INSTANCE)
		                                        .build();
		Assert.assertThat(result.arrayElementType(), is(CharacterType.INSTANCE));
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided displaced-to contains non-character elements.
	 */
	@Test
	public void test_Builder_DisplacedToContainsNonCharacters() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not an array with a subtype of the upgraded-array-element-type"));

		final VectorStruct displacedTo = VectorStruct.builder(IntegerStruct.ONE)
		                                             .elementType(IntegerType.INSTANCE)
		                                             .initialElement(IntegerStruct.ZERO)
		                                             .build();
		StringStruct.builder(IntegerStruct.ONE)
		            .displacedTo(displacedTo)
		            .build();
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided displaced-to-index-offset is negative.
	 */
	@Test
	public void test_Builder_DisplacedIndexOffsetOutOfBounds_Negative() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final String str = "abc";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		            .displacedTo(displacedTo)
		            .displacedIndexOffset(IntegerStruct.MINUS_ONE)
		            .build();
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided displaced-to-index-offset is greater than the
	 * size of the resulting string.
	 */
	@Test
	public void test_Builder_DisplacedIndexOffsetOutOfBounds_TooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final String str = "abc";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		StringStruct.builder(IntegerStruct.toLispInteger(str.length()))
		            .displacedTo(displacedTo)
		            .displacedIndexOffset(IntegerStruct.TEN)
		            .build();
	}

	/**
	 * Test for {@link StringStruct.Builder} creation where the provided initial-contents contains non-character
	 * elements.
	 */
	@Test
	public void test_Builder_InitialContentsContainsNonCharacters() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a subtype of the upgraded-array-element-type"));

		final VectorStruct initialContents = VectorStruct.builder(IntegerStruct.ONE)
		                                                 .initialElement(IntegerStruct.ZERO)
		                                                 .build();
		StringStruct.builder(IntegerStruct.ONE)
		            .initialContents(initialContents)
		            .build();
	}
}
