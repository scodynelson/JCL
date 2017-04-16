package jcl.lang;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.number.IntegerStructImpl;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;

/**
 * Unit tests for {@link StringStruct} string equality methods.
 */
public class StringStructStringEqualityTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	String=
	 */

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start1 index was
	 * negative.
	 */
	@Test
	public void test_stringEqual_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start1(IntegerStruct.MINUS_ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringEqual_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start1(IntegerStruct.ONE)
		                                         .build());

	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringEqual_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start1(IntegerStruct.TWO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end1 index was negative.
	 */
	@Test
	public void test_stringEqual_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end1(IntegerStruct.MINUS_ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end1 index was more than
	 * the total size of the first structure.
	 */
	@Test
	public void test_stringEqual_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end1(IntegerStruct.ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end1 index was more than
	 * the fill pointer of the first structure.
	 */
	@Test
	public void test_stringEqual_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end1(IntegerStruct.TWO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start1 index was more
	 * than the end1 index.
	 */
	@Test
	public void test_stringEqual_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start1(IntegerStruct.ONE)
		                                         .end1(IntegerStruct.ZERO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start2 index was
	 * negative.
	 */
	@Test
	public void test_stringEqual_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start2(IntegerStruct.MINUS_ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringEqual_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start2(IntegerStruct.ONE)
		                                         .build());

	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringEqual_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start2(IntegerStruct.TWO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end2 index was negative.
	 */
	@Test
	public void test_stringEqual_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end2(IntegerStruct.MINUS_ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end2 index was more than
	 * the total size of the second structure.
	 */
	@Test
	public void test_stringEqual_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end2(IntegerStruct.ONE)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the end2 index was more than
	 * the fill pointer of the second structure.
	 */
	@Test
	public void test_stringEqual_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .end2(IntegerStruct.TWO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where the start2 index was more
	 * than the end2 index.
	 */
	@Test
	public void test_stringEqual_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                         .start2(IntegerStruct.ONE)
		                                         .end2(IntegerStruct.ZERO)
		                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)}.
	 */
	@Test
	public void test_stringEqual_NoStartsAndNoEnds() {
		final String str = "1";
		final StringStruct struct1 = StringStruct.toLispString(str);
		final StringStruct struct2 = StringStruct.toLispString(str);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str));
		Assert.assertThat(struct2.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringEqual_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "31";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringEqual_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .end1(IntegerStruct.ONE)
		                                                                      .end2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringEqual_StartsAndEnds() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "321";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .end1(IntegerStruct.TWO)
		                                                                      .end2(IntegerStruct.TWO)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string1 has a fill pointer.
	 */
	@Test
	public void test_stringEqual_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string2 has a fill pointer.
	 */
	@Test
	public void test_stringEqual_FillPointer2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where both string1 and string2 have
	 * fill pointers.
	 */
	@Test
	public void test_stringEqual_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "124";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string1 is displaced.
	 */
	@Test
	public void test_stringEqual_Displaced1() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string2 is displaced.
	 */
	@Test
	public void test_stringEqual_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where both string1 and string2 are
	 * displaced.
	 */
	@Test
	public void test_stringEqual_Displaced_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "423";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string1 and string2 are not
	 * equal.
	 */
	@Test
	public void test_stringEqual_NotEqual() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqual(StringEqualityContext)} where string1 and string2 are not
	 * equal and case is accounted for.
	 */
	@Test
	public void test_stringEqual_NotEqual_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final BooleanStruct result = struct1.stringEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String/=
	 */

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start1 index was
	 * negative.
	 */
	@Test
	public void test_stringNotEqual_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringNotEqual_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.ONE)
		                                            .build());

	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringNotEqual_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end1 index was
	 * negative.
	 */
	@Test
	public void test_stringNotEqual_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringNotEqual_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringNotEqual_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start1 index was more
	 * than the end1 index.
	 */
	@Test
	public void test_stringNotEqual_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.ONE)
		                                            .end1(IntegerStruct.ZERO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start2 index was
	 * negative.
	 */
	@Test
	public void test_stringNotEqual_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringNotEqual_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.ONE)
		                                            .build());

	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringNotEqual_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end2 index was
	 * negative.
	 */
	@Test
	public void test_stringNotEqual_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringNotEqual_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the end2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringNotEqual_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where the start2 index was more
	 * than the end2 index.
	 */
	@Test
	public void test_stringNotEqual_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.ONE)
		                                            .end2(IntegerStruct.ZERO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)}.
	 */
	@Test
	public void test_stringNotEqual_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringNotEqual_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "31";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringNotEqual_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .end1(IntegerStruct.ONE)
		                                                                      .end2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringNotEqual_StartsAndEnds() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "321";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .end1(IntegerStruct.TWO)
		                                                                      .end2(IntegerStruct.TWO)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string1 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringNotEqual_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string2 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringNotEqual_FillPointer2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where both string1 and string2
	 * have fill pointers.
	 */
	@Test
	public void test_stringNotEqual_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "124";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string1 is displaced.
	 */
	@Test
	public void test_stringNotEqual_Displaced1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "23";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string2 is displaced.
	 */
	@Test
	public void test_stringNotEqual_Displaced2() {
		final String str1 = "23";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where both string1 and string2
	 * are displaced.
	 */
	@Test
	public void test_stringNotEqual_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "412";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string1 and string2 are
	 * equal.
	 */
	@Test
	public void test_stringNotEqual_Equal() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "121";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqual(StringEqualityContext)} where string1 and string2 are
	 * equal and case is accounted for.
	 */
	@Test
	public void test_stringNotEqual_Equal_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqual(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String<
	 */

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start1 index was
	 * negative.
	 */
	@Test
	public void test_stringLessThan_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThan_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.ONE)
		                                            .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThan_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end1 index was
	 * negative.
	 */
	@Test
	public void test_stringLessThan_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThan_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThan_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end1(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start1 index was more
	 * than the end1 index.
	 */
	@Test
	public void test_stringLessThan_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start1(IntegerStruct.ONE)
		                                            .end1(IntegerStruct.ZERO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start2 index was
	 * negative.
	 */
	@Test
	public void test_stringLessThan_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThan_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.ONE)
		                                            .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThan_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end2 index was
	 * negative.
	 */
	@Test
	public void test_stringLessThan_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.MINUS_ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThan_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.ONE)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the end2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThan_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .end2(IntegerStruct.TWO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where the start2 index was more
	 * than the end2 index.
	 */
	@Test
	public void test_stringLessThan_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                            .start2(IntegerStruct.ONE)
		                                            .end2(IntegerStruct.ZERO)
		                                            .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)}.
	 */
	@Test
	public void test_stringLessThan_NoStartsAndNoEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringLessThan_StartsAndNoEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "21";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringLessThan_NoStartsAndEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .end1(IntegerStruct.ONE)
		                                                                      .end2(IntegerStruct.ONE)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringLessThan_StartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "311";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .start1(IntegerStruct.ONE)
		                                                                      .start2(IntegerStruct.ONE)
		                                                                      .end1(IntegerStruct.TWO)
		                                                                      .end2(IntegerStruct.TWO)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string1 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringLessThan_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string2 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringLessThan_FillPointer2() {
		final String str1 = "13";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where both string1 and string2
	 * have fill pointers.
	 */
	@Test
	public void test_stringLessThan_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "1123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string1 is displaced.
	 */
	@Test
	public void test_stringLessThan_Displaced1() {
		final String str1 = "112";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string2 is displaced.
	 */
	@Test
	public void test_stringLessThan_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "111";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where both string1 and string2
	 * are displaced.
	 */
	@Test
	public void test_stringLessThan_Displaced_Both() {
		final String str1 = "213";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string1 is greater than
	 * string2.
	 */
	@Test
	public void test_stringLessThan_GreaterThan() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "132";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string1 is equal to
	 * string2.
	 */
	@Test
	public void test_stringLessThan_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThan(StringEqualityContext)} where string1 is greater than
	 * string2 differing by case.
	 */
	@Test
	public void test_stringLessThan_GreaterThan_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThan(StringEqualityContext.builder(struct2)
		                                                                      .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String>
	 */

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start1 index was
	 * negative.
	 */
	@Test
	public void test_stringGreaterThan_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start1(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start1 index was
	 * more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThan_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start1(IntegerStruct.ONE)
		                                               .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start1 index was
	 * more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThan_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start1(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end1 index was
	 * negative.
	 */
	@Test
	public void test_stringGreaterThan_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end1(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end1 index was more
	 * than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThan_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end1(IntegerStruct.ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end1 index was more
	 * than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThan_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end1(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start1 index was
	 * more than the end1 index.
	 */
	@Test
	public void test_stringGreaterThan_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start1(IntegerStruct.ONE)
		                                               .end1(IntegerStruct.ZERO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start2 index was
	 * negative.
	 */
	@Test
	public void test_stringGreaterThan_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start2(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start2 index was
	 * more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThan_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start2(IntegerStruct.ONE)
		                                               .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start2 index was
	 * more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThan_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start2(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end2 index was
	 * negative.
	 */
	@Test
	public void test_stringGreaterThan_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end2(IntegerStruct.MINUS_ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end2 index was more
	 * than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThan_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end2(IntegerStruct.ONE)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the end2 index was more
	 * than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThan_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .end2(IntegerStruct.TWO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where the start2 index was
	 * more than the end2 index.
	 */
	@Test
	public void test_stringGreaterThan_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                               .start2(IntegerStruct.ONE)
		                                               .end2(IntegerStruct.ZERO)
		                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)}.
	 */
	@Test
	public void test_stringGreaterThan_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringGreaterThan_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .start1(IntegerStruct.ONE)
		                                                                         .start2(IntegerStruct.ONE)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringGreaterThan_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .end1(IntegerStruct.ONE)
		                                                                         .end2(IntegerStruct.ONE)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where a start index and an
	 * end index were provided.
	 */
	@Test
	public void test_stringGreaterThan_StartsAndEnds() {
		final String str1 = "311";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .start1(IntegerStruct.ONE)
		                                                                         .start2(IntegerStruct.ONE)
		                                                                         .end1(IntegerStruct.TWO)
		                                                                         .end2(IntegerStruct.TWO)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string1 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringGreaterThan_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string2 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringGreaterThan_FillPointer2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where both string1 and
	 * string2 have fill pointers.
	 */
	@Test
	public void test_stringGreaterThan_FillPointer_Both() {
		final String str1 = "1123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string1 is displaced.
	 */
	@Test
	public void test_stringGreaterThan_Displaced1() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string2 is displaced.
	 */
	@Test
	public void test_stringGreaterThan_Displaced2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "112";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where both string1 and
	 * string2 are displaced.
	 */
	@Test
	public void test_stringGreaterThan_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "213";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string1 is less than
	 * string2.
	 */
	@Test
	public void test_stringGreaterThan_LessThan() {
		final String str1 = "132";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string1 is equal to
	 * string2.
	 */
	@Test
	public void test_stringGreaterThan_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThan(StringEqualityContext)} where string1 is less than
	 * string2 differing by case.
	 */
	@Test
	public void test_stringGreaterThan_LessThan_AccountsForCase() {
		final String str1 = "a";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "A";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThan(StringEqualityContext.builder(struct2)
		                                                                         .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String<=
	 */

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start1 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start1(IntegerStruct.MINUS_ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start1(IntegerStruct.ONE)
		                                                     .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start1(IntegerStruct.TWO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end1(IntegerStruct.MINUS_ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end1(IntegerStruct.ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end1(IntegerStruct.TWO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start1 index
	 * was more than the end1 index.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start1(IntegerStruct.ONE)
		                                                     .end1(IntegerStruct.ZERO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start2 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start2(IntegerStruct.MINUS_ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start2(IntegerStruct.ONE)
		                                                     .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start2(IntegerStruct.TWO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end2(IntegerStruct.MINUS_ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end2(IntegerStruct.ONE)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .end2(IntegerStruct.TWO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where the start2 index
	 * was more than the end2 index.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                     .start2(IntegerStruct.ONE)
		                                                     .end2(IntegerStruct.ZERO)
		                                                     .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)}.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NoStartsAndNoEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_StartsAndNoEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "21";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .start1(IntegerStruct.ONE)
		                                                                               .start2(IntegerStruct.ONE)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_NoStartsAndEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .end1(IntegerStruct.ONE)
		                                                                               .end2(IntegerStruct.ONE)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where a start index and
	 * an end index were provided.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_StartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "311";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .start1(IntegerStruct.ONE)
		                                                                               .start2(IntegerStruct.ONE)
		                                                                               .end1(IntegerStruct.TWO)
		                                                                               .end2(IntegerStruct.TWO)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string1 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string2 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_FillPointer2() {
		final String str1 = "13";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where both string1 and
	 * string2 have fill pointers.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "1123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Displaced1() {
		final String str1 = "112";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "111";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where both string1 and
	 * string2 are displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Displaced_Both() {
		final String str1 = "213";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string1 is
	 * greater than string2.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_GreaterThan() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "132";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string1 is
	 * equal to string2.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(IntegerStructImpl.valueOf(3)));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualTo(StringEqualityContext)} where string1 is
	 * greater than string2 differing by case.
	 */
	@Test
	public void test_stringLessThanOrEqualTo_GreaterThan_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                                               .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String>=
	 */

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start1
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start1(IntegerStruct.MINUS_ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start1
	 * index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start1(IntegerStruct.ONE)
		                                                        .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start1
	 * index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start1(IntegerStruct.TWO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end1(IntegerStruct.MINUS_ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end1(IntegerStruct.ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end1(IntegerStruct.TWO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start1
	 * index was more than the end1 index.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start1(IntegerStruct.ONE)
		                                                        .end1(IntegerStruct.ZERO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start2
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start2(IntegerStruct.MINUS_ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start2
	 * index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start2(IntegerStruct.ONE)
		                                                        .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start2
	 * index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start2(IntegerStruct.TWO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end2(IntegerStruct.MINUS_ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end2(IntegerStruct.ONE)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the end2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .end2(IntegerStruct.TWO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where the start2
	 * index was more than the end2 index.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanOrEqualTo(StringEqualityContext.builder(struct2)
		                                                        .start2(IntegerStruct.ONE)
		                                                        .end2(IntegerStruct.ZERO)
		                                                        .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)}.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where a start index
	 * was provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where an end index
	 * was provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where a start index
	 * and an end index were provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_StartsAndEnds() {
		final String str1 = "311";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string1 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string2 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_FillPointer2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where both string1
	 * and string2 have fill pointers.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_FillPointer_Both() {
		final String str1 = "1123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Displaced1() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Displaced2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "112";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where both string1
	 * and string2 are displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "213";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string1 is
	 * less than string2.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_LessThan() {
		final String str1 = "132";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string1 is
	 * equal to string2.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStructImpl.valueOf(3)));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualTo(StringEqualityContext)} where string1 is
	 * greater than string2 differing by case.
	 */
	@Test
	public void test_stringGreaterThanOrEqualTo_NotEqual_AccountsForCase() {
		final String str1 = "a";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "A";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualTo(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Equal
	 */

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was negative.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start1(IntegerStruct.MINUS_ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start1(IntegerStruct.ONE)
		                                                   .build());

	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start1(IntegerStruct.TWO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end1 index was
	 * negative.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end1(IntegerStruct.MINUS_ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end1 index was
	 * more than the total size of the first structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end1(IntegerStruct.ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end1 index was
	 * more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end1(IntegerStruct.TWO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the end1 index.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start1(IntegerStruct.ONE)
		                                                   .end1(IntegerStruct.ZERO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was negative.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start2(IntegerStruct.MINUS_ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start2(IntegerStruct.ONE)
		                                                   .build());

	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start2(IntegerStruct.TWO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end2 index was
	 * negative.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end2(IntegerStruct.MINUS_ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end2 index was
	 * more than the total size of the second structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end2(IntegerStruct.ONE)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the end2 index was
	 * more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .end2(IntegerStruct.TWO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the end2 index.
	 */
	@Test
	public void test_stringEqualIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                   .start2(IntegerStruct.ONE)
		                                                   .end2(IntegerStruct.ZERO)
		                                                   .build());
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NoStartsAndNoEnds() {
		final String str = "1";
		final StringStruct struct1 = StringStruct.toLispString(str);
		final StringStruct struct2 = StringStruct.toLispString(str);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str));
		Assert.assertThat(struct2.toJavaString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where a start index was
	 * provided.
	 */
	@Test
	public void test_stringEqualIgnoreCase_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "31";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .start1(IntegerStruct.ONE)
		                                                                             .start2(IntegerStruct.ONE)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .end1(IntegerStruct.ONE)
		                                                                             .end2(IntegerStruct.ONE)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where a start index and
	 * an end index were provided.
	 */
	@Test
	public void test_stringEqualIgnoreCase_StartsAndEnds() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "321";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .start1(IntegerStruct.ONE)
		                                                                             .start2(IntegerStruct.ONE)
		                                                                             .end1(IntegerStruct.TWO)
		                                                                             .end2(IntegerStruct.TWO)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string1 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringEqualIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string2 has a fill
	 * pointer.
	 */
	@Test
	public void test_stringEqualIgnoreCase_FillPointer2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 have fill pointers.
	 */
	@Test
	public void test_stringEqualIgnoreCase_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "124";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Displaced1() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 are displaced.
	 */
	@Test
	public void test_stringEqualIgnoreCase_Displaced_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "423";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string1 and string2
	 * are not equal.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NotEqual() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringEqualIgnoreCase(StringEqualityContext)} where string1 and string2
	 * are not equal and case is accounted for.
	 */
	@Test
	public void test_stringEqualIgnoreCase_NotEqual_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                             .build());
		Assert.assertThat(result, is(BooleanStruct.T));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Not-Equal
	 */

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was negative.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.ONE)
		                                                      .build());

	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end1 index
	 * was negative.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the end1 index.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.ONE)
		                                                      .end1(IntegerStruct.ZERO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was negative.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.ONE)
		                                                      .build());

	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end2 index
	 * was negative.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the end2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the end2 index.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.ONE)
		                                                      .end2(IntegerStruct.ZERO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where a start index
	 * was provided.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "31";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where a start index
	 * and an end index were provided.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_StartsAndEnds() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "321";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string1 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                                .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string2 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_FillPointer2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 have fill pointers.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "124";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Displaced1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "23";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Displaced2() {
		final String str1 = "23";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 are displaced.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "412";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string1 and
	 * string2 are not equal.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NotEqual() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "121";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringNotEqualIgnoreCase(StringEqualityContext)} where string1 and
	 * string2 are not equal and case is accounted for.
	 */
	@Test
	public void test_stringNotEqualIgnoreCase_NotEqual_AccountsForCase() {
		final String str1 = "A";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringNotEqualIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Lessp
	 */

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start1 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.ONE)
		                                                      .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end1 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end1 index
	 * was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end1 index
	 * was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end1(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start1 index
	 * was more than the end1 index.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start1(IntegerStruct.ONE)
		                                                      .end1(IntegerStruct.ZERO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start2 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.ONE)
		                                                      .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end2 index
	 * was negative.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.MINUS_ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end2 index
	 * was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.ONE)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the end2 index
	 * was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .end2(IntegerStruct.TWO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where the start2 index
	 * was more than the end2 index.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                      .start2(IntegerStruct.ONE)
		                                                      .end2(IntegerStruct.ZERO)
		                                                      .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NoStartsAndNoEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where a start index
	 * was provided.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_StartsAndNoEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "21";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where an end index was
	 * provided.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_NoStartsAndEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where a start index
	 * and an end index were provided.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_StartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "311";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string1 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string2 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_FillPointer2() {
		final String str1 = "13";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 have fill pointers.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "1123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Displaced1() {
		final String str1 = "112";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "111";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where both string1 and
	 * string2 are displaced.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Displaced_Both() {
		final String str1 = "213";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string1 is
	 * greater than string2.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_GreaterThan() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "132";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string1 is
	 * equal to string2.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanIgnoreCase(StringEqualityContext)} where string1 is
	 * greater than string2 differing by case.
	 */
	@Test
	public void test_stringLessThanIgnoreCase_GreaterThan_AccountsForCase() {
		final String str1 = "a";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "B";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Greaterp
	 */

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start1
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start1(IntegerStruct.MINUS_ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start1
	 * index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start1(IntegerStruct.ONE)
		                                                         .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start1
	 * index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start1(IntegerStruct.TWO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end1
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end1(IntegerStruct.MINUS_ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end1
	 * index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end1(IntegerStruct.ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end1
	 * index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end1(IntegerStruct.TWO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start1
	 * index was more than the end1 index.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start1(IntegerStruct.ONE)
		                                                         .end1(IntegerStruct.ZERO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start2
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start2(IntegerStruct.MINUS_ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start2
	 * index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start2(IntegerStruct.ONE)
		                                                         .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start2
	 * index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start2(IntegerStruct.TWO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end2
	 * index was negative.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end2(IntegerStruct.MINUS_ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end2
	 * index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end2(IntegerStruct.ONE)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the end2
	 * index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .end2(IntegerStruct.TWO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where the start2
	 * index was more than the end2 index.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanIgnoreCase(StringEqualityContext.builder(struct2)
		                                                         .start2(IntegerStruct.ONE)
		                                                         .end2(IntegerStruct.ZERO)
		                                                         .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where a start index
	 * was provided.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where an end index
	 * was provided.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where a start index
	 * and an end index were provided.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_StartsAndEnds() {
		final String str1 = "311";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string1 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string2 has a
	 * fill pointer.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_FillPointer2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where both string1
	 * and string2 have fill pointers.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_FillPointer_Both() {
		final String str1 = "1123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string1 is
	 * displaced.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Displaced1() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string2 is
	 * displaced.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Displaced2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "112";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where both string1
	 * and string2 are displaced.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "213";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string1 is
	 * less than string2.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_LessThan() {
		final String str1 = "132";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string1 is
	 * equal to string2.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanIgnoreCase(StringEqualityContext)} where string1 is
	 * less than string2 differing by case.
	 */
	@Test
	public void test_stringGreaterThanIgnoreCase_LessThan_AccountsForCase() {
		final String str1 = "B";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Not-Greaterp
	 */

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start1(IntegerStruct.MINUS_ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start1(IntegerStruct.ONE)
		                                                               .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start1(IntegerStruct.TWO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end1(IntegerStruct.MINUS_ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end1(IntegerStruct.ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end1(IntegerStruct.TWO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the end1 index.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start1(IntegerStruct.ONE)
		                                                               .end1(IntegerStruct.ZERO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start2(IntegerStruct.MINUS_ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start2(IntegerStruct.ONE)
		                                                               .build());

	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start2(IntegerStruct.TWO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was negative.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end2(IntegerStruct.MINUS_ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end2(IntegerStruct.ONE)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .end2(IntegerStruct.TWO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the end2 index.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringLessThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                               .start2(IntegerStruct.ONE)
		                                                               .end2(IntegerStruct.ZERO)
		                                                               .build());
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NoStartsAndNoEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "1";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where a start
	 * index was provided.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_StartsAndNoEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "21";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where an end
	 * index was provided.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_NoStartsAndEnds() {
		final String str1 = "2";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where a start
	 * index and an end index were provided.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_StartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "311";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where string1
	 * has a fill pointer.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where string2
	 * has a fill pointer.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_FillPointer2() {
		final String str1 = "13";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where both
	 * string1 and string2 have fill pointers.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_FillPointer_Both() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "1123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where string1
	 * is displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Displaced1() {
		final String str1 = "112";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "11";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where string2
	 * is displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Displaced2() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "111";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where both
	 * string1 and string2 are displaced.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Displaced_Both() {
		final String str1 = "213";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "312";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is greater than string2.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_GreaterThan() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "132";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is equal to string2.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStructImpl.valueOf(3)));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringLessThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is greater than string2 differing by case.
	 */
	@Test
	public void test_stringLessThanOrEqualToIgnoreCase_GreaterThan_AccountsForCase() {
		final String str1 = "a";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "B";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringLessThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/*
	String-Not-Lessp
	 */

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NegativeStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start1(IntegerStruct.MINUS_ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Start1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start1(IntegerStruct.ONE)
		                                                                  .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Start1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start1(IntegerStruct.TWO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NegativeEnd1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end1(IntegerStruct.MINUS_ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was more than the total size of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End1MoreThanTotalSize1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end1(IntegerStruct.ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end1 index was more than the fill pointer of the first structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End1MoreThanFillPointer1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end1(IntegerStruct.TWO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start1 index was more than the end1 index.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End1MoreThanStart1() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start1(IntegerStruct.ONE)
		                                                                  .end1(IntegerStruct.ZERO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NegativeStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start2(IntegerStruct.MINUS_ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Start2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start2(IntegerStruct.ONE)
		                                                                  .build());

	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Start2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad start value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start2(IntegerStruct.TWO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was negative.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NegativeEnd2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end2(IntegerStruct.MINUS_ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was more than the total size of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End2MoreThanTotalSize2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.emptyString();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end2(IntegerStruct.ONE)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * end2 index was more than the fill pointer of the second structure.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End2MoreThanFillPointer2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.emptyString();
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .fillPointer(IntegerStruct.ONE)
		                                         .build();
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .end2(IntegerStruct.TWO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where the
	 * start2 index was more than the end2 index.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_End2MoreThanStart2() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("Bad end value "));

		final StringStruct struct1 = StringStruct.toLispString("1");
		final StringStruct struct2 = StringStruct.toLispString("1");
		struct1.stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext.builder(struct2)
		                                                                  .start2(IntegerStruct.ONE)
		                                                                  .end2(IntegerStruct.ZERO)
		                                                                  .build());
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)}.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NoStartsAndNoEnds() {
		final String str1 = "1";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where a
	 * start index was provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_StartsAndNoEnds() {
		final String str1 = "21";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where an
	 * end index was provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_NoStartsAndEnds() {
		final String str1 = "12";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "2";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .end1(IntegerStruct.ONE)
				                     .end2(IntegerStruct.ONE)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where a
	 * start index and an end index were provided.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_StartsAndEnds() {
		final String str1 = "311";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .start1(IntegerStruct.ONE)
				                     .start2(IntegerStruct.ONE)
				                     .end1(IntegerStruct.TWO)
				                     .end2(IntegerStruct.TWO)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 has a fill pointer.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_FillPointer1() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "13";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string2 has a fill pointer.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_FillPointer2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where both
	 * string1 and string2 have fill pointers.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_FillPointer_Both() {
		final String str1 = "1123";
		final StringStruct struct1 = StringStruct.builder(IntegerStructImpl.valueOf(str1.length()))
		                                         .initialContents(StringStruct.toLispString(str1))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.builder(IntegerStructImpl.valueOf(str2.length()))
		                                         .initialContents(StringStruct.toLispString(str2))
		                                         .fillPointer(IntegerStruct.TWO)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Displaced1() {
		final String str1 = "111";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "12";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string2 is displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Displaced2() {
		final String str1 = "11";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "112";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where both
	 * string1 and string2 are displaced.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Displaced_Both() {
		final String str1 = "312";
		final StringStruct struct1 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str1))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final String str2 = "213";
		final StringStruct struct2 = StringStruct.builder(IntegerStruct.TWO)
		                                         .displacedTo(StringStruct.toLispString(str2))
		                                         .displacedIndexOffset(IntegerStruct.ONE)
		                                         .build();
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(BooleanStruct.NIL));
		Assert.assertThat(struct1.toJavaString(), is(str1.substring(1)));
		Assert.assertThat(struct2.toJavaString(), is(str2.substring(1)));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is less than string2.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_LessThan() {
		final String str1 = "132";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ONE));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is equal to string2.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_Equal() {
		final String str1 = "123";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "123";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStructImpl.valueOf(3)));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}

	/**
	 * Test for {@link StringStruct#stringGreaterThanOrEqualToIgnoreCase(StringEqualityContext)} where
	 * string1 is greater than string2 differing by case.
	 */
	@Test
	public void test_stringGreaterThanOrEqualToIgnoreCase_LessThan_AccountsForCase() {
		final String str1 = "B";
		final StringStruct struct1 = StringStruct.toLispString(str1);
		final String str2 = "a";
		final StringStruct struct2 = StringStruct.toLispString(str2);
		final LispStruct result = struct1.stringGreaterThanOrEqualToIgnoreCase(
				StringEqualityContext.builder(struct2)
				                     .build());
		Assert.assertThat(result, is(IntegerStruct.ZERO));
		Assert.assertThat(struct1.toJavaString(), is(str1));
		Assert.assertThat(struct2.toJavaString(), is(str2));
	}
}
