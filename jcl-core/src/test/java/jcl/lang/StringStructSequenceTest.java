package jcl.lang;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Spliterator;

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
 * Unit tests for {@link StringStruct} sequence methods.
 */
public class StringStructSequenceTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Length
	 */

	/**
	 * Test for {@link StringStruct#length()}.
	 */
	@Test
	public void test_length() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .build();
		Assert.assertThat(struct.length(), is(IntegerStruct.TEN));
	}

	/**
	 * Test for {@link StringStruct#length()} where there is a fill-pointer.
	 */
	@Test
	public void test_length_FillPointer() {
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .fillPointer(IntegerStruct.TWO)
				              .build();
		Assert.assertThat(struct.length(), is(IntegerStruct.TWO));
	}

	/*
	Elt
	 */

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the index provided was too small.
	 */
	@Test
	public void test_elt_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.elt(IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the index provided was too large.
	 */
	@Test
	public void test_elt_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		struct.elt(IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the index provided was beyond the specified
	 * fill-pointer.
	 */
	@Test
	public void test_elt_IndexBeyondFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is not a valid sequence index for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		struct.elt(IntegerStruct.TWO);
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the total size is greater than the contents.
	 */
	@Test
	public void test_elt_TotalSizeGreaterThanContents() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .adjustable(TStruct.INSTANCE)
				              .build();
		struct.adjustArray(AdjustArrayContext.builder(IntegerStruct.TWO)
		                                     .build());
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
		Assert.assertThat(struct.elt(IntegerStruct.ONE), is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)}.
	 */
	@Test
	public void test_elt() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} with a fill-pointer.
	 */
	@Test
	public void test_elt_FillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_elt_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the array is displaced and has a fill-pointer.
	 */
	@Test
	public void test_elt_Displaced_FillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .displacedTo(displacedTo)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the array is displaced and the displaced array has a
	 * fill-pointer.
	 */
	@Test
	public void test_elt_Displaced_DisplacedFillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));
	}

	/*
	Set-Elt
	 */

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} where the provided new element is
	 * not a {@link CharacterStruct}.
	 */
	@Test
	public void test_setfElt_NewElementNotCharacterStruct() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage(containsString("is not a character type."));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final IntegerStruct newElement = IntegerStruct.ONE;
		struct.setfElt(newElement, IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} where the index provided was too
	 * small.
	 */
	@Test
	public void test_setfElt_IndexTooSmall() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfElt(newElement, IntegerStruct.MINUS_ONE);
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} where the index provided was too
	 * large.
	 */
	@Test
	public void test_setfElt_IndexTooLarge() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is out of bounds for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfElt(newElement, IntegerStruct.ONE);
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} where the index provided was beyond the
	 * specified fill-pointer.
	 */
	@Test
	public void test_setfElt_IndexBeyondFillPointer() {
		thrown.expect(ErrorException.class);
		thrown.expectMessage(containsString("is not a valid sequence index for"));

		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TEN)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		struct.setfElt(newElement, IntegerStruct.TWO);
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)}.
	 */
	@Test
	public void test_setfElt() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		final LispStruct result = struct.setfElt(newElement, IntegerStruct.ZERO);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} with a fill-pointer.
	 */
	@Test
	public void test_setfElt_FillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		final LispStruct result = struct.setfElt(newElement, IntegerStruct.ZERO);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#setfElt(LispStruct, IntegerStruct)} where the array is displaced.
	 */
	@Test
	public void test_setfElt_Displaced() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.ONE)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfElt(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(displacedTo.elt(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the array is displaced and has a fill-pointer.
	 */
	@Test
	public void test_setfElt_Displaced_FillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.TWO)
				              .displacedTo(displacedTo)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		final LispStruct result = struct.setfElt(newElement, IntegerStruct.ZERO);
		Assert.assertThat(result, is(newElement));
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(newElement));
	}

	/**
	 * Test for {@link StringStruct#elt(IntegerStruct)} where the array is displaced and the displaced array has a
	 * fill-pointer.
	 */
	@Test
	public void test_setfElt_Displaced_DisplacedFillPointer() {
		final CharacterStruct initialElement = CharacterConstants.DOLLAR_SIGN_CHAR;
		final StringStruct displacedTo
				= StringStruct.builder(IntegerStruct.TWO)
				              .initialElement(initialElement)
				              .fillPointer(IntegerStruct.ONE)
				              .build();
		final StringStruct struct
				= StringStruct.builder(IntegerStruct.ONE)
				              .displacedTo(displacedTo)
				              .build();
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(initialElement));

		final CharacterStruct newElement = CharacterConstants.NUMBER_SIGN_CHAR;
		Assert.assertThat(struct.setfElt(newElement, IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(struct.elt(IntegerStruct.ZERO), is(newElement));
		Assert.assertThat(displacedTo.elt(IntegerStruct.ZERO), is(newElement));
	}

	/*
	Reverse
	 */

	/**
	 * Test for {@link StringStruct#reverse()}.
	 */
	@Test
	public void test_reverse() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final StringStruct struct = StringStruct.toLispString(str);
		final StringStruct result = struct.reverse();
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.toJavaString(false), is(reversedStr));
	}

	/**
	 * Test for {@link StringStruct#reverse()} with a fill-pointer.
	 */
	@Test
	public void test_reverse_FillPointer() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final String reversedSubStr = new StringBuilder(str.substring(0, fillPointer.intValue())).reverse().toString();

		final StringStruct initialContents = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .initialContents(initialContents)
				              .fillPointer(fillPointer)
				              .build();

		final StringStruct result = struct.reverse();
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.toJavaString(false), is(not(reversedStr)));
		Assert.assertThat(result.toJavaString(false), is(reversedSubStr));
	}

	/**
	 * Test for {@link StringStruct#reverse()} where the array is displaced.
	 */
	@Test
	public void test_reverse_Displaced() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .build();

		final StringStruct result = struct.reverse();
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.toJavaString(false), is(reversedStr));
	}

	/**
	 * Test for {@link StringStruct#reverse()} where the array is displaced and the displaced array has a
	 * fill-pointer.
	 */
	@Test
	public void test_reverse_DisplacedFillPointer() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final String reversedSubStr = new StringBuilder(str.substring(0, fillPointer.intValue())).reverse().toString();

		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .fillPointer(fillPointer)
				              .build();

		final StringStruct result = struct.reverse();
		Assert.assertThat(result, not(sameInstance(struct)));
		Assert.assertThat(result.toJavaString(false), is(not(reversedStr)));
		Assert.assertThat(result.toJavaString(false), is(reversedSubStr));
	}

	/*
	NReverse
	 */

	/**
	 * Test for {@link StringStruct#nReverse()}.
	 */
	@Test
	public void test_nReverse() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final StringStruct struct = StringStruct.toLispString(str);

		final StringStruct result = struct.nReverse();
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(false), is(reversedStr));
	}

	/**
	 * Test for {@link StringStruct#nReverse()} with a fill-pointer.
	 */
	@Test
	public void test_nReverse_FillPointer() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final String reversedSubStr = new StringBuilder(str.substring(0, fillPointer.intValue())).reverse().toString();

		final StringStruct initialContents = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .initialContents(initialContents)
				              .fillPointer(fillPointer)
				              .build();

		final StringStruct result = struct.nReverse();
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(false), is(not(reversedStr)));
		Assert.assertThat(result.toJavaString(false), is(reversedSubStr));
	}

	/**
	 * Test for {@link StringStruct#nReverse()} where the array is displaced.
	 */
	@Test
	public void test_nReverse_Displaced() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .build();

		final StringStruct result = struct.nReverse();
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(false), is(reversedStr));
	}

	/**
	 * Test for {@link StringStruct#nReverse()} where the array is displaced and the displaced array has a
	 * fill-pointer.
	 */
	@Test
	public void test_nReverse_DisplacedFillPointer() {
		final String str = "12345";
		final String reversedStr = new StringBuilder(str).reverse().toString();

		final IntegerStruct fillPointer = IntegerStruct.TWO;
		final String reversedSubStr = new StringBuilder(str.substring(0, fillPointer.intValue())).reverse().toString();

		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .fillPointer(fillPointer)
				              .build();

		final StringStruct result = struct.nReverse();
		Assert.assertThat(result, sameInstance(struct));
		Assert.assertThat(result.toJavaString(false), is(not(reversedStr)));
		Assert.assertThat(result.toJavaString(false), is(reversedSubStr));
	}

	/*
	Iterator
	 */

	/**
	 * Test for {@link StringStruct#iterator()}.
	 */
	@Test
	public void test_iterator() {
		final String str = "12345";
		final StringStruct struct = StringStruct.toLispString(str);
		final Iterator<LispStruct> iterator = struct.iterator();

		final StringBuilder resultBuilder = new StringBuilder();
		while (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			populateBuilder(resultBuilder, element);
		}
		Assert.assertThat(resultBuilder.toString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#iterator()} where {@link Iterator#next()} is called and no values are left.
	 */
	@Test
	public void test_iterator_NoSuchElement() {
		thrown.expect(NoSuchElementException.class);
		thrown.expectMessage("All elements consumed.");

		final String str = "12345";
		final StringStruct struct = StringStruct.toLispString(str);
		final Iterator<LispStruct> iterator = struct.iterator();

		while (iterator.hasNext()) {
			iterator.next();
		}
		iterator.next();
	}

	/**
	 * Test for {@link StringStruct#iterator()} where the array is displaced.
	 */
	@Test
	public void test_iterator_Displaced() {
		final String str = "12345";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .build();
		final Iterator<LispStruct> iterator = struct.iterator();

		final StringBuilder resultBuilder = new StringBuilder();
		while (iterator.hasNext()) {
			final LispStruct element = iterator.next();
			populateBuilder(resultBuilder, element);
		}
		Assert.assertThat(resultBuilder.toString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#iterator()} where the array is displaced and {@link Iterator#next()} is called and
	 * no values are left.
	 */
	@Test
	public void test_iterator_Displaced_NoSuchElement() {
		thrown.expect(NoSuchElementException.class);
		thrown.expectMessage("All elements consumed.");

		final String str = "12345";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .build();
		final Iterator<LispStruct> iterator = struct.iterator();

		while (iterator.hasNext()) {
			iterator.next();
		}
		iterator.next();
	}

	/*
	Spliterator
	 */

	/**
	 * Test for {@link StringStruct#spliterator()}.
	 */
	@Test
	public void test_spliterator() {
		final String str = "12345";
		final StringStruct struct = StringStruct.toLispString(str);
		final Spliterator<LispStruct> spliterator = struct.spliterator();

		final StringBuilder resultBuilder = new StringBuilder();
		spliterator.forEachRemaining(element -> populateBuilder(resultBuilder, element));
		Assert.assertThat(resultBuilder.toString(), is(str));
	}

	/**
	 * Test for {@link StringStruct#spliterator()} where the array is displaced.
	 */
	@Test
	public void test_spliterator_Displaced() {
		final String str = "12345";
		final StringStruct displacedTo = StringStruct.toLispString(str);
		final StringStruct struct
				= StringStruct.builder(IntegerStructImpl.valueOf(str.length()))
				              .displacedTo(displacedTo)
				              .build();
		final Spliterator<LispStruct> spliterator = struct.spliterator();

		final StringBuilder resultBuilder = new StringBuilder();
		spliterator.forEachRemaining(element -> populateBuilder(resultBuilder, element));
		Assert.assertThat(resultBuilder.toString(), is(str));
	}

	/**
	 * Helper method for testing {@link StringStruct#iterator()} and {@link StringStruct#spliterator()} methods,
	 * populating the result builder with the provided element if it is a {@link CharacterStruct}, or asserting a
	 * failure if it isn't.
	 *
	 * @param resultBuilder
	 * 		the {@link StringBuilder} used to test iterator or spliterator results
	 * @param element
	 * 		the current element in the iterator or spliterator iteration
	 */
	private static void populateBuilder(final StringBuilder resultBuilder, final LispStruct element) {
		if (element instanceof CharacterStruct) {
			resultBuilder.appendCodePoint(((CharacterStruct) element).getCodePoint());
		} else {
			Assert.fail("Element returned from 'iterator.next()' not a CharacterStruct: " + element);
		}
	}
}
