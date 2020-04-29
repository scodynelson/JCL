package jcl.lang;

import java.util.stream.Stream;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.ComplexStringStructImpl;
import jcl.lang.internal.SimpleStringStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StringStruct} is the object representation of a Lisp 'string' type.
 */
public interface StringStruct extends VectorStruct {

	/**
	 * Constant representing an empty StringStruct.
	 */
	StringStruct EMPTY_STRING = new SimpleStringStructImpl(
			IntegerStruct.ZERO, CommonLispSymbols.CHARACTER, new StringBuilder()
	);

	/**
	 * Returns a new string with the contents upper-cased according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return a new string with the contents upper-cased
	 */
	StringStruct stringUpcase(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Returns a new string with the contents lower-cased according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return a new string with the contents lower-cased
	 */
	StringStruct stringDowncase(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Returns a new string with the contents capitalized according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return a new string with the contents capitalized
	 */
	StringStruct stringCapitalize(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Destructively modifies this string with the contents upper-cased according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return this string with the contents upper-cased
	 */
	StringStruct nStringUpcase(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Destructively modifies this string with the contents lower-cased according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return this string with the contents lower-cased
	 */
	StringStruct nStringDowncase(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Destructively modifies this string with the contents capitalized according to the provided start and end.
	 *
	 * @param start
	 * 		the starting index of the string casing
	 * @param end
	 * 		the ending index of the string casing
	 *
	 * @return this string with the contents capitalized
	 */
	StringStruct nStringCapitalize(final IntegerStruct start, final IntegerStruct end);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the beginning and end of the
	 * string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the beginning and end of
	 * 		the string
	 */
	StringStruct stringTrim(final SequenceStruct characterBag);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the beginning of the string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the beginning of the string
	 */
	StringStruct stringLeftTrim(final SequenceStruct characterBag);

	/**
	 * Returns a new string with the characters in the provided character-bag trimmed from the end of the string.
	 *
	 * @param characterBag
	 * 		the bag of characters to trim from the string
	 *
	 * @return and new string with the characters in the provided character-bag trimmed from the end of the string
	 */
	StringStruct stringRightTrim(final SequenceStruct characterBag);

	/**
	 * Determines equality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return T if the strings are equal; NIL otherwise
	 */
	BooleanStruct stringEqual(final StringStruct string2,
	                          final IntegerStruct start1, final IntegerStruct end1,
	                          final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines inequality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the strings are not equal; an {@link IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringNotEqual(final StringStruct string2,
	                          final IntegerStruct start1, final IntegerStruct end1,
	                          final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines less-than inequality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is less-than the second; an {@link IntegerStruct} mismatch index where the
	 * 		strings differ otherwise
	 */
	LispStruct stringLessThan(final StringStruct string2,
	                          final IntegerStruct start1, final IntegerStruct end1,
	                          final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines greater-than inequality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is greater-than the second; an {@link IntegerStruct} mismatch index where the
	 * 		strings differ otherwise
	 */
	LispStruct stringGreaterThan(final StringStruct string2,
	                             final IntegerStruct start1, final IntegerStruct end1,
	                             final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines less-than-or-equal-to inequality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is less-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * 		where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualTo(final StringStruct string2,
	                                   final IntegerStruct start1, final IntegerStruct end1,
	                                   final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines greater-than-or-equal-to inequality of strings. Case is accounted for.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is greater-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * 		where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualTo(final StringStruct string2,
	                                      final IntegerStruct start1, final IntegerStruct end1,
	                                      final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines equality of strings. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return T if the strings are equal; NIL otherwise
	 */
	BooleanStruct stringEqualIgnoreCase(final StringStruct string2,
	                                    final IntegerStruct start1, final IntegerStruct end1,
	                                    final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines inequality of strings. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the strings are not equal; an {@link IntegerStruct} mismatch index where the strings differ otherwise
	 */
	LispStruct stringNotEqualIgnoreCase(final StringStruct string2,
	                                    final IntegerStruct start1, final IntegerStruct end1,
	                                    final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines less-than inequality of strings. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is less-than the second; an {@link IntegerStruct} mismatch index where the
	 * 		strings differ otherwise
	 */
	LispStruct stringLessThanIgnoreCase(final StringStruct string2,
	                                    final IntegerStruct start1, final IntegerStruct end1,
	                                    final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines greater-than inequality of strings}. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is greater-than the second; an {@link IntegerStruct} mismatch index where the
	 * 		strings differ otherwise
	 */
	LispStruct stringGreaterThanIgnoreCase(final StringStruct string2,
	                                       final IntegerStruct start1, final IntegerStruct end1,
	                                       final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines less-than-or-equal-to inequality of strings. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is less-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * 		where the strings differ otherwise
	 */
	LispStruct stringLessThanOrEqualToIgnoreCase(final StringStruct string2,
	                                             final IntegerStruct start1, final IntegerStruct end1,
	                                             final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Determines greater-than-or-equal-to inequality of strings. Case is ignored.
	 *
	 * @param string2
	 * 		the string to compare to
	 * @param start1
	 * 		the starting index of the first string in the comparison
	 * @param end1
	 * 		the ending index of the first string in the comparison
	 * @param start2
	 * 		the starting index of the second string in the comparison
	 * @param end2
	 * 		the ending index of the second string in the comparison
	 *
	 * @return NIL if the first string is greater-than-or-equal-to the second; an {@link IntegerStruct} mismatch index
	 * 		where the strings differ otherwise
	 */
	LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringStruct string2,
	                                                final IntegerStruct start1, final IntegerStruct end1,
	                                                final IntegerStruct start2, final IntegerStruct end2);

	/**
	 * Returns whether or not the String is a 'simple' string.
	 *
	 * @return true if the String is a 'simple' string; false otherwise
	 */
	BooleanStruct isSimpleString();

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String toJavaString();

	/**
	 * Returns the {@link String} representation of the StringStruct, ignoring the fill-pointer attribute accordingly
	 * based on the provided {@code ignoreFillPointer} value.
	 *
	 * @param ignoreFillPointer
	 * 		whether or not to ignore the fill-pointer value when constructing the resulting {@link String}
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String toJavaString(final boolean ignoreFillPointer);

	/**
	 * Returns a new empty StringStruct with no contents.
	 *
	 * @return a new empty StringStruct with no contents
	 */
	static StringStruct emptyString() {
		return EMPTY_STRING;
	}

	/**
	 * Returns a new StringStruct representation of the provided {@link String}.
	 *
	 * @param str
	 * 		the {@link String} to represent as a StringStruct
	 *
	 * @return a new StringStruct representation of the provided {@link String}
	 */
	static StringStruct toLispString(final String str) {
		final IntegerStruct size = IntegerStruct.toLispInteger(str.length());
		final StringBuilder contents = new StringBuilder(str);
		return new SimpleStringStructImpl(size, CommonLispSymbols.CHARACTER, contents);
	}

	/**
	 * Returns a new StringStruct representation of the provided size, element-type, and contents.
	 *
	 * @param size
	 * 		the string size
	 * @param elementType
	 * 		the string elementType
	 * @param contents
	 * 		the string contents
	 *
	 * @return a new StringStruct representation of the provided size, element-type, and contents
	 */
	static StringStruct toLispString(final IntegerStruct size, final SymbolStruct elementType,
	                                 final String contents) {
		return new SimpleStringStructImpl(size, elementType, new StringBuilder(contents));
	}

	/**
	 * Returns a new StringStruct representation of the provided size, element-type, contents, adjustable, and
	 * fillPointer.
	 *
	 * @param size
	 * 		the string size
	 * @param contents
	 * 		the string contents
	 * @param elementType
	 * 		the string elementType
	 * @param adjustable
	 * 		whether or not the string is adjustable
	 * @param fillPointer
	 * 		the string fillPointer
	 *
	 * @return a new StringStruct representation of the provided size, element-type, contents, adjustable, and fillPointer
	 */
	static StringStruct toLispString(final IntegerStruct size, final SymbolStruct elementType,
	                                 final String contents, final BooleanStruct adjustable,
	                                 final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexStringStructImpl(size, elementType, new StringBuilder(contents), realAdjustable, fillPointer);
	}

	/**
	 * Returns a new StringStruct representation of the provided size, element-type, displacedTo, displacedIndexOffset,
	 * adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the string size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param elementType
	 * 		the string elementType
	 * @param adjustable
	 * 		whether or not the string is adjustable
	 * @param fillPointer
	 * 		the string fillPointer
	 *
	 * @return a new StringStruct representation of the provided size, element-type, displacedTo, displacedIndexOffset,
	 * 		adjustable, and fillPointer
	 */
	static VectorStruct toLispString(final IntegerStruct size, final SymbolStruct elementType,
	                                 final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                 final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexStringStructImpl(
				size, elementType, displacedTo, displacedIndexOffset, realAdjustable, fillPointer
		);
	}

	/**
	 * Returns a new StringStruct representation of the provided {@link LispStruct}.
	 *
	 * @param lispStruct
	 * 		a {@link LispStruct} that can be represented as a string
	 *
	 * @return a new StringStruct representation of the provided {@link LispStruct}
	 *
	 * @throws TypeErrorException
	 * 		if the provided @link LispStruct} cannot be converted to a string
	 */
	static StringStruct toLispString(final LispStruct lispStruct) {
		if (lispStruct instanceof StringStruct) {
			return (StringStruct) lispStruct;
		} else if (lispStruct instanceof SymbolStruct) {
			final SymbolStruct symbolStruct = (SymbolStruct) lispStruct;
			final String name = symbolStruct.getName();
			return toLispString(name);
		} else if (lispStruct instanceof CharacterStruct) {
			final CharacterStruct characterStruct = (CharacterStruct) lispStruct;
			return toLispString(characterStruct.toJavaCharacter().toString());
		} else {
			throw new TypeErrorException("Type cannot be converted to String.");
		}
	}

	/**
	 * Returns a simple string of length size whose elements have been initialized to initial-element.
	 * <p>
	 * The element-type names the type of the elements of the string; a string is constructed of the most specialized
	 * type that can accommodate elements of the given type.
	 *
	 * @param size
	 * 		the size of the string to make
	 * @param initialElement
	 * 		a character to initialize the string
	 * @param elementType
	 * 		the element-type of the string
	 *
	 * @return a simple string of length size whose elements have been initialized to initial-element
	 */
	static StringStruct makeString(final IntegerStruct size, final CharacterStruct initialElement,
	                               final SymbolStruct elementType) {
		final LispStruct characterType = ArrayStruct.upgradedArrayElementType(elementType);

		final StringBuilder contents = Stream.generate(() -> initialElement)
		                                     .limit(size.toJavaInt())
		                                     .mapToInt(CharacterStruct::toUnicodeCodePoint)
		                                     .collect(StringBuilder::new,
		                                              StringBuilder::appendCodePoint,
		                                              StringBuilder::append);
		return new SimpleStringStructImpl(size, characterType, contents);
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	CharacterStruct elt(final IntegerStruct index);

	@Override
	CharacterStruct setfElt(final LispStruct newElement, final IntegerStruct index);

	@Override
	StringStruct reverse();

	@Override
	StringStruct nReverse();

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof StringStruct) {
			final StringStruct string = (StringStruct) object;

			final IntegerStruct thisLength = length();
			if (!thisLength.eql(string.length())) {
				return false;
			}
			for (int i = 0; i < thisLength.toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).equal(string.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof StringStruct) {
			final StringStruct string = (StringStruct) object;

			final IntegerStruct thisLength = length();
			if (!thisLength.eql(string.length())) {
				return false;
			}
			for (int i = 0; i < thisLength.toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).equalp(string.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		if (object instanceof BitVectorStruct) {
			return false;
		}
		if (object instanceof VectorStruct) {
			return object.equalp(this);
		}
		return false;
	}
}
