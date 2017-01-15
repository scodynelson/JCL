package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.StringStructImpl;
import jcl.lang.statics.CharacterConstants;
import jcl.type.BaseCharType;
import jcl.type.BaseStringType;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleBaseStringType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

/**
 * The {@link StringStruct} is the object representation of a Lisp 'string' type.
 */
public interface StringStruct extends VectorStruct {

	CharacterStruct char_(final IntegerStruct index);

	default CharacterStruct schar(final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleStringType.INSTANCE.equals(type)) {
			return char_(index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	CharacterStruct setfChar(final CharacterStruct newElement, final IntegerStruct index);

	default CharacterStruct setfSchar(final CharacterStruct newElement, final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleStringType.INSTANCE.equals(type)) {
			return setfChar(newElement, index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleStringType.INSTANCE + '.');
	}

	default StringStruct string() {
		return this;
	}

	class StringCaseContext {

		private IntegerStruct start;

		private IntegerStruct end;

		private StringCaseContext() {
		}

		public StringCaseContext start(final IntegerStruct start) {
			this.start = start;
			return this;
		}

		public StringCaseContext end(final IntegerStruct end) {
			this.end = end;
			return this;
		}

		public StringCaseContext build() {
			return this;
		}

		public static StringCaseContext builder() {
			return new StringCaseContext();
		}
	}

	StringStruct stringUpcase(final StringCaseContext context);

	StringStruct stringDowncase(final StringCaseContext context);

	StringStruct stringCapitalize(final StringCaseContext context);

	StringStruct nStringUpcase(final StringCaseContext context);

	StringStruct nStringDowncase(final StringCaseContext context);

	StringStruct nStringCapitalize(final StringCaseContext context);

	StringStruct stringTrim(final SequenceStruct characterBag);

	StringStruct stringLeftTrim(final SequenceStruct characterBag);

	StringStruct stringRightTrim(final SequenceStruct characterBag);

	class StringEqualityContext {

		private final StringStruct struct;

		private IntegerStruct start1;

		private IntegerStruct end1;

		private IntegerStruct start2;

		private IntegerStruct end2;

		private StringEqualityContext(final StringStruct struct) {
			this.struct = struct;
		}

		public StringEqualityContext start1(final IntegerStruct start1) {
			this.start1 = start1;
			return this;
		}

		public StringEqualityContext end1(final IntegerStruct end1) {
			this.end1 = end1;
			return this;
		}

		public StringEqualityContext start2(final IntegerStruct start2) {
			this.start2 = start2;
			return this;
		}

		public StringEqualityContext end2(final IntegerStruct end2) {
			this.end2 = end2;
			return this;
		}

		public StringEqualityContext build() {
			return this;
		}

		public static StringEqualityContext builder(final StringStruct struct) {
			return new StringEqualityContext(struct);
		}
	}

	BooleanStruct stringEqual(final StringEqualityContext context);

	IntegerStruct stringNotEqual(final StringEqualityContext context);

	IntegerStruct stringLessThan(final StringEqualityContext context);

	IntegerStruct stringGreaterThan(final StringEqualityContext context);

	IntegerStruct stringLessThanOrEqualTo(final StringEqualityContext context);

	IntegerStruct stringGreaterThanOrEqualTo(final StringEqualityContext context);

	BooleanStruct stringEqualIgnoreCase(final StringEqualityContext context);

	IntegerStruct stringNotEqualIgnoreCase(final StringEqualityContext context);

	IntegerStruct stringLessThanIgnoreCase(final StringEqualityContext context);

	IntegerStruct stringGreaterThanIgnoreCase(final StringEqualityContext context);

	IntegerStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context);

	IntegerStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context);

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String getAsJavaString();

	static StringStruct.Builder builder(final IntegerStruct size) {
		return new StringStruct.Builder(size);
	}

	final class Builder extends ArrayStruct.AbstractBuilder<StringStruct, CharacterType, CharacterStruct> {

		private final IntegerStruct size;
		private IntegerStruct fillPointer;

		private Builder(final IntegerStruct size) {
			super(CharacterType.INSTANCE, CharacterConstants.NULL_CHAR);
			this.size = size;
		}

		@Override
		public StringStruct.Builder elementType(final CharacterType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public StringStruct.Builder initialElement(final CharacterStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public StringStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public StringStruct.Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public StringStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		@Override
		public StringStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public StringStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public StringStruct build() {
			final int sizeInt = size.intValue();
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable.booleanValue();
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.intValue();

			if (displacedTo != null) {
				final LispType displacedToType = displacedTo.getType();
				if (displacedToType.isNotOfType(upgradedET)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				try {
					displacedTo.rowMajorAref(displacedIndexOffset);
				} catch (final ErrorException ignore) {
					throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
				}

				final StringType stringType = getStringType(adjustableBoolean, fillPointerInt, upgradedET);
				return new StringStructImpl(stringType,
				                            sizeInt,
				                            upgradedET,
				                            displacedTo,
				                            displacedIndexOffset.intValue(),
				                            adjustableBoolean,
				                            fillPointerInt);
			}
			final StringType stringType = getStringType(adjustableBoolean, fillPointerInt, upgradedET);

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (initialElementType.isNotOfType(upgradedET)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<CharacterStruct> validContents
						= ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
						                               upgradedET,
						                               initialContents);

				final StringBuilder contents = validContents.stream()
				                                            .mapToInt(CharacterStruct::getCodePoint)
				                                            .collect(StringBuilder::new,
				                                                     StringBuilder::appendCodePoint,
				                                                     StringBuilder::append);
				return new StringStructImpl(stringType,
				                            sizeInt,
				                            CharacterType.INSTANCE,
				                            contents,
				                            adjustableBoolean,
				                            fillPointerInt);
			}

			final LispType initialElementType = initialElement.getType();
			if (initialElementType.isNotOfType(upgradedET)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}

			final StringBuilder contents = Stream.generate(() -> initialElement)
			                                     .limit(sizeInt)
			                                     .mapToInt(CharacterStruct::getCodePoint)
			                                     .collect(StringBuilder::new,
			                                              StringBuilder::appendCodePoint,
			                                              StringBuilder::append);
			return new StringStructImpl(stringType,
			                            sizeInt,
			                            CharacterType.INSTANCE,
			                            contents,
			                            adjustableBoolean,
			                            fillPointerInt);
		}

		/**
		 * Gets the string type from the provided isAdjustable, fillPointer, and elementType values.
		 *
		 * @param isAdjustable
		 * 		whether or not the string is adjustable
		 * @param fillPointer
		 * 		the string fillPointer
		 * @param elementType
		 * 		the string elementType
		 *
		 * @return the matching string type for the provided isAdjustable, fillPointer, and elementType values
		 */
		private static StringType getStringType(final boolean isAdjustable,
		                                        final Integer fillPointer,
		                                        final LispType elementType) {
			if (isAdjustable || (fillPointer != null)) {
				return (elementType instanceof BaseCharType)
				       ? BaseStringType.INSTANCE
				       : StringType.INSTANCE;
			} else {
				return (elementType instanceof BaseCharType)
				       ? SimpleBaseStringType.INSTANCE
				       : SimpleStringType.INSTANCE;
			}
		}
	}
}
