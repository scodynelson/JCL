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

	class StringIntervalOpContext {

		public static class Builder {

			private IntegerStruct start;

			private IntegerStruct end;

			private Builder() {
			}

			public StringIntervalOpContext.Builder start(final IntegerStruct start) {
				this.start = start;
				return this;
			}

			public StringIntervalOpContext.Builder end(final IntegerStruct end) {
				this.end = end;
				return this;
			}

			public StringIntervalOpContext build() {
				return new StringIntervalOpContext(start, end);
			}
		}

		private final IntegerStruct start;
		private final IntegerStruct end;

		private StringIntervalOpContext(final IntegerStruct start, final IntegerStruct end) {
			this.start = start;
			this.end = end;
		}

		public IntegerStruct getStart() {
			return start;
		}

		public IntegerStruct getEnd() {
			return end;
		}

		public static StringIntervalOpContext.Builder builder() {
			return new StringIntervalOpContext.Builder();
		}
	}

	StringStruct stringUpcase(final StringIntervalOpContext context);

	StringStruct stringDowncase(final StringIntervalOpContext context);

	StringStruct stringCapitalize(final StringIntervalOpContext context);

	StringStruct nStringUpcase(final StringIntervalOpContext context);

	StringStruct nStringDowncase(final StringIntervalOpContext context);

	StringStruct nStringCapitalize(final StringIntervalOpContext context);

	StringStruct stringTrim(final SequenceStruct characterBag);

	StringStruct stringLeftTrim(final SequenceStruct characterBag);

	StringStruct stringRightTrim(final SequenceStruct characterBag);

	class StringEqualityContext {

		public static class Builder {

			private final StringStruct struct;
			private StringIntervalOpContext.Builder intervalOpContext1;
			private StringIntervalOpContext.Builder intervalOpContext2;

			private Builder(final StringStruct struct) {
				this.struct = struct;
			}

			public StringEqualityContext.Builder start1(final IntegerStruct start1) {
				if (intervalOpContext1 == null) {
					intervalOpContext1 = StringIntervalOpContext.builder();
				}
				intervalOpContext1.start(start1);
				return this;
			}

			public StringEqualityContext.Builder end1(final IntegerStruct end1) {
				if (intervalOpContext1 == null) {
					intervalOpContext1 = StringIntervalOpContext.builder();
				}
				intervalOpContext1.end(end1);
				return this;
			}

			public StringEqualityContext.Builder start2(final IntegerStruct start2) {
				if (intervalOpContext2 == null) {
					intervalOpContext2 = StringIntervalOpContext.builder();
				}
				intervalOpContext2.start(start2);
				return this;
			}

			public StringEqualityContext.Builder end2(final IntegerStruct end2) {
				if (intervalOpContext2 == null) {
					intervalOpContext2 = StringIntervalOpContext.builder();
				}
				intervalOpContext2.end(end2);
				return this;
			}

			public StringEqualityContext build() {
				return new StringEqualityContext(struct,
				                                 intervalOpContext1.build(),
				                                 intervalOpContext2.build());
			}
		}

		private final StringStruct struct;
		private final StringIntervalOpContext context1;
		private final StringIntervalOpContext context2;

		private StringEqualityContext(final StringStruct struct,
		                              final StringIntervalOpContext context1,
		                              final StringIntervalOpContext context2) {
			this.struct = struct;
			this.context1 = context1;
			this.context2 = context2;
		}

		public StringStruct getStruct() {
			return struct;
		}

		public StringIntervalOpContext getContext1() {
			return context1;
		}

		public StringIntervalOpContext getContext2() {
			return context2;
		}

		public static StringEqualityContext.Builder builder(final StringStruct struct) {
			return new StringEqualityContext.Builder(struct);
		}
	}

	BooleanStruct stringEqual(final StringEqualityContext context);

	LispStruct stringNotEqual(final StringEqualityContext context);

	LispStruct stringLessThan(final StringEqualityContext context);

	LispStruct stringGreaterThan(final StringEqualityContext context);

	LispStruct stringLessThanOrEqualTo(final StringEqualityContext context);

	LispStruct stringGreaterThanOrEqualTo(final StringEqualityContext context);

	BooleanStruct stringEqualIgnoreCase(final StringEqualityContext context);

	LispStruct stringNotEqualIgnoreCase(final StringEqualityContext context);

	LispStruct stringLessThanIgnoreCase(final StringEqualityContext context);

	LispStruct stringGreaterThanIgnoreCase(final StringEqualityContext context);

	LispStruct stringLessThanOrEqualToIgnoreCase(final StringEqualityContext context);

	LispStruct stringGreaterThanOrEqualToIgnoreCase(final StringEqualityContext context);

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	default String toJavaString() {
		// TODO: right now this ignores fill-pointer by default. Should it or should it not??
		return toJavaString(false);
	}

	String toJavaString(final boolean fillPointerRestriction);

	static StringStruct toLispString(final String str) {
		return new StringStructImpl(SimpleStringType.INSTANCE,
		                            str.length(),
		                            CharacterType.INSTANCE,
		                            new StringBuilder(str),
		                            false,
		                            null);
	}

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
