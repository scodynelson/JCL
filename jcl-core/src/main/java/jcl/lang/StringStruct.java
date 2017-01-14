package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.StringStructImpl;
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

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
	String getAsJavaString();

	static StringStruct.Builder builder(final IntegerStruct size) {
		return new StringStruct.Builder(size);
	}

	final class Builder extends VectorStruct.Builder {

		private Builder(final IntegerStruct size) {
			super(size);
		}

		@Override
		public StringStruct.Builder elementType(final LispType elementType) { // TODO ??
			super.elementType(elementType);
			return this;
		}

		@Override
		public StringStruct.Builder initialElement(final LispStruct initialElement) { // TODO ??
			super.initialElement(initialElement);
			return this;
		}

		@Override
		public StringStruct.Builder initialContents(final SequenceStruct initialContents) {
			super.initialContents(initialContents);
			return this;
		}

		@Override
		public StringStruct.Builder adjustable(final BooleanStruct adjustable) {
			super.adjustable(adjustable);
			return this;
		}

		@Override
		public StringStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			super.fillPointer(fillPointer);
			return this;
		}

		@Override
		public StringStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			super.displacedTo(displacedTo);
			return this;
		}

		@Override
		public StringStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			super.displacedIndexOffset(displacedIndexOffset);
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
				final StringType stringType = (elementType instanceof BaseCharType)
				                              ? BaseStringType.INSTANCE
				                              : StringType.INSTANCE;

				// TODO:
//				return new StringStructImpl(stringType,
//				                            sizeInt,
//				                            upgradedET,
//				                            displacedTo,
//				                            displacedIndexOffset.intValue(),
//				                            adjustableBoolean,
//				                            fillPointerInt);
				return null;
			}

			final StringType stringType = getStringType(adjustableBoolean, fillPointerInt, (CharacterType) upgradedET);

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
				return new StringStructImpl(stringType,
				                            sizeInt,
				                            CharacterType.INSTANCE,
				                            validContents,
				                            adjustableBoolean,
				                            fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (initialElementType.isNotOfType(upgradedET)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<CharacterStruct> contents = Stream.generate(() -> (CharacterStruct) initialElement) // TODO
				                                             .limit(sizeInt)
				                                             .collect(Collectors.toList());
				return new StringStructImpl(stringType,
				                            sizeInt,
				                            CharacterType.INSTANCE,
				                            contents,
				                            adjustableBoolean,
				                            fillPointerInt);
			}
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
		private static StringType getStringType(final boolean isAdjustable, final Integer fillPointer,
		                                        final CharacterType elementType) {
			if (isAdjustable || (fillPointer != null)) {
				return (elementType instanceof BaseCharType) ? BaseStringType.INSTANCE : StringType.INSTANCE;
			} else {
				return (elementType instanceof BaseCharType)
				       ? SimpleBaseStringType.INSTANCE
				       : SimpleStringType.INSTANCE;
			}
		}
	}
}
