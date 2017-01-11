package jcl.lang.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.CharacterStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.type.BaseCharType;
import jcl.type.BaseStringType;
import jcl.type.CharacterType;
import jcl.type.SimpleBaseStringType;
import jcl.type.SimpleStringType;
import jcl.type.StringType;

/**
 * The {@link StringStructImpl} is the object representation of a Lisp 'string' type.
 */
public final class StringStructImpl extends VectorStructImpl<CharacterStruct> implements StringStruct {

	public StringStructImpl(final StringType stringType, final Integer size, final CharacterType elementType,
	                         final List<CharacterStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(stringType, size, elementType, contents, isAdjustable, fillPointer);
	}

	public static StringStruct valueOf(final Integer size, final CharacterType elementType, final CharacterStruct initialElement,
	                                   final boolean isAdjustable, final Integer fillPointer) {
		final List<CharacterStruct> initialContents = Stream.generate(() -> initialElement)
		                                                    .limit(size)
		                                                    .collect(Collectors.toList());
		final StringType stringType = getStringType(isAdjustable, fillPointer, elementType);
		return new StringStructImpl(stringType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static StringStruct valueOf(final Integer size, final CharacterType elementType, final List<CharacterStruct> initialContents,
	                                   final boolean isAdjustable, final Integer fillPointer) {
		final StringType stringType = getStringType(isAdjustable, fillPointer, elementType);
		return new StringStructImpl(stringType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static StringStruct valueOf(final Integer size, final CharacterType elementType, final CharacterStruct initialElement) {
		final List<CharacterStruct> initialContents = Stream.generate(() -> initialElement)
		                                                    .limit(size)
		                                                    .collect(Collectors.toList());
		final StringType stringType = getStringType(false, null, elementType);
		return new StringStructImpl(stringType, size, elementType, initialContents, false, null);
	}

	public static StringStruct valueOf(final Integer size, final CharacterType elementType, final List<CharacterStruct> initialContents) {
		final StringType stringType = getStringType(false, null, elementType);
		return new StringStructImpl(stringType, size, elementType, initialContents, false, null);
	}

	/*
		Old Builders
	 */

	public static StringStruct valueOf(final String stringValue) {
		return new StringStructImpl(SimpleStringType.INSTANCE, stringValue.length(), CharacterType.INSTANCE, getCharList(stringValue), false, null);
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
	private static StringType getStringType(final boolean isAdjustable, final Integer fillPointer, final CharacterType elementType) {
		if (isAdjustable || (fillPointer != null)) {
			return (elementType instanceof BaseCharType) ? BaseStringType.INSTANCE : StringType.INSTANCE;
		} else {
			return (elementType instanceof BaseCharType) ? SimpleBaseStringType.INSTANCE : SimpleStringType.INSTANCE;
		}
	}

	/**
	 * Gets a list of {@link CharacterStruct}s from the provided {@link String} value.
	 *
	 * @param stringValue
	 * 		the Java string to convert to a list of {@link CharacterStruct}s
	 *
	 * @return a list of {@link CharacterStruct}s from the provided {@link String} value
	 */
	private static List<CharacterStruct> getCharList(final String stringValue) {
		final List<CharacterStruct> charList = new ArrayList<>(stringValue.length());
		for (final char character : stringValue.toCharArray()) {
			final CharacterStruct characterStruct = CharacterStructImpl.valueOf(character);
			charList.add(characterStruct);
		}
		return charList;
	}

	@Override
	public String getAsJavaString() {
		final StringBuilder stringBuilder = new StringBuilder(contents.size());

		for (final CharacterStruct characterStruct : contents) {
			final int codePoint = characterStruct.getCodePoint();
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}

	@Override
	public Supplier<CharacterStruct> asCharacter() {
		return () -> {
			// TODO: Can improve this
			final String javaString = getAsJavaString();
			if (javaString.length() != 1) {
				throw new SimpleErrorException("String is not of length one: " + javaString);
			}
			return CharacterStructImpl.valueOf(javaString.charAt(0));
		};
	}

	@Override
	public Supplier<CharacterStruct> asNamedCharacter() {
		return () -> {
			final String javaString = getAsJavaString();
			return CharacterStructImpl.valueOf(UCharacter.getCharFromName(javaString));
		};
	}

	@Override
	public Supplier<PathnameStruct> asPathname() {
		return () -> {
			final String namestring = getAsJavaString();
			return PathnameStructImpl.valueOf(namestring);
		};
	}

	@Override
	public Supplier<SymbolStruct> asSymbol() {
		return () -> {
			final String namestring = getAsJavaString();
			return SymbolStructImpl.valueOf(namestring);
		};
	}

	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> {
			final String packageName = getAsJavaString();
			return PackageStruct.findPackage(packageName);
		};
	}

	@Override
	public Supplier<StringStruct> asString() {
		return () -> this;
	}

	@Override
	public Long length() {
		// TODO: Do this right later...

		final List<CharacterStruct> asJavaList = contents;
		final int size = asJavaList.size();
		return (long) size;
	}

	@Override
	public String toString() {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().booleanValue();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final List<Integer> codePointContents =
				getContents().stream()
				             .map(CharacterStruct::getCodePoint)
				             .collect(Collectors.toList());

		final int amountToPrint = (fillPointer == null) ? codePointContents.size() : fillPointer;

		for (int i = 0; i < amountToPrint; i++) {
			final int codePoint = codePointContents.get(i);

			final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);
			if ((codePoint == '"') || (syntaxType == SyntaxType.SINGLE_ESCAPE)) {
				stringBuilder.append('\\');
			}
			stringBuilder.appendCodePoint(codePoint);
		}

		if (printEscape) {
			stringBuilder.append('"');
		}

		return stringBuilder.toString();
	}
}
