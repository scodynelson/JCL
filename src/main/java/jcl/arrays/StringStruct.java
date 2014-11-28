package jcl.arrays;

import jcl.characters.CharacterStruct;
import jcl.printer.PrinterVariables;
import jcl.reader.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.reader.SyntaxType;
import jcl.types.BaseChar;
import jcl.types.BaseString;
import jcl.types.Character;
import jcl.types.SimpleBaseString;
import jcl.types.SimpleString;
import jcl.types.String;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

/**
 * The {@link StringStruct} is the object representation of a Lisp 'string' type.
 */
public class StringStruct extends VectorStruct<CharacterStruct> {

	/**
	 * Public constructor.
	 *
	 * @param stringValue
	 * 		a Java string used for the string contents
	 */
	public StringStruct(final java.lang.String stringValue) {
		this(stringValue.length(), getCharList(stringValue), Character.INSTANCE, false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size
	 * 		the string size
	 * @param contents
	 * 		the string contents
	 * @param elementType
	 * 		the string elementType
	 * @param isAdjustable
	 * 		whether or not the string is adjustable
	 * @param fillPointer
	 * 		the string fillPointer
	 */
	public StringStruct(final int size, final List<CharacterStruct> contents, final Character elementType,
	                    final boolean isAdjustable, final Integer fillPointer) {
		super(getStringType(isAdjustable, fillPointer, elementType), size, contents, elementType, isAdjustable, fillPointer);
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
	private static String getStringType(final boolean isAdjustable, final Integer fillPointer, final Character elementType) {
		if (isAdjustable || (fillPointer != null)) {
			return (elementType instanceof BaseChar) ? BaseString.INSTANCE : String.INSTANCE;
		} else {
			return (elementType instanceof BaseChar) ? SimpleBaseString.INSTANCE : SimpleString.INSTANCE;
		}
	}

	/**
	 * Gets a list of {@link CharacterStruct}s from the provided {@link java.lang.String} value.
	 *
	 * @param stringValue
	 * 		the Java string to convert to a list of {@link CharacterStruct}s
	 *
	 * @return a list of {@link CharacterStruct}s from the provided {@link java.lang.String} value
	 */
	private static List<CharacterStruct> getCharList(final java.lang.String stringValue) {
		final List<CharacterStruct> charList = new ArrayList<>(stringValue.length());
		for (final char character : stringValue.toCharArray()) {
			final CharacterStruct characterStruct = new CharacterStruct(character);
			charList.add(characterStruct);
		}
		return charList;
	}

	/**
	 * Returns the {@link java.lang.String} representation of the StringStruct.
	 *
	 * @return a {@link java.lang.String} representation of the StringStruct
	 */
	public java.lang.String getAsJavaString() {
		final StringBuilder stringBuilder = new StringBuilder(contents.size());

		for (final CharacterStruct characterStruct : contents) {
			final int codePoint = characterStruct.getCodePoint();
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}

	@Override
	public java.lang.String printStruct() {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getValue().booleanValue();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append('"');
		}

		final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

		for (int i = 0; i < amountToPrint; i++) {
			final CharacterStruct characterStruct = contents.get(i);
			final int codePoint = characterStruct.getCodePoint();

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

	@Override
	public java.lang.String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
