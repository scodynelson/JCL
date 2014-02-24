package jcl.structs.strings;

import jcl.structs.arrays.VectorStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.types.LispType;
import jcl.types.characters.Character;
import jcl.types.strings.String;

import java.util.ArrayList;
import java.util.List;

public class StringStruct extends VectorStruct<CharacterStruct> {

	public StringStruct(final java.lang.String stringValue) {
		this(stringValue.length(), getCharList(stringValue), Character.INSTANCE, false, null);
	}

	public StringStruct(final int size, final List<CharacterStruct> contents, final LispType elementType,
						final boolean isAdjustable, final Integer fillPointer) {
		super(String.INSTANCE, size, contents, elementType, isAdjustable, fillPointer);
	}

	private static List<CharacterStruct> getCharList(final java.lang.String stringValue) {
		final List<CharacterStruct> charList = new ArrayList<>(stringValue.length());
		for (final char character : stringValue.toCharArray()) {
			final CharacterStruct characterStruct = new CharacterStruct(character);
			charList.add(characterStruct);
		}
		return charList;
	}

	public java.lang.String getAsJavaString() {

		final List<CharacterStruct> contents = getContents();
		final StringBuilder stringBuilder = new StringBuilder(contents.size());

		for (final CharacterStruct characterStruct : contents) {
			final int codePoint = characterStruct.getCodePoint();
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}
}
