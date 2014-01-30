package jcl.structs.strings;

import jcl.structs.arrays.VectorStruct;
import jcl.structs.characters.CharacterStruct;
import jcl.types.LispType;
import jcl.types.characters.Character;

import java.util.ArrayList;
import java.util.List;

public class StringStruct extends VectorStruct<CharacterStruct> {

	protected StringStruct(final int size, final List<CharacterStruct> contents, final LispType elementType,
						   final boolean isAdjustable, final Integer fillPointer) {
		super(size, contents, elementType, isAdjustable, fillPointer);
	}

	public String getAsJavaString() {

		final StringBuilder stringBuilder = new StringBuilder(contents.size());

		for (final CharacterStruct characterStruct : contents) {
			final int codePoint = characterStruct.getCodePoint();
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}

	// BUILDERS

	public static StringStruct getStruct(final String stringValue) {

		final List<CharacterStruct> charList = new ArrayList<>(stringValue.length());
		for (final char character : stringValue.toCharArray()) {
			final CharacterStruct characterStruct = CharacterStruct.getStruct(character);
			charList.add(characterStruct);
		}
		return new StringStruct(charList.size(), charList, Character.INSTANCE, false, null);
	}
}
