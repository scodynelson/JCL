package jcl.lang.factory;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.array.ArrayStruct;
import jcl.lang.array.BitVectorStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.array.VectorStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.number.IntegerStruct;

public final class LispStructFactory {

	private LispStructFactory() {
	}

	/*
	 * Array
	 */

	public static <T extends LispStruct> ArrayStruct<T> toArray(final List<Integer> dimensions, final List<T> contents) {
		return ArrayStruct.valueOf(dimensions, contents);
	}

	/*
	 * BitVector
	 */

	public static BitVectorStruct toBitVector(final String bitString) {
		return BitVectorStruct.valueOf(bitString);
	}

	public static BitVectorStruct toBitVector(final List<IntegerStruct> contents) {
		return BitVectorStruct.valueOfCont(contents);
	}

	/*
	 * Character
	 */

	/**
	 * Returns a CharacterStruct object with the provided {@code character} value.
	 *
	 * @param character
	 * 		the character value used to derive the {@link CharacterStruct#codePoint} of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code character} value
	 */
	public static CharacterStruct toCharacter(final Character character) {
		return CharacterStruct.valueOf(character);
	}

	/**
	 * Returns a CharacterStruct object with the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the {@link CharacterStruct#codePoint} value of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code codePoint} value
	 */
	public static CharacterStruct toCharacter(final Integer codePoint) {
		return CharacterStruct.valueOf(codePoint);
	}

	/*
	 * String
	 */

	public static StringStruct toString(final String stringValue) {
		return StringStruct.valueOf(stringValue);
	}

	/*
	 * Vector
	 */

	public static <T extends LispStruct> VectorStruct<T> toVector(final List<T> contents) {
		return VectorStruct.valueOf(contents);
	}
}
