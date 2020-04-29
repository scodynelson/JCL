package jcl.lang.internal;

import java.util.Iterator;
import java.util.NoSuchElementException;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;

/**
 * Iterator for {@link StringStruct} structures without displaced contents.
 */
final class StringIterator implements Iterator<LispStruct> {

	/**
	 * The contents of the {@link ComplexStringStructImpl} being iterated over.
	 */
	private final StringBuilder contents;

	/**
	 * The current index of the iteration.
	 */
	private int current;

	/**
	 * Constructor for building the iterator.
	 *
	 * @param contents
	 * 		the contents of the {@link ComplexStringStructImpl} to be iterated over
	 */
	StringIterator(final StringBuilder contents) {
		this.contents = contents;
	}

	@Override
	public boolean hasNext() {
		try {
			contents.charAt(current);
			return true;
		} catch (final StringIndexOutOfBoundsException ignored) {
			return false;
		}
	}

	@Override
	public LispStruct next() {
		final char character;
		try {
			character = contents.charAt(current);
		} catch (final StringIndexOutOfBoundsException ignored) {
			throw new NoSuchElementException("All elements consumed.");
		} finally {
			current++;
		}
		return CharacterStruct.toLispCharacter(character);
	}
}
