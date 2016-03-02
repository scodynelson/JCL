package jcl.arrays;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.packages.PackageStruct;
import jcl.pathnames.PathnameStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.BaseCharType;
import jcl.types.BaseStringType;
import jcl.types.CharacterType;
import jcl.types.SimpleBaseStringType;
import jcl.types.SimpleStringType;
import jcl.types.StringType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

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
	public StringStruct(final String stringValue) {
		this(stringValue.length(), getCharList(stringValue), CharacterType.INSTANCE, false, null);
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
	public StringStruct(final int size, final List<CharacterStruct> contents, final CharacterType elementType,
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
			final CharacterStruct characterStruct = CharacterStruct.valueOf(character);
			charList.add(characterStruct);
		}
		return charList;
	}

	/**
	 * Returns the {@link String} representation of the StringStruct.
	 *
	 * @return a {@link String} representation of the StringStruct
	 */
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
			return CharacterStruct.valueOf(javaString.charAt(0));
		};
	}

	@Override
	public Supplier<CharacterStruct> asNamedCharacter() {
		return () -> {
			final String javaString = getAsJavaString();
			return CharacterStruct.nameChar(javaString);
		};
	}

	@Override
	public Supplier<PathnameStruct> asPathname() {
		return () -> {
			final String namestring = getAsJavaString();
			return new PathnameStruct(namestring);
		};
	}

	@Override
	public Supplier<SymbolStruct> asSymbol() {
		return () -> {
			final String namestring = getAsJavaString();
			return new SymbolStruct(namestring);
		};
	}

	/**
	 * {@inheritDoc}
	 * Returns the PackageStruct with the {@link PackageStruct#name} that matches the StringStruct instance via
	 * {@link PackageStruct#findPackage(String)}.
	 *
	 * @return the PackageStruct with the {@link PackageStruct#name} that matches the instance
	 */
	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> {
			final String packageName = getAsJavaString();
			return PackageStruct.findPackage(packageName);
		};
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .isEquals();
	}
}
