package jcl.structs.streams;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Special object denoting a peek type in how the 'peek-char' should operate. There are 3 categories of PeekTypes:
 * <tab>
 * 1. {@link PeekType#NIL_PEEK_TYPE} - this denotes the NIL based PeekType
 * 2. {@link PeekType#T_PEEK_TYPE} - this denotes the T based PeekType
 * 3. Character based PeekTypes - these are created by calling the static method {@link PeekType#getCharacterPeekType}
 * to create a specific character-based PeekType based off of a provided {@link Integer} codePoint value
 * </tab>
 */
public final class PeekType {

	public static final PeekType NIL_PEEK_TYPE = new PeekType(PeekTypeType.NIL, null);
	public static final PeekType T_PEEK_TYPE = new PeekType(PeekTypeType.T, null);

	private final PeekTypeType type;
	private final Integer codePoint;

	/**
	 * Private constructor to create a PeekType with the provided {@link PeekTypeType} and {@link Integer} codePoint
	 * value.
	 *
	 * @param type
	 * 		the {@link PeekTypeType} value for the PeekType
	 * @param codePoint
	 * 		the {@link Integer} codePoint value for the PeekType
	 */
	private PeekType(final PeekTypeType type, final Integer codePoint) {
		this.type = type;
		this.codePoint = codePoint;
	}

	/**
	 * Gets a new {@link PeekTypeType#CHARACTER} based PeekType instance.
	 *
	 * @param codePoint
	 * 		the codePoint value of the character for the peek
	 *
	 * @return a new {@link PeekTypeType#CHARACTER} with the provided codePoint value
	 */
	public static PeekType getCharacterPeekType(final int codePoint) {
		return new PeekType(PeekTypeType.CHARACTER, codePoint);
	}

	/**
	 * Getter for the {@link #type} value.
	 *
	 * @return the {@link #type} value
	 */
	PeekTypeType getType() {
		return type;
	}

	/**
	 * Getter for the {@link #codePoint} value.
	 *
	 * @return the {@link #codePoint} value
	 */
	Integer getCodePoint() {
		return codePoint;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * Package private enumeration to encapsulate the specific 'peek' type.
	 */
	enum PeekTypeType {
		T,
		NIL,
		CHARACTER
	}
}