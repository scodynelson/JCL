package jcl.reader.syntax.reader;

public class PeekType {

	public enum PeekTypeType {
		T,
		NIL,
		CHARACTER
	}

	private final PeekTypeType type;
	private final Integer codePoint;

	private PeekType(final PeekTypeType type, final Integer codePoint) {
		this.type = type;
		this.codePoint = codePoint;
	}

	public PeekTypeType getType() {
		return type;
	}

	public Integer getCodePoint() {
		return codePoint;
	}

	public static final PeekType NIL_PEEK_TYPE = new PeekType(PeekTypeType.NIL, null);
	public static final PeekType T_PEEK_TYPE = new PeekType(PeekTypeType.T, null);

	public static PeekType getCharacterPeekType(final int codePoint) {
		return new PeekType(PeekTypeType.CHARACTER, codePoint);
	}
}
