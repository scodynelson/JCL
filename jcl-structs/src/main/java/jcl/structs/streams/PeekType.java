package jcl.structs.streams;

public class PeekType {

	public enum PeekTypeType {
		T,
		NIL,
		CHARACTER
	}

	private final PeekTypeType type;
	private final Integer codePoint;

	public PeekType(final PeekTypeType type, final Integer codePoint) {
		this.type = type;
		this.codePoint = codePoint;
	}

	public PeekTypeType getType() {
		return type;
	}

	public Integer getCodePoint() {
		return codePoint;
	}
}
