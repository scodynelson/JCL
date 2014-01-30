package jcl.reader.macrofunction;

public class ReadExtendedToken {

	private final String token;
	private final boolean hasEscapes;
	private final boolean hasPackageDelimiter;

	public ReadExtendedToken(final String token, final boolean hasEscapes, final boolean hasPackageDelimiter) {
		this.token = token;
		this.hasEscapes = hasEscapes;
		this.hasPackageDelimiter = hasPackageDelimiter;
	}

	public String getToken() {
		return token;
	}

	public boolean hasEscapes() {
		return hasEscapes;
	}

	public boolean hasPackageDelimiter() {
		return hasPackageDelimiter;
	}
}
