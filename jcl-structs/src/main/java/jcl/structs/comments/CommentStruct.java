package jcl.structs.comments;

import jcl.structs.LispStruct;
import jcl.types.LispType;

public class CommentStruct implements LispStruct {

	private final String commentString;

	private CommentStruct(final String commentString) {
		this.commentString = commentString;
	}

	@Override
	public LispType getType() {
		return null;
	}

	@Override
	public String toString() {
		return "CommentStruct{" +
				"commentString='" + commentString + '\'' +
				'}';
	}

	// BUILDERS

	public static CommentStruct getStruct(final String commentString) {
		return new CommentStruct(commentString);
	}
}
