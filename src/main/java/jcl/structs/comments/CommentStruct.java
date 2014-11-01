package jcl.structs.comments;

import jcl.LispStruct;
import jcl.LispType;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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
	public String printStruct() {
		return toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	// BUILDERS

	public static CommentStruct getStruct(final String commentString) {
		return new CommentStruct(commentString);
	}
}
