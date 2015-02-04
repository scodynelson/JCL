package jcl.compiler.real.environment;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.UUID;

public class LoadTimeValue {

	private final UUID uniqueLTVId;
	private final Element value;

	public LoadTimeValue(final UUID uniqueLTVId, final Element value) {
		this.uniqueLTVId = uniqueLTVId;
		this.value = value;
	}

	public UUID getUniqueLTVId() {
		return uniqueLTVId;
	}

	public Element getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
