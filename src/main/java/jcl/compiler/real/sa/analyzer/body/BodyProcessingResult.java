package jcl.compiler.real.sa.analyzer.body;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class BodyProcessingResult {

	private final DeclareStruct declareElement;

	private final StringStruct docString;

	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final DeclareStruct declareElement, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declareElement = declareElement;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public DeclareStruct getDeclareElement() {
		return declareElement;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
