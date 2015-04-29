package jcl.compiler.real.sa.analyzer.body;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class BodyProcessingResult {

	private final List<LispStruct> declares;

	private final StringStruct docString;

	private final List<LispStruct> bodyForms;

	public BodyProcessingResult(final List<LispStruct> declares, final StringStruct docString, final List<LispStruct> bodyForms) {
		this.declares = declares;
		this.docString = docString;
		this.bodyForms = bodyForms;
	}

	public List<LispStruct> getDeclares() {
		return declares;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getBodyForms() {
		return bodyForms;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(declares)
		                            .append(docString)
		                            .append(bodyForms)
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
		final BodyProcessingResult rhs = (BodyProcessingResult) obj;
		return new EqualsBuilder().append(declares, rhs.declares)
		                          .append(docString, rhs.docString)
		                          .append(bodyForms, rhs.bodyForms)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(declares)
		                                                                .append(docString)
		                                                                .append(bodyForms)
		                                                                .toString();
	}
}
