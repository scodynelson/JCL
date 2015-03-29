package jcl.compiler.real.sa.analyzer.body;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.analyzer.declare.DeclareExpander;
import jcl.compiler.real.struct.specialoperator.declare.DeclareStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BodyWithDeclaresAnalyzer implements Serializable {

	private static final long serialVersionUID = -4533785417061599823L;

	@Autowired
	private DeclareExpander declareExpander;

	public BodyProcessingResult analyze(final List<LispStruct> input, final Environment environment) {

		DeclareStruct declareElement = new DeclareStruct();
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = input.iterator();

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			final List<LispStruct> allDeclarations = new ArrayList<>();
			allDeclarations.add(SpecialOperatorStruct.DECLARE);

			while (isDeclaration(next)) {
				final ListStruct declareStatement = (ListStruct) next;
				final List<LispStruct> declarations = declareStatement.getRest().getAsJavaList();

				allDeclarations.addAll(declarations);

				if (iterator.hasNext()) {
					next = iterator.next();
				} else {
					next = null;
					break;
				}
			}

			final ListStruct fullDeclaration = ListStruct.buildProperList(allDeclarations);
			declareElement = declareExpander.expand(fullDeclaration, environment);

			if (next != null) {
				bodyForms.add(next);
			}
			while (iterator.hasNext()) {
				next = iterator.next();
				bodyForms.add(next);
			}
		}

		return new BodyProcessingResult(declareElement, null, bodyForms);
	}

	private boolean isDeclaration(final LispStruct next) {
		return (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperatorStruct.DECLARE);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(declareExpander)
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
		final BodyWithDeclaresAnalyzer rhs = (BodyWithDeclaresAnalyzer) obj;
		return new EqualsBuilder().append(declareExpander, rhs.declareExpander)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(declareExpander)
		                                                                .toString();
	}
}
