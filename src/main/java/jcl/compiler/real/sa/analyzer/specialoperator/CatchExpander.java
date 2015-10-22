package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	private static final long serialVersionUID = -4421664278117234704L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the catch macro function and adds it to the special operator 'catch'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.CATCH.setMacroFunctionExpander(this);
	}

	@Override
	public CatchStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct catchTag = formRest.getFirst();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new CatchStruct(catchTagAnalyzed, new PrognStruct(analyzedForms));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(formAnalyzer)
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
		final CatchExpander rhs = (CatchExpander) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(formAnalyzer, rhs.formAnalyzer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(formAnalyzer)
		                                                                .toString();
	}
}
